defmodule Exhub.MCP.Tools.WebTerm do
  @moduledoc """
  Drive a browser-hosted web terminal (JumpServer/Luna, Guacamole, any
  xterm.js canvas) through the `chrome` upstream MCP server.

  ## Why this tool exists

  The `browser` upstream tool takes a newline-separated command DSL where text
  is delimited by double quotes (`type "<text>"`). Any command containing a
  `"` — awk/sed/python string literals, JSON, `grep "x y"` — silently breaks
  the request (`type takes at most 1 argument`, `type has no -h flag`), and
  long payloads have to be typed character by character.

  `webterm` sidesteps the DSL entirely: text is base64-encoded and injected
  with `document.execCommand('insertText')`, so there is **no quoting rule at
  all** and a whole command lands in one round trip.

  ## One call per terminal transaction

  `action: "run"` clears the screen, runs `command; printf 'WTDONE %s\\n' $?`
  and polls screenshots until a line that is exactly `WTDONE <exit_code>`
  appears, then returns `{exit_code, output}`. That replaces the usual
  "type -> sleep -> screenshot -> OCR -> parse" chain with one call.

  The marker is deliberately echo-proof: the command echo contains
  `WTDONE %s` (literal `%s`), while only the real output line is
  `WTDONE 0` — so a line-anchored `^WTDONE <digits>$` cannot accidentally
  match the echoed command. (An earlier design used a random nonce; OCR
  mangled it often enough to cause spurious timeouts.)

  ## Reading the screen

  A canvas terminal exposes no DOM text (no `.xterm-rows`, no accessibility
  tree), so a screenshot + vision OCR is the only readback channel; this tool
  keeps that cost inside one call by using `look` in `mode: "text"`.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Hub.ClientManager
  alias Exhub.MCP.Tools.Look

  use Anubis.Server.Component, type: :tool

  @chrome_server "chrome"
  @chrome_tool "browser"
  @shot_path "/tmp/webterm_shot.jpg"
  @default_max_lines 40
  @default_timeout_s 60
  @max_timeout_s 600
  @chunk_size 3_000
  @marker "WTDONE"

  def name, do: "webterm"

  @impl true
  def description do
    """
    Drive a browser web terminal (JumpServer/Luna xterm.js, canvas-rendered TTY)
    via the Chrome extension — quote-safe, one call per command.

    Use this instead of the raw `browser` tool whenever you need to send shell
    commands to a web terminal: `browser`'s `type "<text>"` DSL breaks on any
    command containing a double quote and is very slow for long payloads.

    **Actions:**
    - `tabs` — list open browser tabs (id + title + url).
    - `attach` — select a tab by `tab` (id or title substring) and focus its
      xterm textarea. Do this once per session.
    - `paste` — inject `text` into the focused terminal without pressing
      Enter (unless `enter: true`). Base64 transport: no quoting rules.
    - `run` — clear the screen, paste `command`, press Enter, poll until the
      `WTDONE <exit_code>` marker appears, and return `exit_code` + output.
      This is the main action.
    - `keys` — press a key chord, e.g. `Control+c`, `Control+l` (`keys`).
    - `read` — transcribe the current screen without running anything.

    **Notes:**
    - Canvas terminals have no readable DOM, so `run` reads the screen with the
      `look` vision model (`mode: "text"`). OCR output can contain small
      glitches; verify anything security-critical with a second read.
    - `max_lines` (default 40) bounds how much is transcribed per poll.
    - The echoed command line is stripped on a best-effort basis; if a command
      wrapped across lines, a fragment of the echo may remain in `output`.
    """
  end

  schema do
    field(:action, {:required, :string},
      description: "\"tabs\" | \"attach\" | \"paste\" | \"run\" | \"keys\" | \"read\""
    )

    field(:command, :string,
      description:
        "action \"run\": the shell command to execute (may contain quotes, pipes, anything)"
    )

    field(:text, :string, description: "action \"paste\": raw text to inject into the terminal")

    field(:tab, :string,
      description:
        "action \"attach\": tab id (e.g. 931804463) or a substring of the tab title (e.g. \"攻击分析主机\")"
    )

    field(:keys, :string,
      description: "action \"keys\": key chord, e.g. \"Control+c\" or \"Enter\""
    )

    field(:enter, :boolean,
      description: "action \"paste\": also press Enter after pasting (default false)"
    )

    field(:timeout_s, :integer,
      description:
        "action \"run\": seconds to wait for the command to finish (default 60, max 600)"
    )

    field(:max_lines, :integer,
      description: "how many terminal lines to transcribe per read (default 40)"
    )

    field(:chunk_size, :integer,
      description: "action \"paste\": characters per insertText call (default 3000)"
    )
  end

  @impl true
  def execute(params, frame) do
    case dispatch(params) do
      {:ok, text} -> {:reply, Response.tool() |> Response.text(text), frame}
      {:error, reason} -> {:reply, Response.tool() |> Response.error(to_string(reason)), frame}
    end
  end

  # --- Actions ---

  defp dispatch(%{action: "tabs"}), do: browser_text("tab list")

  defp dispatch(%{action: "attach"} = params) do
    attach(Map.get(params, :tab) || "")
  end

  defp dispatch(%{action: "paste"} = params) do
    text = Map.get(params, :text) || ""
    enter? = Map.get(params, :enter, false) == true
    chunk = Map.get(params, :chunk_size) || @chunk_size

    paste(text, enter?, chunk)
  end

  defp dispatch(%{action: "run"} = params) do
    case Map.get(params, :command) do
      command when is_binary(command) and command != "" ->
        max_lines = Map.get(params, :max_lines) || @default_max_lines
        timeout_s = clamp_timeout(Map.get(params, :timeout_s) || @default_timeout_s)
        run(command, max_lines, timeout_s)

      _ ->
        {:error, "action \"run\" requires `command`"}
    end
  end

  defp dispatch(%{action: "keys"} = params) do
    case Map.get(params, :keys) do
      keys when is_binary(keys) and keys != "" -> browser_text("press " <> keys)
      _ -> {:error, "action \"keys\" requires `keys`"}
    end
  end

  defp dispatch(%{action: "read"} = params) do
    screen_text(Map.get(params, :max_lines) || @default_max_lines)
  end

  defp dispatch(%{action: action}), do: {:error, "unknown action: #{inspect(action)}"}
  defp dispatch(_), do: {:error, "`action` is required"}

  # --- Terminal primitives ---

  @doc false
  def insert_js(b64) do
    "(function(){var t;try{t=atob('" <>
      b64 <>
      "');}catch(e){return 'b64-error';}" <>
      "var e=document.querySelector('.xterm-helper-textarea')||document.querySelector('textarea')||document.activeElement;" <>
      "if(!e){return 'no-input';}e.focus();var ok=false;" <>
      "try{ok=document.execCommand('insertText',false,t);}catch(err){ok=false;}" <>
      "if(!ok&&e.setRangeText){e.setRangeText(t,e.selectionStart||0,e.selectionEnd||0,'end');" <>
      "e.dispatchEvent(new Event('input',{bubbles:true}));ok=true;}" <>
      "return (ok?'ok':'fail')+':'+t.length;})()"
  end

  defp paste("", _enter?, _chunk), do: {:error, "nothing to paste"}

  defp paste(text, enter?, chunk) when is_binary(text) do
    result =
      text
      |> chunk(chunk)
      |> Enum.reduce_while({:ok, 0}, fn part, {:ok, sent} ->
        cmd = "eval \"" <> insert_js(Base.encode64(part)) <> "\""

        case browser_text(cmd) do
          {:ok, reply} ->
            # The chrome DSL echoes the command before printing the JSON result,
            # so only the last line is the actual return value.
            case eval_result(reply) do
              "no-input" ->
                {:halt,
                 {:error,
                  "no terminal input element on the attached tab — run action \"attach\" first"}}

              "b64-error" ->
                {:halt, {:error, "could not decode the base64 payload"}}

              "fail:" <> _ ->
                {:halt, {:error, "insertText was refused by the page"}}

              _ ->
                {:cont, {:ok, sent + String.length(part)}}
            end

          {:error, reason} ->
            {:halt, {:error, reason}}
        end
      end)

    with {:ok, sent} <- result do
      if enter? do
        case browser_text("press Enter") do
          {:ok, _} -> {:ok, "pasted #{sent} chars and pressed Enter"}
          {:error, reason} -> {:error, reason}
        end
      else
        {:ok, "pasted #{sent} chars"}
      end
    end
  end

  @doc false
  def chunk(text, size) when is_integer(size) and size > 0 do
    text
    |> String.codepoints()
    |> Enum.chunk_every(size)
    |> Enum.map(&Enum.join/1)
  end

  def chunk(text, _size), do: [text]

  @doc false
  def find_tab(listing, needle) do
    needle = String.trim(needle)

    listing
    |> String.split("\n")
    |> Enum.find_value(fn line ->
      if needle != "" and String.contains?(line, needle) do
        case Regex.run(~r/^\s*\*?\s*(\d{5,})\b/, line) do
          [_, id] -> id
          _ -> nil
        end
      end
    end)
  end

  defp attach(tab) do
    with {:ok, listing} <- browser_text("tab list"),
         id when is_binary(id) <-
           find_tab(listing, tab) || {:error, "no tab matching #{inspect(tab)}"},
         {:ok, _} <- browser_text("tab select #{id}") do
      focus =
        "eval \"() => { const e = document.querySelector('.xterm-helper-textarea'); if (!e) return 'no-xterm'; e.focus(); return 'ok'; }\""

      case browser_text(focus) do
        {:ok, reply} -> {:ok, "attached to tab #{id} (focus: #{eval_result(reply)})"}
        {:error, reason} -> {:error, reason}
      end
    else
      {:error, reason} -> {:error, reason}
      _ -> {:error, "no tab matching #{inspect(tab)}"}
    end
  end

  # --- Command execution with a line-anchored marker ---

  @doc """
  Wraps a command so its exit status lands on its own line.

  The literal `%s` stays in the echoed command, while the produced line is
  `WTDONE <rc>` — which is what makes `extract_result/1` echo-proof.
  """
  @spec wrap_command(String.t()) :: String.t()
  def wrap_command(command) do
    String.trim_trailing(command) <> "; printf '\\n" <> @marker <> " %s\\n' $?"
  end

  @doc """
  Looks for a line that is exactly `WTDONE <exit_code>`.

  Returns `{:ok, exit_code, text_before_marker}` or `:pending`.
  """
  @spec extract_result(String.t()) :: {:ok, integer(), String.t()} | :pending
  def extract_result(screen) when is_binary(screen) do
    case Regex.run(~r/^[ \t]*WTDONE[ \t]+(-?\d+)[ \t]*$/m, screen) do
      [marker, rc] ->
        {pos, _} = :binary.match(screen, marker)
        {:ok, String.to_integer(rc), clean_output(binary_part(screen, 0, pos))}

      _ ->
        :pending
    end
  end

  @doc """
  Best-effort removal of the echoed command from a transcribed screen.
  """
  @spec clean_output(String.t(), String.t() | nil) :: String.t()
  def clean_output(text, command \\ nil) do
    lines = String.split(text, "\n")

    lines =
      cond do
        is_binary(command) and String.length(String.trim(command)) >= 8 ->
          probe = command |> String.trim() |> String.slice(0, 20)

          case Enum.find_index(lines, &String.contains?(&1, probe)) do
            nil -> lines
            idx -> Enum.drop(lines, idx + 1)
          end

        true ->
          lines
      end

    lines
    |> Enum.drop_while(&(String.trim(&1) == ""))
    |> Enum.reverse()
    |> Enum.drop_while(&(String.trim(&1) == ""))
    |> Enum.reverse()
    |> Enum.join("\n")
  end

  defp run(command, max_lines, timeout_s) do
    # Start from a clean screen so the marker appears exactly once.
    _ = browser_text("press Control+l")
    Process.sleep(200)

    case paste(wrap_command(command), true, @chunk_size) do
      {:ok, _} ->
        deadline = System.monotonic_time(:millisecond) + timeout_s * 1000
        poll(command, deadline, max_lines, timeout_s, 0, nil)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp poll(command, deadline, max_lines, timeout_s, attempts, last_screen) do
    Process.sleep(if attempts == 0, do: 700, else: 1_200)

    case screen_text(max_lines) do
      {:ok, screen} ->
        case extract_result(screen) do
          {:ok, rc, output} ->
            {:ok, format_run(rc, finalize_output(screen, command, output), attempts + 1)}

          :pending ->
            if System.monotonic_time(:millisecond) >= deadline do
              {:error,
               "timed out after #{timeout_s}s waiting for the command to finish.\n" <>
                 "--- last screen ---\n" <> String.trim(screen)}
            else
              poll(command, deadline, max_lines, timeout_s, attempts + 1, screen)
            end
        end

      {:error, reason} ->
        {:error,
         "screen read failed after #{attempts + 1} poll(s): #{reason}" <>
           case last_screen do
             nil -> ""
             s -> "\n--- last screen ---\n" <> String.trim(s)
           end}
    end
  end

  defp finalize_output(_screen, command, output), do: clean_output(output, command)

  @doc false
  def format_run(rc, output, attempts) do
    "exit_code: #{rc}\npolls: #{attempts}\n--- output ---\n" <> String.trim_trailing(output)
  end

  defp screen_text(max_lines) do
    with {:ok, path} <- screenshot(),
         {:ok, text} <- Look.analyze(path, %{mode: "text", max_lines: max_lines}) do
      {:ok, text}
    end
  end

  defp screenshot do
    case browser("screenshot") do
      {:ok, blocks} ->
        case Enum.find_value(blocks, &image_data/1) do
          nil ->
            {:error, "chrome upstream returned no image data for `screenshot`"}

          data ->
            case Base.decode64(data) do
              {:ok, jpeg} ->
                File.write!(@shot_path, jpeg)
                {:ok, @shot_path}

              :error ->
                {:error, "could not base64-decode the screenshot"}
            end
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp image_data(%{"data" => data}) when is_binary(data), do: data
  defp image_data(%{data: data}) when is_binary(data), do: data
  defp image_data(_), do: nil

  # --- Chrome upstream plumbing ---

  defp browser_text(command) do
    case browser(command) do
      {:ok, blocks} ->
        text =
          blocks
          |> Enum.map(&block_text/1)
          |> Enum.reject(&is_nil/1)
          |> Enum.join("\n")

        if text == "", do: {:ok, "(no text reply)"}, else: {:ok, text}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Extracts the value returned by an `eval` command.

  The chrome DSL echoes the command first and prints the JSON-encoded result on
  the following line, so the echoed JS must never be inspected.
  """
  def eval_result(reply) when is_binary(reply) do
    reply
    |> String.split("\n")
    |> Enum.reject(&(String.trim(&1) == ""))
    |> List.last()
    |> Kernel.||("")
    |> String.trim()
    |> String.trim("\"")
  end

  def eval_result(_), do: ""

  defp block_text(%{"type" => "text", "text" => t}), do: t
  defp block_text(%{type: "text", text: t}), do: t
  defp block_text(%{"text" => t}) when is_binary(t), do: t
  defp block_text(_), do: nil

  defp browser(command) do
    case ClientManager.call_tool(@chrome_server, @chrome_tool, %{"commands" => command}) do
      {:ok, %{"content" => blocks}} when is_list(blocks) ->
        {:ok, blocks}

      {:ok, %{content: blocks}} when is_list(blocks) ->
        {:ok, blocks}

      {:ok, other} when is_list(other) ->
        {:ok, other}

      {:ok, other} ->
        {:error, "unexpected chrome reply: #{inspect(other, limit: 20)}"}

      {:error, :server_not_found} ->
        {:error, "chrome upstream server is not configured/enabled"}

      {:error, :server_not_running} ->
        {:error, "chrome upstream server is not running (extension paired?)"}

      {:error, reason} ->
        {:error, "chrome upstream error: #{inspect(reason)}"}
    end
  end

  # --- misc ---

  defp clamp_timeout(s) when is_integer(s), do: s |> max(5) |> min(@max_timeout_s)
  defp clamp_timeout(_), do: @default_timeout_s
end
