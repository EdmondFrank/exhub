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

  ## Reading the screen: exact buffer first

  The terminal API is reachable from the page. Luna mounts a Vue app whose
  component tree holds the live `Terminal` instance, and
  `terminal.buffer.active.getLine(i).translateToString(true)` is the exact,
  already-parsed screen: characters, `isWrapped` flags and the cursor row.
  `run` and `read` use that channel whenever it is available —

    * no screenshot and no vision call (a poll costs one ~20 ms `eval`);
    * byte-exact text, immune to OCR glyph slips;
    * the echoed command is dropped by content, not by heuristics, and the
      marker is matched line-anchored on the buffer so `WTDONE <rc>` cannot be
      confused with the echo;
    * the buffer type is checked, so a full-screen program (`alternate`) is
      refused instead of silently captured.

  A screenshot + `look` OCR remains as the fallback for pages where no
  terminal instance can be found (non-xterm terminals, a different app shell).
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

  # Vision OCR occasionally drops or swaps a glyph ("WTDONE" -> "WDONE",
  # "WTD0NE"), so the sentinel is matched with one edit of tolerance: W + up to
  # four capitals + N + E/F/D. The pattern is anchored to the whole line, so the
  # echoed command (which also contains `WTDONE %s`) can never match it.
  @marker_regex ~r/^[ \t]*W[A-Z0-9]{0,4}N[EFD][ \t]+(-?[0-9O]+)[ \t]*$/m

  @first_poll_ms 500
  @poll_ms 900
  @marker_only_after 3

  # Exact-buffer path. A poll is a metadata-only read of the last few lines, so
  # it can run far more often (and far cheaper) than a screenshot + OCR.
  @buffer_poll_ms 120
  @buffer_chunk_lines 100
  @fast_output_lines 120
  @default_max_output_lines 1_000
  @max_output_lines 5_000

  @marker_prompt """
  Output ONLY the single line that starts with WTDONE (for example `WTDONE 0`), verbatim.
  If no such line is visible, output exactly NONE. No other text, no code fences.
  """

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
    - `run` and `read` read the terminal's own buffer (xterm `buffer.active`)
      through an injected `eval`: exact characters, no screenshot, no vision
      call. The reply says `source: buffer`; if no terminal instance can be
      found the tool falls back to screenshot + OCR (`source: ocr`) — that
      path can contain small OCR glitches, so verify security-critical strings
      with a second read.
    - The echoed command is dropped deterministically by line index on the
      buffer path; on the OCR fallback it stays best-effort.
    - A full-screen program (less/top/vim) puts the terminal in the alternate
      buffer; `run` refuses instead of capturing garbage. Pass
      `allow_alt_screen: true` to override.
    - `max_output_lines` (default 1000) caps how much output is returned.
    - `readback` forces a channel: `\"auto\"` (default), `\"buffer\"` (require the
      exact buffer) or `\"ocr\"` (legacy screenshot path, for A/B checks).
    - `max_lines` (default 40) bounds OCR transcription and the timeout dump.
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

    field(:max_output_lines, :integer,
      description:
        "action \"run\": cap on output lines returned from the terminal buffer (default 1000, max 5000)"
    )

    field(:allow_alt_screen, :boolean,
      description:
        "action \"run\": allow running inside a full-screen program (alternate buffer); default false"
    )

    field(:readback, :string,
      description:
        "action \"run\": \"auto\" (default: exact buffer, OCR fallback) | \"buffer\" (require the buffer) | \"ocr\" (force the legacy screenshot path)"
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
        max_out = clamp_output_lines(Map.get(params, :max_output_lines))
        alt? = Map.get(params, :allow_alt_screen, false) == true
        readback = Map.get(params, :readback) || "auto"

        run(command, max_lines, timeout_s, max_out, alt?, readback)

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
    max_lines = Map.get(params, :max_lines) || @default_max_lines

    case buffer_tail(max_lines) do
      {:ok, text} when text != "" -> {:ok, text}
      _ -> screen_text(max_lines)
    end
  end

  defp dispatch(%{action: action}), do: {:error, "unknown action: #{inspect(action)}"}
  defp dispatch(_), do: {:error, "`action` is required"}

  # --- Terminal primitives ---

  @doc false
  def insert_js(b64) do
    "(function(){var t;try{var b=atob('" <>
      b64 <>
      "');var u=new Uint8Array(b.length);" <>
      "for(var i=0;i<b.length;i++){u[i]=b.charCodeAt(i);}" <>
      "t=new TextDecoder('utf-8').decode(u);}catch(e){return 'b64-error';}" <>
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
        {:ok, reply} ->
          {:ok, "attached to tab #{id} (focus: #{eval_result(reply)}, #{buffer_label()})"}

        {:error, reason} ->
          {:error, reason}
      end
    else
      {:error, reason} -> {:error, reason}
      _ -> {:error, "no tab matching #{inspect(tab)}"}
    end
  end

  # --- Exact buffer read (the page's xterm instance) ---

  @doc """
  Builds the JS probe that finds the live xterm `Terminal` and returns part of
  its buffer.

  Options:

    * `:tail` — return the last N lines (default 4)
    * `:cursor` — return a small window around the cursor row (default ±8); the
      buffer may hold many blank lines below the prompt, so this is what polling
      uses to see a fresh marker
    * `:from` / `:to` — return an absolute line range instead

  Every line is `[index, text, is_wrapped]`. The string contains no double
  quote, so it can be embedded in the chrome `eval "<js>"` DSL (it is also
  base64-wrapped by `eval_js/1`).
  """
  @spec terminal_js(keyword()) :: String.t()
  def terminal_js(opts \\ []) do
    {from, to} =
      case {Keyword.get(opts, :from), Keyword.get(opts, :to)} do
        {nil, nil} ->
          if Keyword.get(opts, :cursor) do
            span = Keyword.get(opts, :span, 8)
            {"Math.max(0,cy-" <> Integer.to_string(span) <> ")", "cy+1"}
          else
            {"Math.max(0,n-" <> Integer.to_string(Keyword.get(opts, :tail, 4)) <> ")", "n"}
          end

        {from, to} ->
          {Integer.to_string(from), "Math.min(n," <> Integer.to_string(to) <> ")"}
      end

    """
    (function(){
      function isT(o){try{return !!o&&typeof o==='object'&&typeof o.cols==='number'&&typeof o.rows==='number'&&o.buffer&&o.buffer.active&&typeof o.buffer.active.getLine==='function';}catch(e){return false;}}
      function vis(t){try{var e=t.element;if(!e||!e.isConnected)return 0;var r=e.getBoundingClientRect();return (r.width>4&&r.height>4)?Math.round(r.width*r.height):0;}catch(e){return 0;}}
      var seen=new Set(),terms=[];
      function visit(o,d){
        if(!o||d>12||seen.size>200000)return;
        var ty=typeof o;
        if(ty!=='object'&&ty!=='function')return;
        if(seen.has(o))return;seen.add(o);
        try{if(typeof o.nodeType==='number')return;}catch(e){return;}
        if(isT(o)){terms.push(o);return;}
        var k;try{k=Object.keys(o);}catch(e){return;}
        for(var i=0;i<k.length;i++){try{visit(o[k[i]],d+1);}catch(e){}}
      }
      var roots=[document.querySelector('#__nuxt'),document.querySelector('#app'),document.querySelector('#root')];
      for(var j=0;j<roots.length;j++){var a=roots[j]&&roots[j].__vue_app__;if(a)visit(a,0);}
      if(!terms.length){
        var all=document.querySelectorAll('*');
        for(var q=0;q<all.length&&q<4000;q++){var c=all[q].__vueParentComponent;if(c)visit(c,0);}
      }
      if(!terms.length)return JSON.stringify({found:false});
      var t=terms[0];
      for(var z=1;z<terms.length;z++){if(vis(terms[z])>vis(t))t=terms[z];}
      var b=t.buffer.active,n=b.length,cy=b.baseY+b.cursorY;
      var lines=[];
      var end=Math.min(n,#{to}+1);
      for(var p=#{from};p<end;p++){var l=b.getLine(p);lines.push([p,l?l.translateToString(true):'',l&&l.isWrapped?1:0]);}
      var cl=b.getLine(cy);
      return JSON.stringify({found:true,type:b.type,len:n,cols:t.cols,rows:t.rows,cursor:cy,echo:cl?cl.translateToString(true):'',lines:lines});
    })()
    """
    |> String.trim()
  end

  @doc """
  Runs JS in the attached tab and decodes the JSON object it returns.

  `--max all` lifts the chrome DSL's 10 000-character reply cap: a buffer
  chunk of 100 lines can be wider than that on a 250-column terminal.
  """
  @spec eval_js(String.t()) :: {:ok, map()} | {:error, term()}
  def eval_js(js) when is_binary(js) do
    command = "eval --max all \"eval(atob('" <> Base.encode64(js) <> "'))\""

    with {:ok, reply} <- browser_text(command) do
      decode_payload(reply)
    end
  end

  @doc """
  Decodes the chrome `eval` reply.

  The DSL echoes the command and prints `JSON.stringify(result)` on the last
  line, so the payload is double-encoded: JSON string, then the actual object.
  """
  @spec decode_payload(String.t()) :: {:ok, map()} | {:error, String.t()}
  def decode_payload(reply) when is_binary(reply) do
    line =
      reply
      |> String.split("\n")
      |> Enum.reject(&(String.trim(&1) == ""))
      |> List.last()

    cond do
      is_nil(line) ->
        {:error, "empty eval reply"}

      String.starts_with?(String.trim(line), "…") and String.contains?(line, "cut at ") ->
        {:error,
         "eval reply was truncated by the browser extension: " <> String.slice(line, 0, 120)}

      true ->
        decode_json_line(line)
    end
  end

  def decode_payload(_), do: {:error, "empty eval reply"}

  defp decode_json_line(line) do
    case Jason.decode(line) do
      {:ok, inner} when is_binary(inner) ->
        case Jason.decode(inner) do
          {:ok, map} when is_map(map) -> {:ok, map}
          _ -> {:error, "eval payload is not a JSON object: " <> String.slice(inner, 0, 120)}
        end

      {:ok, map} when is_map(map) ->
        {:ok, map}

      _ ->
        {:error, "eval reply is not JSON: " <> String.slice(line, 0, 120)}
    end
  end

  @doc """
  Finds the last `WTDONE<nonce> <exit_code>` line in `lines`.

  `lines` is what `terminal_js/1` returns (`[index, text, is_wrapped]`). The
  per-call nonce means an older command's marker left in the scrollback can
  never be mistaken for this one, and the echoed command — which contains the
  literal `%s` rather than a number — can never match.
  """
  @spec marker_index([[integer() | String.t()]], String.t()) ::
          {:ok, integer(), integer()} | :pending
  def marker_index(lines, nonce) when is_list(lines) and is_binary(nonce) do
    regex = marker_regex(nonce)

    lines
    |> Enum.filter(fn
      [_idx, text, _wrapped] when is_binary(text) -> Regex.match?(regex, text)
      _ -> false
    end)
    |> List.last()
    |> case do
      nil ->
        :pending

      [idx, text, _wrapped] ->
        case Regex.run(regex, text) do
          [_, rc] -> {:ok, String.to_integer(rc), idx}
          _ -> :pending
        end
    end
  end

  def marker_index(_lines, _nonce), do: :pending

  @doc """
  Drops the echoed command — and anything before it — from a block of lines.

  The echo is the logical line containing the literal `WTDONE<nonce> %s`, which
  also finds it when xterm wrapped it across several visual lines. This is
  deterministic, unlike the OCR path's heuristic.
  """
  @spec strip_echo([[integer() | String.t()]], String.t()) :: [String.t()]
  def strip_echo(lines, nonce) when is_list(lines) and is_binary(nonce) do
    logical = join_wrapped(lines)

    case echo_index(logical, nonce) do
      nil -> logical
      idx -> Enum.drop(logical, idx + 1)
    end
  end

  # The echo is the only line carrying the literal `WTDONE<nonce> %s`; finding
  # it also tells us the output was not truncated by the read window.
  defp echo_index(logical, nonce) do
    needle = @marker <> nonce <> " %s"
    Enum.find_index(logical, &String.contains?(&1, needle))
  end

  defp marker_regex(nonce), do: ~r/^[ \t]*WTDONE#{nonce}[ \t]+(-?[0-9]+)[ \t]*$/

  @doc """
  Joins xterm's visual lines back into logical lines.

  xterm wraps a long logical line into several buffer lines, marking the
  continuations with `isWrapped`.
  """
  @spec join_wrapped([[integer() | String.t()]]) :: [String.t()]
  def join_wrapped(lines) when is_list(lines) do
    lines
    |> Enum.reduce([], fn
      [_idx, text, 1], [prev | rest] -> [prev <> text | rest]
      [_idx, text, _wrapped], acc when is_binary(text) -> [text | acc]
      _other, acc -> acc
    end)
    |> Enum.reverse()
  end

  # --- Command execution with a line-anchored marker ---

  @doc """
  Wraps a command so its exit status lands on its own line.

  The literal `%s` stays in the echoed command, while the produced line is
  `WTDONE<nonce> <rc>` — which is what makes `extract_result/1` echo-proof, and
  the nonce what makes each invocation's marker unique in the scrollback.
  """
  @spec wrap_command(String.t()) :: String.t()
  def wrap_command(command), do: wrap_command(command, "")

  @spec wrap_command(String.t(), String.t()) :: String.t()
  def wrap_command(command, nonce) do
    String.trim_trailing(command) <> "; printf '\\n" <> @marker <> nonce <> " %s\\n' $?"
  end

  @doc """
  Looks for a line that is `WTDONE <exit_code>` (one OCR edit tolerated).

  Returns `{:ok, exit_code, text_before_marker}` or `:pending`.
  """
  @spec extract_result(String.t()) :: {:ok, integer(), String.t()} | :pending
  def extract_result(screen) when is_binary(screen) do
    case find_marker(screen) do
      {:ok, rc, output, _grade} -> {:ok, rc, output}
      :pending -> :pending
    end
  end

  # `:exact` means the sentinel was read verbatim, which only happens on a
  # settled frame; `:fuzzy` means OCR had to be forgiven a glyph, i.e. the
  # screenshot was probably taken mid-paint and the transcript is suspect.
  defp find_marker(screen) do
    case Regex.run(@marker_regex, screen) do
      [marker, rc] ->
        with {:ok, code} <- normalize_rc(rc) do
          {pos, _} = :binary.match(screen, marker)
          {:ok, code, clean_output(binary_part(screen, 0, pos)), grade(marker)}
        else
          :error -> :pending
        end

      _ ->
        :pending
    end
  end

  defp grade(marker) do
    if Regex.match?(~r/^[ \t]*WTDONE[ \t]/, marker), do: :exact, else: :fuzzy
  end

  # OCR happily reads an exit code of 0 as a capital O.
  defp normalize_rc(raw) do
    case raw |> String.replace("O", "0") |> String.trim() |> Integer.parse() do
      {code, ""} -> {:ok, code}
      _ -> :error
    end
  end

  defp strip_marker_lines(text) do
    text
    |> String.split("\n")
    |> Enum.reject(&Regex.match?(@marker_regex, &1))
    |> Enum.join("\n")
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

  defp run(command, max_lines, timeout_s, max_out, alt?, readback) do
    if readback == "ocr" do
      run_ocr(command, max_lines, timeout_s)
    else
      run_auto(command, max_lines, timeout_s, max_out, alt?, readback)
    end
  end

  defp run_auto(command, max_lines, timeout_s, max_out, alt?, readback) do
    case buffer_state() do
      {:ok, %{"found" => true, "type" => "alternate"}} when not alt? ->
        {:error,
         "the terminal is in the alternate screen (a full-screen program such as " <>
           "less/vim/top is open). Exit it first, or pass allow_alt_screen: true."}

      {:ok, %{"found" => true} = state} ->
        run_buffer(command, state, max_lines, max_out, timeout_s, alt?)

      {:ok, _} when readback == "buffer" ->
        {:error, "no xterm terminal instance found on the attached tab (readback: buffer)"}

      {:error, reason} when readback == "buffer" ->
        {:error, "terminal buffer unavailable: #{reason}"}

      _ ->
        # No terminal instance reachable (non-xterm UI, app shell changed): keep
        # the screenshot + OCR path, which is slower and lossier.
        run_ocr(command, max_lines, timeout_s)
    end
  end

  defp run_ocr(command, max_lines, timeout_s) do
    # Start from a clean screen so the marker appears exactly once.
    _ = browser_text("press Control+l")
    Process.sleep(120)

    case paste(wrap_command(command), true, @chunk_size) do
      {:ok, _} ->
        deadline = now_ms() + timeout_s * 1000
        poll(command, deadline, max_lines, timeout_s, 0)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp run_buffer(command, state, max_lines, max_out, timeout_s, alt?) do
    nonce = nonce()
    echo_idx = state["cursor"]

    case paste(wrap_command(command, nonce), true, @chunk_size) do
      {:ok, _} ->
        deadline = now_ms() + timeout_s * 1000
        poll_buffer(nonce, echo_idx, deadline, max_lines, max_out, timeout_s, alt?, 0)

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Each poll is a metadata-only read (len/type/cursor + the last few lines), so
  # it costs ~20 ms and no vision call.
  defp poll_buffer(nonce, echo_idx, deadline, max_lines, max_out, timeout_s, alt?, attempts) do
    Process.sleep(@buffer_poll_ms)

    case buffer_state() do
      {:ok, %{"found" => true, "type" => "alternate"}} when not alt? ->
        {:error,
         "the command switched to the alternate screen; refusing to capture it " <>
           "(pass allow_alt_screen: true if that is intended)"}

      {:ok, %{"found" => true} = state} ->
        case marker_index(state["lines"], nonce) do
          {:ok, rc, marker_idx} ->
            output = fetch_output(nonce, marker_idx, echo_idx, max_out)
            {:ok, format_run(rc, output, attempts + 1, "buffer")}

          :pending ->
            if now_ms() >= deadline do
              {:error, timeout_error_buffer(timeout_s, max_lines, alt?)}
            else
              poll_buffer(
                nonce,
                echo_idx,
                deadline,
                max_lines,
                max_out,
                timeout_s,
                alt?,
                attempts + 1
              )
            end
        end

      {:ok, _} ->
        {:error,
         "lost the terminal buffer while waiting for the command to finish " <>
           "(the page may have navigated or the terminal was closed)"}

      {:error, reason} ->
        {:error, "buffer read failed: #{reason}"}
    end
  end

  # Reads a window ending at the marker and drops everything up to and
  # including the echoed command line. The echo is found by content (it is the
  # only line carrying the literal `WTDONE<nonce> %s`), so buffer indices that
  # shifted because xterm trimmed its scrollback do not matter.
  defp fetch_output(nonce, marker_idx, echo_idx, max_out) do
    # The fast path starts at the pre-command cursor row, so a normal command
    # costs one small read. The wide window is only needed when the echo is not
    # there (the scrollback trimmed, or the command scrolled the screen).
    from = max(0, min(echo_idx, marker_idx - @fast_output_lines))
    wide = max(0, marker_idx - max_out - 8)

    case output_window(nonce, from, marker_idx) do
      {:ok, text} ->
        text

      {:missing, _} when from > wide ->
        case output_window(nonce, wide, marker_idx) do
          {:ok, text} -> text
          {:missing, logical} -> truncated_output(logical, max_out)
        end

      {:missing, logical} ->
        truncated_output(logical, max_out)
    end
  end

  defp output_window(nonce, from, marker_idx) do
    case read_lines(from, marker_idx - 1) do
      {:ok, lines} ->
        logical = join_wrapped(lines)

        case echo_index(logical, nonce) do
          nil -> {:missing, logical}
          idx -> {:ok, logical |> Enum.drop(idx + 1) |> Enum.join("\n") |> trim_blank()}
        end

      {:error, _} ->
        {:ok, ""}
    end
  end

  defp truncated_output(logical, max_out) do
    "…(output truncated to the last #{max_out} lines)\n" <>
      (logical |> Enum.join("\n") |> trim_blank())
  end

  # A short, unambiguous tag so concurrent/older runs in the scrollback cannot
  # be confused with this one. Exact buffer text means no OCR tolerance is
  # needed, so the alphabet can exclude look-alike glyphs anyway.
  defp nonce do
    alphabet = ~c"ABCDEFGHJKMNPQRSTUVWXYZ23456789"

    for _ <- 1..6, into: "" do
      <<Enum.random(alphabet)>>
    end
  end

  defp read_lines(from, to) when from <= to do
    from..to
    |> Enum.chunk_every(@buffer_chunk_lines)
    |> Enum.reduce_while({:ok, []}, fn chunk, {:ok, acc} ->
      case eval_js(terminal_js(from: List.first(chunk), to: List.last(chunk))) do
        {:ok, %{"found" => true, "lines" => lines}} -> {:cont, {:ok, acc ++ lines}}
        {:ok, _} -> {:halt, {:error, :no_terminal}}
        {:error, _} = error -> {:halt, error}
      end
    end)
  end

  defp read_lines(_from, _to), do: {:ok, []}

  defp buffer_state do
    case eval_js(terminal_js(cursor: true)) do
      {:ok, %{"found" => true} = state} -> {:ok, state}
      {:ok, _} -> {:ok, %{"found" => false}}
      {:error, _} = error -> error
    end
  end

  defp buffer_tail(n) do
    with {:ok, %{"found" => true, "len" => len}} <- buffer_state(),
         {:ok, lines} <- read_lines(max(0, len - n), len - 1) do
      {:ok, render(lines)}
    else
      _ -> {:error, :no_terminal}
    end
  end

  defp buffer_label do
    case buffer_state() do
      {:ok, %{"found" => true, "type" => type, "cols" => cols, "rows" => rows}} ->
        "buffer: exact #{cols}x#{rows} (#{type})"

      _ ->
        "buffer: unavailable, OCR fallback"
    end
  end

  defp timeout_error_buffer(timeout_s, max_lines, alt?) do
    screen =
      case buffer_tail(max_lines) do
        {:ok, text} -> text
        _ -> ""
      end

    suffix = if alt?, do: "\n(the terminal is in the alternate screen)", else: ""

    "timed out after #{timeout_s}s waiting for the command to finish (buffer read)." <>
      suffix <> "\n--- last screen ---\n" <> screen
  end

  defp render(lines), do: lines |> join_wrapped() |> Enum.join("\n") |> trim_blank()

  defp trim_blank(text) do
    text
    |> String.split("\n")
    |> Enum.drop_while(&(String.trim(&1) == ""))
    |> Enum.reverse()
    |> Enum.drop_while(&(String.trim(&1) == ""))
    |> Enum.reverse()
    |> Enum.join("\n")
  end

  # Fast path: one full transcription per poll, which doubles as the result, so
  # a command typically costs a single `look` call. When the sentinel only
  # matched with OCR tolerance the frame was mid-paint, so the transcript is
  # re-read once the screen has settled. After @marker_only_after misses the
  # poll falls back to the marker-only prompt, which cannot be truncated on a
  # long screen.
  defp poll(command, deadline, max_lines, timeout_s, attempts) do
    Process.sleep(if attempts == 0, do: @first_poll_ms, else: @poll_ms)
    marker_only? = attempts >= @marker_only_after

    case read_screen(marker_only?, max_lines) do
      {:ok, screen} ->
        case find_marker(screen) do
          {:ok, rc, output, :exact} ->
            {:ok,
             format_run(rc, finish_output(command, marker_only?, output, max_lines), attempts + 1)}

          {:ok, rc, _output, :fuzzy} ->
            {:ok, format_run(rc, finish_output(command, true, "", max_lines), attempts + 1)}

          :pending ->
            if now_ms() >= deadline do
              {:error, timeout_error(timeout_s, marker_only?, screen)}
            else
              poll(command, deadline, max_lines, timeout_s, attempts + 1)
            end
        end

      {:error, reason} ->
        {:error, "screen read failed after #{attempts + 1} poll(s): #{reason}"}
    end
  end

  defp read_screen(true, _max_lines), do: marker_screen()
  defp read_screen(false, max_lines), do: screen_text(max_lines)

  # A marker-only read carries no transcript, so it always needs a full read.
  defp finish_output(command, true, _output, max_lines), do: read_output(command, max_lines)
  defp finish_output(command, false, output, _max_lines), do: clean_output(output, command)

  defp timeout_error(timeout_s, true, _screen) do
    "timed out after #{timeout_s}s waiting for the command to finish.\n" <>
      "--- last screen ---\n" <> String.trim(screen_text_or_blank(40))
  end

  defp timeout_error(timeout_s, false, screen) do
    "timed out after #{timeout_s}s waiting for the command to finish.\n" <>
      "--- last screen ---\n" <> String.trim(screen)
  end

  # Full transcription of a settled screen: this is the result.
  defp read_output(command, max_lines) do
    case screen_text(max_lines) do
      {:ok, screen} ->
        case find_marker(screen) do
          {:ok, _rc, output, _grade} -> clean_output(output, command)
          :pending -> screen |> strip_marker_lines() |> clean_output(command)
        end

      {:error, _reason} ->
        ""
    end
  end

  defp screen_text_or_blank(max_lines) do
    case screen_text(max_lines) do
      {:ok, screen} -> screen
      {:error, _reason} -> ""
    end
  end

  defp now_ms, do: System.monotonic_time(:millisecond)

  @doc false
  def format_run(rc, output, attempts), do: format_run(rc, output, attempts, "ocr")

  @doc false
  def format_run(rc, output, attempts, source) do
    "exit_code: #{rc}\npolls: #{attempts}\nsource: #{source}\n--- output ---\n" <>
      String.trim_trailing(output)
  end

  defp screen_text(max_lines) do
    with {:ok, path} <- screenshot(),
         {:ok, text} <- Look.analyze(path, %{mode: "text", max_lines: max_lines}) do
      {:ok, text}
    end
  end

  # Cheap poll read: only the sentinel line, a handful of output tokens.
  defp marker_screen do
    with {:ok, path} <- screenshot(),
         {:ok, text} <- Look.analyze(path, %{mode: "text", prompt: @marker_prompt}) do
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

  defp clamp_output_lines(n) when is_integer(n), do: n |> max(1) |> min(@max_output_lines)
  defp clamp_output_lines(_), do: @default_max_output_lines
end
