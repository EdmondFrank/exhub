defmodule Exhub.BrowserAgent.BrowserMcp do
  @moduledoc """
  The Browser MCP backend: drives the user's own signed-in Chrome.

  `Exhub.BrowserAgent.KuriHttp` (the default) talks to the ExHub-managed `kuri`
  daemon, i.e. a separate headless Chrome. This backend reaches the browser the
  user is already logged into, through the `chrome` upstream MCP server (Browser
  MCP), whose single `browser` tool executes a small command DSL
  (`snapshot -i`, `click e12`, `fill getByLabel('Email') "..."`,
  `get text --max all`, `eval "..."`).

  ## What it borrows from jev-cdp

    * **One decision per cycle, no generated actions.** The DSL is executed,
      never composed: `Exhub.BrowserAgent.Policy` picks one operation plus one
      observed ref and this module turns that into one command.
    * **A stale decision must not execute.** Browser MCP refs belong to a page:
      they fail after a navigation and can be reused after a re-render, so
      `click`/`fill`/`type`/`select` re-snapshot first and refuse the action when
      the ref is gone or now names a different role/label than the table the
      decision came from (`verify: false` opts out). The baseline is the table
      `snap/1` returned to the loop in the same process.
    * **Focus is not progress.** Browser MCP's `active` marker is dropped from
      the element state, so a click that only moves focus does not read as a page
      change in `Agent`'s fingerprint.
    * **An auditable trace.** `Agent` already records operations, refs, labels,
      confidences, `page_changed` and elapsed time, and enforces `max_steps`;
      `console/1` adds the page's console errors when a step failed silently.
    * **Secrets stay out of the trace.** A field the DSL refuses to expose is
      never echoed back: only the command's own output is returned.

  ## Selection

      config :exhub, Exhub.BrowserAgent, backend: :chrome

  or per run: the `browser_agent` tool's `source: "chrome"`, or by injecting the
  module (`Agent.new(goal, kuri: Exhub.BrowserAgent.BrowserMcp)`).

  The upstream server/tool names are configurable
  (`config :exhub, Exhub.BrowserAgent.BrowserMcp, server: "chrome", tool: "browser"`)
  and `:call` injects the transport (`(map -> {:ok, text} | {:error, reason})`)
  so the backend can be tested without a browser.
  """

  alias Exhub.BrowserAgent.Snapshot

  @default_server "chrome"
  @default_tool "browser"
  @default_text_limit 8_000
  @table_key {__MODULE__, :table}

  # Lines the tool adds around a command's own output: one per executed command,
  # the trailing page banner, and the truncation note of `get text`.
  @noise ~r/^(✓|✗|Page: |…)/

  @typedoc "Result shared with the other backends."
  @type result :: {:ok, String.t()} | {:error, String.t()}

  # --- observation ---

  @doc """
  Takes an accessibility snapshot and returns the actionable element table.

  Browser MCP's `snapshot -i` already is a compact Playwright-derived tree that
  keeps interactive elements, landmarks, headings and cells, so the table is
  filtered to the roles the policy can act on (`Snapshot.clickable_roles/0` +
  `Snapshot.editable_roles/0`) the same way `KuriHttp.snap/1` filters kuri's.
  """
  @spec snap(keyword()) :: {:ok, [map()]} | {:error, String.t()}
  def snap(opts \\ []) do
    with {:ok, text} <- run("snapshot -i", opts) do
      nodes = parse_snapshot(text)
      remember(nodes)
      {:ok, actionable(nodes)}
    end
  end

  @doc """
  The DOM fallback, kept for interface parity.

  Browser MCP reads the page through Playwright rather than CDP's full
  accessibility tree, so the wide-DOM failure `Agent` falls back from does not
  occur here; re-running the snapshot is exactly what a caller needs when the
  first attempt failed (no tab yet, page still loading).
  """
  @spec dom_snapshot(keyword()) :: {:ok, [map()]} | {:error, String.t()}
  def dom_snapshot(opts \\ []), do: snap(opts)

  @doc "Returns the visible page text."
  @spec text(keyword()) :: result()
  def text(opts \\ []) do
    limit = Keyword.get(opts, :text_limit, config(:text_limit, @default_text_limit))

    with {:ok, output} <- run("get text --max #{limit}", opts) do
      {:ok, output |> lines() |> Enum.join("\n") |> String.trim()}
    end
  end

  @doc "Evaluates a JavaScript function or expression in the page."
  @spec eval(String.t(), keyword()) :: result()
  def eval(expression, opts \\ []) when is_binary(expression) do
    with {:ok, output} <- run("eval " <> quote_arg(expression), opts) do
      case lines(output) do
        [] -> {:ok, ""}
        [value] -> {:ok, unwrap_value(value)}
        values -> {:ok, Enum.join(values, "\n")}
      end
    end
  end

  @doc "Reads the tab's console errors and uncaught exceptions."
  @spec console(keyword()) :: result()
  def console(opts \\ []) do
    with {:ok, output} <- run("console --errors", opts) do
      {:ok, output |> lines() |> Enum.join("\n") |> String.trim()}
    end
  end

  # --- actions ---

  @doc "Clicks the element with the given ref."
  @spec click(String.t(), keyword()) :: result()
  def click(ref, opts \\ []), do: act(ref, "click #{ref}", opts)

  @doc "Clears and fills an editable element."
  @spec fill(String.t(), String.t(), keyword()) :: result()
  def fill(ref, value, opts \\ []), do: act(ref, "fill #{ref} #{quote_arg(value)}", opts)

  @doc """
  Clicks the element and types into it (Browser MCP has no targeted `type`).

  Unlike `fill/3` this appends at the caret instead of replacing the value.
  """
  @spec type(String.t(), String.t(), keyword()) :: result()
  def type(ref, value, opts \\ []), do: act(ref, "click #{ref}\ntype #{quote_arg(value)}", opts)

  @doc "Selects a value from a dropdown."
  @spec select(String.t(), String.t(), keyword()) :: result()
  def select(ref, value, opts \\ []),
    do: act(ref, "select #{ref} #{quote_arg(value)}", opts)

  @doc "Scrolls the page one screen in `direction` (`:up` or `:down`)."
  @spec scroll(:up | :down, keyword()) :: result()
  def scroll(direction \\ :down, opts \\ []) do
    command = if direction == :up, do: "scroll up", else: "scroll down"

    with {:ok, output} <- run(command, opts), do: {:ok, output |> lines() |> Enum.join("\n")}
  end

  @doc "Opens `url` in a tab of the agent's own tab group."
  @spec go(String.t(), keyword()) :: result()
  def go(url, opts \\ []) when is_binary(url) do
    with {:ok, output} <- run("goto " <> quote_arg(url), opts),
         do: {:ok, lines(output) |> Enum.join("\n")}
  end

  @doc "Lists the browser's tabs (the agent's are marked `*`)."
  @spec tabs(keyword()) :: result()
  def tabs(opts \\ []) do
    with {:ok, output} <- run("tab list", opts), do: {:ok, output |> lines() |> Enum.join("\n")}
  end

  @doc "Switches to an existing tab by its Browser MCP tab id."
  @spec use(String.t() | integer(), keyword()) :: result()
  def use(tab_id, opts \\ []) do
    with {:ok, output} <- run("tab select #{tab_id}", opts),
         do: {:ok, lines(output) |> Enum.join("\n")}
  end

  @doc "Reports the attached tab's URL and title."
  @spec status(keyword()) :: result()
  def status(opts \\ []) do
    with {:ok, output} <- run("get url\nget title", opts),
         do: {:ok, output |> lines() |> Enum.join("\n")}
  end

  @doc """
  Runs one or more command lines through the `browser` tool in a single call.

  Browser MCP stops at the first failing line, so a caller that batches commands
  must order them so that a failure cannot make a later line act on the wrong
  page.
  """
  @spec run(String.t(), keyword()) :: result()
  def run(commands, opts \\ []) when is_binary(commands) do
    caller = Keyword.get(opts, :call, &default_call/1)

    case caller.(%{"commands" => commands}) do
      {:ok, text} when is_binary(text) -> {:ok, text}
      {:ok, other} -> {:ok, inspect(other)}
      {:error, reason} -> {:error, error_message(reason)}
      other -> {:error, "unexpected browser result: #{inspect(other)}"}
    end
  end

  # --- snapshot parsing ---

  @doc """
  Parses `snapshot` output into accessibility nodes.

  Browser MCP prints one element per line:

      - textbox "Customer name:" [ref=e5]: Ada
        - radio "Small" [checked] [active] [ref=e16]

  i.e. `- <role> "<name>" [<state>]* [ref=<ref>][: <value>]`, indented two
  spaces per level, with `f1e3` refs inside iframes. Output nodes are shaped for
  `Exhub.BrowserAgent.Snapshot.parse/1`.
  """
  @spec parse_snapshot(String.t()) :: [map()]
  def parse_snapshot(text) when is_binary(text) do
    text
    |> lines()
    |> Enum.flat_map(fn line -> List.wrap(parse_line(line)) end)
  end

  # --- internals ---

  defp act(ref, commands, opts) do
    with :ok <- verify(ref, opts),
         {:ok, output} <- run(commands, opts) do
      {:ok, output |> lines() |> List.first() |> Kernel.||("")}
    end
  end

  # jev-cdp's guard: the page the decision was made from is re-read before input.
  # A ref that vanished (navigation) or now names another role/label (re-render)
  # aborts the step instead of clicking something else.
  defp verify(ref, opts) do
    if Keyword.get(opts, :verify, config(:verify, true)) do
      with {:ok, text} <- run("snapshot -i", opts) do
        nodes = parse_snapshot(text)
        verdict = check_ref(ref, nodes, baseline())
        remember(nodes)
        verdict
      end
    else
      :ok
    end
  end

  defp check_ref(ref, nodes, previous) do
    case Enum.find(nodes, &(&1["ref"] == ref)) do
      nil ->
        {:error, "ref #{ref} is no longer on the page; re-observe before acting"}

      %{"role" => role, "name" => name} ->
        case Map.get(previous, ref) do
          nil -> :ok
          {^role, ^name} -> :ok
          {old_role, old_name} -> changed(ref, role, name, old_role, old_name)
        end
    end
  end

  defp changed(ref, role, name, old_role, old_name) do
    {:error,
     "ref #{ref} now names #{role} #{inspect(name)} instead of #{old_role} " <>
       "#{inspect(old_name)}; the page changed under the decision"}
  end

  defp remember(nodes) do
    Process.put(@table_key, Map.new(nodes, &{&1["ref"], {&1["role"], &1["name"]}}))
    :ok
  end

  defp baseline, do: Process.get(@table_key, %{})

  defp actionable(nodes) do
    roles = Snapshot.clickable_roles() ++ Snapshot.editable_roles()

    case Enum.filter(nodes, &(&1["ref"] not in [nil, ""] and &1["role"] in roles)) do
      # An empty table would read as "not observed yet" to the loop, so keep the
      # full observation for pages without controls.
      [] -> nodes
      elements -> elements
    end
  end

  defp parse_line(line) do
    case Regex.run(~r/^(\s*)- (.+)$/s, line) do
      [_, indent, rest] ->
        {ref, rest} = take_ref(rest)
        {role, rest} = take_role(rest)
        {name, rest} = take_name(rest)
        {value, rest} = take_value(rest)

        %{
          "ref" => ref,
          "role" => role,
          "name" => name,
          "value" => value,
          "state" => states(rest),
          "depth" => div(String.length(indent), 2)
        }

      _ ->
        nil
    end
  end

  defp take_ref(rest) do
    case Regex.run(~r/ ?\[ref=([^\]]+)\]/, rest) do
      [full, ref] -> {ref, String.replace(rest, full, "", global: false)}
      _ -> {nil, rest}
    end
  end

  defp take_role(rest) do
    case Regex.run(~r/^(\S+)/, rest) do
      [_full, role] -> {role, String.replace_prefix(rest, role, "")}
      _ -> {"unknown", rest}
    end
  end

  defp take_name(rest) do
    case Regex.run(~r/^ +"((?:[^"\\]|\\.)*)"/, rest) do
      [full, name] -> {decode(name), String.replace_prefix(rest, full, "")}
      _ -> {nil, rest}
    end
  end

  defp take_value(rest) do
    case Regex.run(~r/^: ?(.*)$/s, String.trim_trailing(rest)) do
      [_, value] -> {blank_to_nil(decode(String.trim(value))), ""}
      _ -> {nil, rest}
    end
  end

  # `[checked]`, `[active]`, `[disabled]`... states come before the ref in the
  # real output, and `take_ref/1` removed it first, so whatever brackets remain
  # are states. `…` is the truncation marker, not a state.
  defp states(rest) do
    ~r/\[([^\]\[]*)\]/
    |> Regex.scan(rest, capture: :all_but_first)
    |> List.flatten()
    # `url=`/`value=` style attributes are not states, and `active` marks the
    # focused element — the fingerprint of `Agent` deliberately ignores focus, so
    # a click that only moves it must not read as a page change.
    |> Enum.reject(&(&1 in ["", "…", "active"] or String.contains?(&1, "=")))
    |> Enum.join(" ")
  end

  defp decode(nil), do: nil

  defp decode(text) do
    String.replace(text, ~r/\\(.)/s, "\\1")
  end

  defp blank_to_nil(nil), do: nil
  defp blank_to_nil(""), do: nil
  defp blank_to_nil(text), do: text

  # `eval` prints the result as JSON. `JSON.stringify(...)` therefore arrives
  # one layer deeper than the value kuri's `eval` returns, so unwrap a printed
  # JSON string once (and once more when the script stringified its own result).
  defp unwrap_value(raw) do
    case Jason.decode(raw) do
      {:ok, value} when is_binary(value) ->
        case Jason.decode(value) do
          {:ok, inner} when is_binary(inner) -> inner
          _ -> value
        end

      _ ->
        raw
    end
  end

  # Browser MCP splits arguments on spaces with shell-like quoting, so an
  # expression is sent as one double-quoted argument: newlines collapse (one
  # command per line) and quotes are escaped.
  defp quote_arg(text) do
    escaped =
      text
      |> String.replace(~r/\s*\n\s*/, " ")
      |> String.replace(~s("), ~s(\\"))

    ~s("#{escaped}")
  end

  # The tool's own output: everything that is not a `✓`/`✗` echo, the trailing
  # `Page:` banner, or a `…` truncation note.
  defp lines(text) when is_binary(text) do
    text
    |> String.split("\n")
    |> Enum.reject(&Regex.match?(@noise, &1))
    |> Enum.drop_while(&(String.trim(&1) == ""))
    |> Enum.reverse()
    |> Enum.drop_while(&(String.trim(&1) == ""))
    |> Enum.reverse()
  end

  # --- transport ---

  defp default_call(arguments) do
    case Exhub.MCP.Hub.ClientManager.call_tool(server(), tool(), arguments) do
      {:ok, result} -> {:ok, tool_text(result)}
      {:error, reason} -> {:error, reason}
    end
  end

  defp tool_text(%{"content" => [part | _]}), do: part_text(part)
  defp tool_text(%{content: [part | _]}), do: part_text(part)
  defp tool_text(other), do: inspect(other)

  defp part_text(%{"text" => text}), do: text
  defp part_text(%{text: text}), do: text
  defp part_text(other), do: inspect(other)

  defp error_message(reason) when is_binary(reason), do: reason
  defp error_message(reason), do: inspect(reason)

  defp server, do: config(:server, @default_server)
  defp tool, do: config(:tool, @default_tool)

  defp config(key, default),
    do: Application.get_env(:exhub, __MODULE__, []) |> Keyword.get(key, default)
end
