defmodule Exhub.Lua.Engine do
  @moduledoc """
  Transport-agnostic Lua code-mode engine.

  Instead of paying one LLM round trip per tool call (discover → `call_tools` →
  discover → `call_tools` …), a caller can write **one** Lua 5.3 snippet that
  calls every visible tool as a plain function, filters/branches/loops over the
  results, and returns a distilled answer. This is the
  [code execution beats function calling](https://www.anthropic.com/engineering/code-execution-with-mcp)
  pattern, ported from AiderDesk's `programmatic_tool_calls` extension.

  The design is borrowed from [Legion](https://legion.hexdocs.pm/Legion.html)
  (an Elixir runtime for code-writing agents) without depending on it: execution
  goes through the `Exhub.Lua.Sandbox` behaviour (default `Exhub.Lua.Sandbox.Lua`,
  the pure-Elixir [`lua`](https://hexdocs.pm/lua) VM, which cannot reach the BEAM
  except through the tool closures `Exhub.Lua.Bridge` registers), and is bounded
  by `Exhub.Lua.Runner`.

  ## Tool surface

  Every visible tool is bridged in twice, so a caller can use whichever name it
  already has:

    * Nested, by server — `desktop.read_file({path = \"/etc/hosts\"})`,
      `web_tools.web_fetch({url = \"https://…\"})`. The server name and the tool
      name have every character outside `[A-Za-z0-9_]` replaced with `_`.
    * Flat, by the verbatim `server__tool` name returned by `retrieve_tools` —
      `tools[\"desktop__read_file\"]({…})`. This key is **not** sanitized: a
      hyphenated server keeps its dash (`tools[\"browser-use__browser_navigate\"]`).

  Arguments must be a Lua table with named keys (it is decoded to a JSON
  object). `print(...)` output is collected and returned alongside the
  snippet's value.

  ## Tool discovery

  Three helpers let a snippet discover tools from inside the sandbox, without a
  separate `retrieve_tools` round trip:

    * `retrieve_tools({query = \"…\", limit = 10})` — the in-sandbox counterpart
      of the hub's own `retrieve_tools`, backed by the same `ToolSearch` TF-IDF
      ranking over the bridged tool set. Returns `{tools = {...}, count = n}`;
      an empty query browses every tool alphabetically.
    * `describe_tool(\"server__tool\")` (or `\"server.tool\"`) — the full
      `description` and `input_schema` of one tool, for the nested arguments a
      compact param summary hides. Unknown paths return `{error = \"…\"}`.

  * `help()` / `help(\"server.tool\")` — the tool index, or one tool's rendered
      description and parameters (`Exhub.Lua.Help`). The same docs are exposed as
      the MCP tool `Exhub.MCP.Tools.Lua.Help`.

  All three are reserved globals (see below).

  ## Failure semantics

  A failing call raises a Lua error, catchable with `pcall`. Both failure
  sources raise:

    * hub/transport errors (unknown server, connection loss, timeout) — the
      `{:error, reason}` branch of the tool-call function; and
    * MCP-level failures, i.e. a result carrying `isError = true`.

  MCP failures raise by default so a tool error is never silently mistaken for
  a successful result. Set `raise_on_tool_error: false` (config, or the `run/3`
  option of the same name) to instead return the error payload as ordinary
  data — then check `r.isError`. Note that `pcall` yields the *message string*,
  so the structured payload is not recoverable once an error has raised.

  An *uncaught* failure is returned as a categorised diagnostic: `SyntaxError`
  (the snippet did not compile), `UnknownTool` (a call on a missing tool or
  global, with a `retrieve_tools` hint), `ToolFailure` (`server.tool failed: …`),
  or `TimeoutExceeded`.

  ## Parallelism

  `parallel({…})` fans a list of tool calls out concurrently — the `Promise.all`
  analog. Results come back index-aligned with the input:

      local r = parallel({
        {server = \"desktop\", tool = \"read_file\",       args = {path = \"/etc/hosts\"}},
        {server = \"desktop\", tool = \"execute_command\", args = {command = \"uname -a\"}}
      })
      return r[1].content[1].text .. r[2].content[1].text

  A call may also be given as `{name = \"server__tool\", args = {…}}`.

  `parallel` still aborts on the first failure, but it does not discard the
  batch: `Bridge.run_parallel/3` has already waited for every call, so the abort
  carries a report of each call — descriptor plus outcome. When at least one
  call succeeded that report is returned as an **ordinary result**, not an
  error, so a single bad call cannot replace the successes; only a batch in
  which *every* call failed comes back as an error. A `pcall(parallel, {…})`
  inside the script still catches the abort, and receives the same report as a
  string.

  `parallel_all({…})` is the all-settled analog: it never raises, returning one
  `{ok = true, result = …}` or `{ok = false, error = …}` per call.

  ## Limits

  Each evaluation runs in its own short-lived process, bounded by a wall-clock
  timeout, an instruction budget, a call-depth cap and a heap cap (see
  `Exhub.Lua.Runner`). Runaway loops and allocation bombs die in this process —
  the caller is never linked to it (`spawn_monitor/1`). Concurrency inside a
  snippet is bounded by the `max_concurrency` config.

  The formatted result is capped at `max_output_chars` (default `100_000`) to
  bound context; when it is longer, the full text is written to a temp file and
  its path is returned alongside the truncated prefix, so the caller can read it
  back later. Set `spill_truncated: false` to disable this, or `spill_dir` to
  choose the directory, under `config :exhub, :lua`.

  `print`, `parallel`, `parallel_all`, `retrieve_tools` and `describe_tool` are
  reserved globals; a server whose name sanitizes onto one of them is only
  bridged through the flat `tools` table.

  Config lives under `config :exhub, :lua`.
  """

  require Logger

  alias Exhub.Lua.Diagnostics
  alias Exhub.Lua.Runner
  alias Exhub.Lua.Value

  # `timeout_ms` matches the MCP server's `request_timeout` (600s, see
  # `application.ex` / `router.ex`): the sandbox must hit its own wall-clock
  # limit before the transport hard-kills the `tools/call` Task, so it can
  # return a graceful timeout error.
  @default_config [
    enabled: true,
    timeout_ms: 600_000,
    max_instructions: 5_000_000,
    max_call_depth: 200,
    max_heap_size: 268_435_456,
    max_string_bytes: 8_388_608,
    max_output_chars: 100_000,
    spill_truncated: true,
    spill_dir: nil,
    max_concurrency: 8,
    raise_on_tool_error: true,
    # `:on_demand` keeps the MCP `code_mode` description short and routes the
    # catalog through the `help` / `retrieve_tools` / `describe_tool` helpers;
    # `:inline` embeds the namespace catalog in the description.
    tool_docs: :on_demand,
    exclude_servers: ["mcp-hub", "lua"]
  ]

  # Truncated results are spilled to a temp file the caller can read back later;
  # files matching this prefix/suffix are swept once older than the max age so
  # the temp directory does not grow without bound.
  @spill_prefix "code_mode-"
  @spill_suffix ".txt"
  @spill_max_age_seconds 24 * 60 * 60

  @type tool :: map()
  @type run_result :: {:ok, String.t()} | {:error, String.t()}

  @doc """
  Effective code-mode configuration (defaults deep-merged with
  `config :exhub, :lua`).
  """
  @spec config() :: keyword()
  def config do
    Keyword.merge(@default_config, Application.get_env(:exhub, :lua, []))
  end

  @doc "Whether code mode is enabled (config `:enabled`, default `true`)."
  @spec enabled?() :: boolean()
  def enabled?, do: config()[:enabled] == true

  @doc """
  Server namespaces currently visible through the hub, read from the tool search
  index (cheap, no `ClientManager` round trip).

  Hub-specific: the standalone `lua` server sources its namespaces from the same
  bridged tool list it exposes, so this may move out of the engine.
  """
  @spec namespaces() :: [String.t()]
  def namespaces do
    case Exhub.MCP.Hub.Store.get_search_index() do
      [{:index, {docs, _}}] when is_list(docs) ->
        excluded = config()[:exclude_servers] || []

        docs
        |> Enum.map(&Map.get(&1, "server"))
        |> Enum.reject(&is_nil/1)
        |> Enum.reject(&(&1 in excluded))
        |> Enum.uniq()
        |> Enum.sort()

      _ ->
        []
    end
  rescue
    _ -> []
  end

  @doc """
  Evaluates `code` in a Lua sandbox with `tools` bridged in as functions.

  ## Options

    * `:timeout_ms` — wall-clock budget for the evaluation (default from config)
    * `:call_fun` — `(server, tool, args) -> {:ok, result} | {:error, reason}`,
      defaults to `Exhub.MCP.Hub.ClientManager.call_tool/3`; injectable for tests
  """
  @spec run(term(), term(), keyword()) :: run_result()
  def run(code, tools, opts \\ []) do
    impl = sandbox()

    case impl.check(code, tools) do
      {:error, message} -> {:error, message}
      :ok -> do_run(code, tools, opts, impl)
    end
  end

  defp do_run(code, tools, opts, impl) do
    cfg =
      config()
      |> Keyword.merge(
        Keyword.take(opts, [
          :raise_on_tool_error,
          :max_concurrency,
          :spill_truncated,
          :spill_dir
        ])
      )

    timeout = Keyword.get(opts, :timeout_ms, cfg[:timeout_ms])
    max_chars = Keyword.get(opts, :max_output_chars, cfg[:max_output_chars])
    call_fun = Keyword.get(opts, :call_fun, &__MODULE__.call_hub_tool/3)

    result =
      Runner.run(
        fn ->
          Runner.cap_heap(cfg[:max_heap_size])
          impl.execute(code, tools, call_fun, cfg)
        end,
        timeout
      )
      |> handle_run_result()

    truncate_result(result, max_chars, cfg)
  end

  defp handle_run_result({:ok, {:ok, results, logs}}), do: {:ok, format_result(results, logs)}

  # A strict `parallel` abort that kept some successes arrives as an
  # already-formatted report *string*, not a list of Lua return values, so it
  # must not go through `format_result/2`.
  defp handle_run_result({:ok, {:partial, report, logs}}), do: {:ok, format_report(report, logs)}
  defp handle_run_result({:ok, {:lua_error, message}}), do: {:error, message}

  defp handle_run_result({:exit, reason}) do
    Logger.warning("[Lua.Engine] evaluation exited: #{inspect(reason)}")
    {:error, "Execution failed: #{Diagnostics.format_reason(reason)}"}
  end

  defp handle_run_result({:timeout, ms}),
    do: {:error, "TimeoutExceeded: execution timed out after #{ms} ms"}

  defp sandbox, do: config()[:sandbox] || Exhub.Lua.Sandbox.Lua

  @doc """
  Default bridge: dispatch a tool call through the hub.
  """
  @spec call_hub_tool(String.t(), String.t(), map()) :: {:ok, term()} | {:error, term()}
  def call_hub_tool(server, tool, args) do
    Exhub.MCP.Hub.ClientManager.call_tool(server, tool, args)
  end

  # --- Result formatting ---

  defp format_result(results, logs) do
    body =
      case results do
        [] -> "Execution completed with no return value."
        [single] -> Value.render_value(single)
        many -> Value.render_values(many)
      end

    prepend_logs(body, logs)
  end

  defp format_report(report, logs), do: prepend_logs(report, logs)

  defp prepend_logs(body, logs) do
    case logs do
      [] -> body
      _ -> "print output:\n" <> Enum.join(logs, "\n") <> "\n\n" <> body
    end
  end

  # --- Limits & errors ---

  defp truncate_result({:ok, text}, max, cfg), do: {:ok, truncate(text, max, cfg)}
  defp truncate_result({:error, text}, max, cfg), do: {:error, truncate(text, max, cfg)}
  defp truncate_result(other, _max, _cfg), do: other

  defp truncate(text, max, cfg) when is_integer(max) and max > 0 and byte_size(text) > max do
    notice = "\n... (truncated; #{byte_size(text)} bytes total)"

    case spill(text, cfg) do
      nil ->
        utf8_prefix(text, max) <> notice

      path ->
        utf8_prefix(text, max) <>
          notice <>
          "\nFull output saved to: #{path}\n" <>
          "Read it back with desktop.read_file({path = #{inspect(path)}})."
    end
  end

  defp truncate(text, _max, _cfg), do: text

  # Persists the full, untruncated result so the caller can read it back after
  # receiving only the prefix. Best-effort: `spill_truncated: false`, or any
  # filesystem error, yields `nil` and the plain truncation notice.
  defp spill(text, cfg) do
    if cfg[:spill_truncated] == false do
      nil
    else
      dir = cfg[:spill_dir] || System.tmp_dir!()
      path = Path.join(dir, "code_mode-#{rand_hex()}.txt")

      case File.write(path, text) do
        :ok ->
          prune_spills(dir)
          path

        {:error, _reason} ->
          nil
      end
    end
  rescue
    _ -> nil
  end

  # Temp files outlive the request on purpose (the caller may read them later);
  # sweep stale ones so the temp directory does not grow without bound.
  defp prune_spills(dir) do
    cutoff = System.os_time(:second) - @spill_max_age_seconds

    case File.ls(dir) do
      {:ok, entries} ->
        entries
        |> Enum.filter(&spill_file?/1)
        |> Enum.each(fn entry ->
          path = Path.join(dir, entry)

          with {:ok, %File.Stat{mtime: mtime}} <- File.stat(path, time: :posix),
               true <- mtime < cutoff do
            File.rm(path)
          end
        end)

      _ ->
        :ok
    end
  rescue
    _ -> :ok
  end

  defp spill_file?(name) do
    String.starts_with?(name, @spill_prefix) and String.ends_with?(name, @spill_suffix)
  end

  defp rand_hex do
    :crypto.strong_rand_bytes(6) |> Base.encode16(case: :lower)
  end

  # `binary_part/3` counts bytes and can split a multibyte UTF-8 codepoint,
  # producing an invalid binary for the JSON encoder downstream. Back off over
  # any dangling continuation bytes (at most 3) to land on a valid boundary.
  defp utf8_prefix(text, max) do
    Enum.find_value(0..3, fn drop ->
      len = max - drop

      if len >= 0 do
        candidate = binary_part(text, 0, len)
        String.valid?(candidate) && candidate
      end
    end) || binary_part(text, 0, max)
  end
end
