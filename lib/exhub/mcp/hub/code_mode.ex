defmodule Exhub.MCP.Hub.CodeMode do
  @moduledoc """
  Code-mode sandbox for the MCP hub.

  Instead of paying one LLM round trip per tool call (discover → `call_tools` →
  discover → `call_tools` …), a caller can write **one** Lua 5.3 snippet that
  calls every visible hub tool as a plain function, filters/branches/loops over
  the results, and returns a distilled answer. This is the
  [code execution beats function calling](https://www.anthropic.com/engineering/code-execution-with-mcp)
  pattern, ported from AiderDesk's `programmatic_tool_calls` extension.

  The design is borrowed from [Legion](https://legion.hexdocs.pm/Legion.html)
  (an Elixir runtime for code-writing agents) without depending on it: the
  engine is the pure-Elixir [`lua`](https://hexdocs.pm/lua) VM, which cannot
  reach the BEAM except through the tool closures we register.

  ## Tool surface

  Every visible hub tool is bridged in twice, so a caller can use whichever
  name it already has:

    * Nested, by server — `desktop.read_file({path = "/etc/hosts"})`,
      `web_tools.web_fetch({url = "https://…"})`. The server name and the tool
      name have every character outside `[A-Za-z0-9_]` replaced with `_`.
    * Flat, by the verbatim `server__tool` name returned by `retrieve_tools` —
      `tools["desktop__read_file"]({…})`. This key is **not** sanitized: a
      hyphenated server keeps its dash (`tools["browser-use__browser_navigate"]`).

  Arguments must be a Lua table with named keys (it is decoded to a JSON
  object). `print(...)` output is collected and returned alongside the
  snippet's value.

  ## Failure semantics

  A failing call raises a Lua error, catchable with `pcall`. Both failure
  sources raise:

    * hub/transport errors (unknown server, connection loss, timeout) — the
      `{:error, reason}` branch of `ClientManager.call_tool/3`; and
    * MCP-level failures, i.e. a result carrying `isError = true`.

  MCP failures raise by default so a tool error is never silently mistaken for
  a successful result. Set `raise_on_tool_error: false` (config, or the `run/3`
  option of the same name) to instead return the error payload as ordinary
  data — then check `r.isError`. Note that `pcall` yields the *message string*,
  so the structured payload is not recoverable once an error has raised.

  ## Parallelism

  `parallel({…})` fans a list of tool calls out concurrently — the `Promise.all`
  analog. Results come back index-aligned with the input, and the call raises
  on the first failure:

      local r = parallel({
        {server = "time",      tool = "get_current_time", args = {timezone = "UTC"}},
        {server = "web-tools", tool = "fetch",            args = {url = "…"}}
      })
      return r[1].content[1].text .. r[2].content[1].text

  A call may also be given as `{name = "server__tool", args = {…}}`.

  `parallel_all({…})` is the all-settled analog: it never raises, returning one
  `{ok = true, result = …}` or `{ok = false, error = …}` per call.

  ## Limits

  Each evaluation runs in its own short-lived process, bounded by a wall-clock
  timeout, an instruction budget, a call-depth cap and a heap cap. Runaway
  loops and allocation bombs die in this process — the caller is never linked
  to it (`spawn_monitor/1`). Concurrency inside a snippet is bounded by the
  `max_concurrency` config.

  `print`, `parallel` and `parallel_all` are reserved globals; a server whose
  name sanitizes onto one of them is only bridged through the flat `tools`
  table.
  """

  require Logger

  # `timeout_ms` matches the Hub server's `request_timeout` (600s, see
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
    max_output_chars: 12_000,
    max_concurrency: 8,
    raise_on_tool_error: true,
    exclude_servers: ["mcp-hub"]
  ]

  # Globals the sandbox owns; a server whose sanitized name lands on one of
  # these is only reachable through the flat `tools` table.
  @reserved_globals ~w(print parallel parallel_all)

  @type tool :: map()
  @type run_result :: {:ok, String.t()} | {:error, String.t()}

  @doc """
  Effective code-mode configuration (defaults deep-merged with
  `config :exhub, :code_mode`).
  """
  @spec config() :: keyword()
  def config do
    Keyword.merge(@default_config, Application.get_env(:exhub, :code_mode, []))
  end

  @doc "Whether code mode is enabled (config `:enabled`, default `true`)."
  @spec enabled?() :: boolean()
  def enabled?, do: config()[:enabled] == true

  @doc """
  Server namespaces currently visible through the hub, read from the tool
  search index (cheap, no `ClientManager` round trip).
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
  @spec run(String.t(), [tool()], keyword()) :: run_result()
  def run(code, tools, opts \\ [])

  def run(code, tools, opts) when is_binary(code) and is_list(tools) do
    cfg =
      config()
      |> Keyword.merge(Keyword.take(opts, [:raise_on_tool_error, :max_concurrency]))

    timeout = Keyword.get(opts, :timeout_ms, cfg[:timeout_ms])
    max_chars = Keyword.get(opts, :max_output_chars, cfg[:max_output_chars])
    call_fun = Keyword.get(opts, :call_fun, &__MODULE__.call_hub_tool/3)

    eval_fun = fn ->
      cap_heap(cfg[:max_heap_size])
      eval(code, tools, call_fun, cfg)
    end

    result =
      case run_bounded(eval_fun, timeout) do
        {:ok, {:ok, results, logs}} ->
          {:ok, format_result(results, logs)}

        {:ok, {:lua_error, message}} ->
          {:error, message}

        {:exit, reason} ->
          Logger.warning("[CodeMode] evaluation exited: #{inspect(reason)}")
          {:error, "Execution failed: #{format_reason(reason)}"}

        {:timeout, ms} ->
          {:error, "Execution timed out after #{ms} ms"}
      end

    truncate_result(result, max_chars)
  end

  def run(code, _tools, _opts) when not is_binary(code),
    do: {:error, "`code` must be a string"}

  def run(_code, _tools, _opts), do: {:error, "`tools` must be a list"}

  # Runs inside the sandbox process. Lua-level failures (bad syntax, a sandboxed
  # global, a tool raising) are returned as data rather than crashing the task.
  defp eval(code, tools, call_fun, cfg) do
    lua = build_sandbox(tools, call_fun, cfg)
    {results, _lua} = Lua.eval!(lua, code, source: "code_mode")
    {:ok, results, Process.get(:code_mode_log, [])}
  rescue
    error in [Lua.RuntimeException, Lua.CompilerException] ->
      {:lua_error, Exception.message(error)}
  end

  @doc """
  Default bridge: dispatch a tool call through the hub.
  """
  @spec call_hub_tool(String.t(), String.t(), map()) :: {:ok, term()} | {:error, term()}
  def call_hub_tool(server, tool, args) do
    Exhub.MCP.Hub.ClientManager.call_tool(server, tool, args)
  end

  # --- Sandbox construction ---

  defp build_sandbox(tools, call_fun, cfg) do
    lua =
      Lua.new(
        max_instructions: cfg[:max_instructions],
        max_call_depth: cfg[:max_call_depth],
        max_string_bytes: cfg[:max_string_bytes]
      )

    lua =
      lua
      |> Lua.set!([:print], print_fun())
      |> Lua.set!([:parallel], parallel_fun(call_fun, cfg, :strict))
      |> Lua.set!([:parallel_all], parallel_fun(call_fun, cfg, :soft))

    Enum.reduce(tools, lua, fn tool, lua ->
      server = tool |> Map.get("server") |> to_string_or_nil()
      name = tool |> Map.get("name") |> to_string_or_nil()

      if server == nil or name == nil do
        lua
      else
        bridge = tool_fun(server, name, call_fun, cfg)
        flat = Lua.set!(lua, ["tools", "#{server}__#{name}"], bridge)

        if sanitize(server) in @reserved_globals do
          flat
        else
          Lua.set!(flat, [sanitize(server), sanitize(name)], bridge)
        end
      end
    end)
  end

  defp tool_fun(server, name, call_fun, cfg) do
    fn args, lua ->
      with {:ok, arg_map} <- to_args(Lua.decode_list!(lua, args)) do
        case call_tool(call_fun, server, name, arg_map, cfg) do
          {:ok, result} ->
            {encoded, lua} = Lua.encode!(lua, result)
            {encoded, lua}

          {:error, message} ->
            {:error, "tool #{server}.#{name} failed: #{message}", lua}
        end
      else
        {:error, message} ->
          {:error, "tool #{server}.#{name} failed: #{message}", lua}
      end
    end
  end

  # Single funnel for dispatching one tool call and classifying its outcome.
  # Returns `{:ok, result}` or `{:error, message}`; `parallel_fun/3` reuses it.
  defp call_tool(call_fun, server, tool, args, cfg) do
    case call_fun.(server, tool, args) do
      {:ok, result} ->
        if tool_error?(result) and raise_on_tool_error?(cfg) do
          {:error, tool_error_text(result)}
        else
          {:ok, result}
        end

      {:error, reason} ->
        {:error, format_reason(reason)}

      other ->
        {:error, "unexpected #{inspect(other)}"}
    end
  end

  # --- parallel / parallel_all ---

  defp parallel_fun(call_fun, cfg, mode) do
    fn args, lua ->
      descriptors = args |> then(&Lua.decode_list!(lua, &1)) |> parallel_args()
      results = run_parallel(descriptors, call_fun, cfg)

      case mode do
        :strict -> render_strict(results, lua)
        :soft -> render_soft(results, lua)
      end
    end
  end

  defp run_parallel(descriptors, call_fun, cfg) do
    max = cfg[:max_concurrency] || 8

    descriptors
    |> Task.async_stream(
      fn
        %{server: server, tool: tool, args: args} when is_binary(server) and is_binary(tool) ->
          call_tool(call_fun, server, tool, args, cfg)

        bad ->
          {:error, "invalid descriptor #{inspect(bad)}"}
      end,
      ordered: true,
      max_concurrency: max,
      timeout: :infinity
    )
    |> Enum.map(fn
      {:ok, result} -> result
      {:exit, reason} -> {:error, format_reason(reason)}
    end)
  end

  defp render_strict(results, lua) do
    case results |> Enum.with_index(1) |> Enum.find(fn {r, _i} -> match?({:error, _}, r) end) do
      {{:error, message}, index} ->
        {:error, "parallel call #{index} failed: #{message}", lua}

      nil ->
        values = Enum.map(results, fn {:ok, value} -> value end)
        Lua.encode!(lua, values)
    end
  end

  defp render_soft(results, lua) do
    payload =
      Enum.map(results, fn
        {:ok, value} -> %{"ok" => true, "result" => value}
        {:error, message} -> %{"ok" => false, "error" => message}
      end)

    Lua.encode!(lua, payload)
  end

  defp parallel_args([first | _rest]) do
    case lua_to_elixir(first) do
      list when is_list(list) -> Enum.map(list, &descriptor/1)
      _ -> []
    end
  end

  defp parallel_args(_), do: []

  defp descriptor(%{} = call) do
    args = normalize_args(Map.get(call, "args"))

    case {Map.get(call, "server"), Map.get(call, "tool"), Map.get(call, "name")} do
      {server, tool, _name} when is_binary(server) and is_binary(tool) ->
        %{server: server, tool: tool, args: args}

      {_server, _tool, name} when is_binary(name) ->
        case String.split(name, "__", parts: 2) do
          [server, tool] -> %{server: server, tool: tool, args: args}
          _ -> %{server: nil, tool: nil, args: args}
        end

      _ ->
        %{server: nil, tool: nil, args: args}
    end
  end

  defp descriptor(_), do: %{server: nil, tool: nil, args: %{}}

  defp normalize_args(nil), do: %{}
  defp normalize_args(args) when is_map(args), do: args
  defp normalize_args(_), do: %{}

  # --- MCP-level failure detection ---

  defp raise_on_tool_error?(cfg), do: cfg[:raise_on_tool_error] != false

  defp tool_error?(%{"isError" => true}), do: true
  defp tool_error?(%{isError: true}), do: true
  defp tool_error?(_), do: false

  defp tool_error_text(result) do
    case result do
      %{"content" => content} when is_list(content) ->
        text =
          content
          |> Enum.map(fn
            %{"text" => text} when is_binary(text) -> text
            other -> inspect(other)
          end)
          |> Enum.join("\n")

        if text == "", do: encode_fallback(result), else: text

      _ ->
        encode_fallback(result)
    end
  end

  defp encode_fallback(result) do
    case Jason.encode(result) do
      {:ok, json} -> json
      {:error, _} -> inspect(result, limit: :infinity)
    end
  end

  defp print_fun do
    fn args, lua ->
      line = args |> then(&Lua.decode_list!(lua, &1)) |> Enum.map_join("\t", &stringify/1)
      Process.put(:code_mode_log, Process.get(:code_mode_log, []) ++ [line])
      {[], lua}
    end
  end

  # --- Lua <-> Elixir conversion ---

  # MCP tools take a JSON object; use the first argument when it is one. A
  # table with positional keys (1..n) decodes to a list — reject it instead of
  # silently dropping it to `%{}`, since MCP arguments must be named objects.
  defp to_args([]), do: {:ok, %{}}

  defp to_args([first | _rest]) do
    case lua_to_elixir(first) do
      map when is_map(map) -> {:ok, map}
      list when is_list(list) -> {:error, "arguments must be a table with named keys"}
      _ -> {:ok, %{}}
    end
  end

  # A decoded Lua table is a list of `{key, value}` pairs (see Lua.VM.Value).
  # Array-shaped tables (keys 1..n) become lists, everything else a string-keyed
  # map — the same heuristic Lua uses to distinguish arrays from objects.
  defp lua_to_elixir(list) when is_list(list) do
    cond do
      list == [] -> %{}
      array_like?(list) -> Enum.map(list, fn {_key, value} -> lua_to_elixir(value) end)
      true -> Map.new(list, fn {key, value} -> {key_to_string(key), lua_to_elixir(value)} end)
    end
  end

  defp lua_to_elixir(map) when is_map(map) do
    Map.new(map, fn {key, value} -> {key_to_string(key), lua_to_elixir(value)} end)
  end

  defp lua_to_elixir(other), do: other

  defp array_like?(list) do
    list
    |> Enum.with_index(1)
    |> Enum.all?(fn {{key, _value}, index} -> key == index end)
  end

  defp key_to_string(key) when is_binary(key), do: key
  defp key_to_string(key), do: to_string(key)

  # --- Result formatting ---

  defp format_result(results, logs) do
    body =
      case results do
        [] -> "Execution completed with no return value."
        [single] -> render_value(single)
        many -> render_value(many)
      end

    case logs do
      [] -> body
      _ -> "print output:\n" <> Enum.join(logs, "\n") <> "\n\n" <> body
    end
  end

  defp render_value(value) do
    case lua_to_elixir(value) do
      value when is_binary(value) ->
        value

      nil ->
        "nil"

      value ->
        case Jason.encode(value, pretty: true) do
          {:ok, json} -> json
          {:error, _} -> inspect(value, pretty: true, limit: :infinity)
        end
    end
  end

  defp stringify(value) do
    case lua_to_elixir(value) do
      value when is_binary(value) -> value
      value -> inspect(value)
    end
  end

  # --- Limits & errors ---

  # Runs `fun` in a monitored (unlinked) process and returns its value, or
  # `{:timeout, ms}` / `{:exit, reason}`. The caller is never linked, so a
  # runaway evaluation or a heap kill cannot take it down.
  defp run_bounded(fun, timeout) do
    parent = self()
    ref = make_ref()

    {pid, monitor} = spawn_monitor(fn -> send(parent, {ref, fun.()}) end)

    receive do
      {^ref, result} ->
        Process.demonitor(monitor, [:flush])
        {:ok, result}

      {:DOWN, ^monitor, :process, ^pid, reason} ->
        {:exit, reason}
    after
      timeout ->
        Process.exit(pid, :kill)

        receive do
          {:DOWN, ^monitor, :process, ^pid, _reason} -> :ok
        after
          1_000 -> :ok
        end

        {:timeout, timeout}
    end
  end

  defp cap_heap(:infinity), do: :ok

  defp cap_heap(bytes) when is_integer(bytes) and bytes > 0 do
    words = div(bytes, :erlang.system_info(:wordsize))
    Process.flag(:max_heap_size, %{size: words, kill: true, error_logger: false})
    :ok
  end

  defp cap_heap(_), do: :ok

  defp truncate_result({:ok, text}, max), do: {:ok, truncate(text, max)}
  defp truncate_result({:error, text}, max), do: {:error, truncate(text, max)}
  defp truncate_result(other, _max), do: other

  defp truncate(text, max) when is_integer(max) and max > 0 and byte_size(text) > max do
    utf8_prefix(text, max) <> "\n... (truncated; #{byte_size(text)} bytes total)"
  end

  defp truncate(text, _max), do: text

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

  defp sanitize(name) do
    name
    |> to_string()
    |> String.replace(~r/[^A-Za-z0-9_]/, "_")
  end

  defp to_string_or_nil(nil), do: nil
  defp to_string_or_nil(""), do: nil
  defp to_string_or_nil(value) when is_binary(value), do: value
  defp to_string_or_nil(value), do: to_string(value)

  defp format_reason(reason) when is_binary(reason), do: reason
  defp format_reason(%{__exception__: true} = exception), do: Exception.message(exception)
  defp format_reason(%{message: message}) when is_binary(message), do: message
  defp format_reason(reason), do: inspect(reason)
end
