defmodule Exhub.Lua.Sandbox.Lua do
  @moduledoc """
  Lua 5.3 sandbox implementation of `Exhub.Lua.Sandbox`, backed by the
  pure-Elixir [`lua`](https://hexdocs.pm/lua) VM. The VM cannot reach the BEAM
  except through the tool closures `Exhub.Lua.Bridge` registers.
  """

  @behaviour Exhub.Lua.Sandbox

  alias Exhub.Lua.Bridge
  alias Exhub.Lua.Diagnostics
  alias Exhub.Lua.Lint

  @impl Exhub.Lua.Sandbox
  def check(code, _tools) when not is_binary(code), do: {:error, "`code` must be a string"}
  def check(_code, tools) when not is_list(tools), do: {:error, "`tools` must be a list"}
  def check(_code, _tools), do: :ok

  @impl Exhub.Lua.Sandbox
  # Runs inside the sandbox process. Lua-level failures (bad syntax, a sandboxed
  # global, a tool raising) are returned as data rather than crashing the task.
  def execute(code, tools, call_fun, cfg) do
    {results, _lua} = eval!(code, tools, call_fun, cfg)
    {:ok, results, Process.get(:code_mode_log, [])}
  rescue
    error in [Lua.RuntimeException] ->
      lua_error(Diagnostics.classify(error, code))

    error in [Lua.CompilerException] ->
      compile_failure(error, code, tools, call_fun, cfg)
  end

  defp eval!(code, tools, call_fun, cfg) do
    lua = Bridge.build(tools, call_fun, cfg)
    Lua.eval!(lua, code, source: "code_mode")
  end

  # A compile failure is the one place a retry is safe: nothing ran, and the two
  # mistakes `Exhub.Lua.Lint` repairs (`{key: v}` table keys, backtick strings)
  # cannot be legal Lua, so the rewrite cannot change the meaning of code that
  # would otherwise have compiled. The rewrite is announced in the result so the
  # caller still learns the correct form.
  defp compile_failure(error, code, tools, call_fun, cfg) do
    message = Diagnostics.classify(error, code)

    with true <- cfg[:auto_repair] != false,
         {repaired, [_ | _] = changes} <- Lint.repair(code),
         true <- repaired != code do
      case retry(repaired, tools, call_fun, cfg) do
        {:ok, results, logs} ->
          {:ok, results, logs, Lint.repair_note(changes)}

        # The rewrite was correct but the snippet then failed at runtime: report
        # that (with the repair) rather than the stale syntax error.
        {:runtime, runtime_message} ->
          lua_error(Lint.repair_note(changes) <> "\n\n" <> runtime_message)

        :still_invalid ->
          lua_error(message)
      end
    else
      _ -> lua_error(message)
    end
  end

  defp retry(code, tools, call_fun, cfg) do
    {results, _lua} = eval!(code, tools, call_fun, cfg)
    {:ok, results, Process.get(:code_mode_log, [])}
  rescue
    error in [Lua.RuntimeException] -> {:runtime, Diagnostics.classify(error, code)}
    _error in [Lua.CompilerException] -> :still_invalid
  end

  defp lua_error(message) do
    # A strict `parallel` batch that aborts with at least one settled success
    # stashes its full report before raising (see `Bridge.render_strict/3`).
    # Surface that report as an ordinary result rather than an error, so one bad
    # call cannot replace the successes with a failure. `String.contains?` (not
    # `=~`, which would treat the report as a regex) keeps a script that
    # swallowed the parallel error via `pcall` and then raised something else on
    # the normal error path — its new message won't embed the report.
    case Process.get(:code_mode_partial) do
      report when is_binary(report) ->
        if String.contains?(message, report),
          do: {:partial, report, Process.get(:code_mode_log, [])},
          else: {:lua_error, message}

      _ ->
        {:lua_error, message}
    end
  end

  @impl Exhub.Lua.Sandbox
  def binding_names, do: Bridge.reserved_globals()

  @impl Exhub.Lua.Sandbox
  def prompt_info do
    %{
      language: "Lua 5.3",
      runtime: "pure-Elixir `lua` VM; no BEAM access beyond the bridged tools",
      constraints:
        "ASCII source only — build non-ASCII text from data (string.char) and " <>
          "slice it with utf8.offset",
      tool_usage:
        "call a tool as `server.tool({named = args})` or " <>
          "`tools[\"server__tool\"]({...})`; `parallel` / `parallel_all` fan out"
    }
  end
end
