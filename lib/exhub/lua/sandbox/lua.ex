defmodule Exhub.Lua.Sandbox.Lua do
  @moduledoc """
  Lua 5.3 sandbox implementation of `Exhub.Lua.Sandbox`, backed by the
  pure-Elixir [`lua`](https://hexdocs.pm/lua) VM. The VM cannot reach the BEAM
  except through the tool closures `Exhub.Lua.Bridge` registers.
  """

  @behaviour Exhub.Lua.Sandbox

  alias Exhub.Lua.Bridge
  alias Exhub.Lua.Diagnostics

  @impl Exhub.Lua.Sandbox
  def check(code, _tools) when not is_binary(code), do: {:error, "`code` must be a string"}
  def check(_code, tools) when not is_list(tools), do: {:error, "`tools` must be a list"}
  def check(_code, _tools), do: :ok

  @impl Exhub.Lua.Sandbox
  # Runs inside the sandbox process. Lua-level failures (bad syntax, a sandboxed
  # global, a tool raising) are returned as data rather than crashing the task.
  def execute(code, tools, call_fun, cfg) do
    lua = Bridge.build(tools, call_fun, cfg)
    {results, _lua} = Lua.eval!(lua, code, source: "code_mode")
    {:ok, results, Process.get(:code_mode_log, [])}
  rescue
    error in [Lua.RuntimeException, Lua.CompilerException] ->
      message = Diagnostics.classify(error)

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
