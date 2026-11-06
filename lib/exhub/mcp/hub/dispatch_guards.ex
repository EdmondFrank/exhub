defmodule Exhub.MCP.Hub.DispatchGuards do
  @moduledoc """
  Pre-flight gates for built-in tool dispatch.

  A built-in tool runs through its `handler.execute/2`, so the server's
  `handle_request/2` callback — and any gate it runs there — is skipped. Two
  funnels reach a handler that way, and both have to be gated:

      hub `/mcp` route (`Exhub.MCP.Hub.ProxyPlug`)  → `BuiltInRegistry.call_tool/3`
      `code_mode`'s Lua sandbox (`Exhub.Lua.Engine`) → `ClientManager.call_tool/3`
                                                        → `BuiltInRegistry.call_tool/3`
      `ClientManager` built-in dispatch             → `BuiltInRegistry.call_tool/3`

      `/desktop/mcp` route (`Exhub.MCP.LazyPlug`)   → `ConcurrentToolDispatcher.maybe_handle/2`
                                                        → `handler.execute/2`

  Without a gate a `search_files` rooted at `~` — or a `cat ~/.env` — runs
  straight from a `code_mode` snippet or from a plain POST to `/desktop/mcp`,
  side-stepping the cost and secret policies entirely. `check/3` supplies that
  gate: `BuiltInRegistry.call_tool/3` and
  `ConcurrentToolDispatcher.handle_concurrent/9` both call it before anything
  executes.

  `Exhub.MCP.DesktopServer.handle_request/2` keeps its own `check_request/2`
  calls, but they only see requests that actually reach the Anubis session:
  `ConcurrentToolDispatcher` intercepts every POST `tools/call` in front of it.

  ## Selection

  Gates are attached per *server*, not per tool name: tool names repeat across
  servers (`read_file`, `search_files`, …), so a name list would also gate an
  unrelated server's tool. The mapping defaults to

      %{"desktop" => [Exhub.MCP.Desktop.SecretGuard, Exhub.MCP.Desktop.CostGuard]}

  and can be replaced under `:exhub, Exhub.MCP.Hub.DispatchGuards`:

      config :exhub, Exhub.MCP.Hub.DispatchGuards, servers: %{"desktop" => [...]}

  Guards run in the order listed and the first `{:refuse, message}` wins. The
  secret gate goes first: its deterministic fast path usually settles a
  secret-shaped command with no model call, and a leaked credential is the more
  damaging mistake. Each guard's own configuration (`:guarded_tools`,
  `:always_allow`, model, threshold, cache) still applies — this module only
  decides *when* a guard is consulted.

  Both guards cache verdicts per `{tool, arguments}`. No call is gated twice —
  the funnels are alternatives, not stages — so the cache only pays off across
  repeated calls.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.CostGuard
  alias Exhub.MCP.Desktop.SecretGuard
  alias Exhub.MCP.Hub.BuiltInRegistry

  @default_servers %{"desktop" => [SecretGuard, CostGuard]}

  @doc """
  The gate modules applied to each built-in server, keyed by server name.

  Configured with `config :exhub, Exhub.MCP.Hub.DispatchGuards, servers: %{…}`
  (a full replacement, so an empty map disables funnel gating).
  """
  @spec servers() :: %{String.t() => [module()]}
  def servers do
    case Application.get_env(:exhub, __MODULE__, []) do
      config when is_list(config) -> Keyword.get(config, :servers) || @default_servers
      config when is_map(config) -> Map.get(config, :servers) || @default_servers
      _ -> @default_servers
    end
  end

  @doc """
  Judges one built-in tool invocation *before* it executes.

  The first argument is either a built-in server's route name (`"desktop"`, the
  registry's key) or the server module itself (`Exhub.MCP.DesktopServer`, all a
  route-level caller such as `Exhub.MCP.ConcurrentToolDispatcher` holds); a module
  is resolved through `BuiltInRegistry.server_name/1`.

  Returns `:ok` when the call may run, or `{:refuse, message}` with a
  caller-facing explanation. Servers outside the mapping, a module the registry
  does not know, and any call whose tool name is not a binary, fail open.
  """
  @spec check(term(), term(), term()) :: :ok | {:refuse, String.t()}
  def check(server, tool_name, arguments) when is_binary(tool_name) do
    case route_name(server) do
      nil -> :ok
      server_name -> check_for(server_name, tool_name, arguments)
    end
  end

  def check(_server, _tool_name, _arguments), do: :ok

  defp check_for(server_name, tool_name, arguments) do
    servers()
    |> Map.get(server_name, [])
    |> Enum.reduce_while(:ok, fn guard, :ok ->
      case guard.check(tool_name, arguments) do
        :ok -> {:cont, :ok}
        {:refuse, message} -> {:halt, {:refuse, message}}
        # A guard answers `:ok | {:refuse, _}`; anything else is a bug in the
        # guard, and blocking real work over it would be worse than letting the
        # call through.
        _other -> {:cont, :ok}
      end
    end)
  end

  defp route_name(name) when is_binary(name), do: name
  defp route_name(module) when is_atom(module), do: BuiltInRegistry.server_name(module)
  defp route_name(_other), do: nil

  @doc """
  Wraps a refusal message as an MCP tool-error result.

  This is the shape `Exhub.MCP.DesktopServer.handle_request/2` replies with on
  the HTTP route (`Response.tool() |> Response.error/2`), so both paths surface
  the same `isError` payload — and `code_mode` turns it into a `ToolFailure`
  carrying the message rather than a silently-passed value.
  """
  @spec refuse_result(String.t()) :: map()
  def refuse_result(message) when is_binary(message) do
    Response.tool()
    |> Response.error(message)
    |> Response.to_protocol()
  end
end
