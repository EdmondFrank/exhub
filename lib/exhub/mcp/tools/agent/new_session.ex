defmodule Exhub.MCP.Tools.Agent.NewSession do
  @moduledoc """
  MCP Tool: agent_new_session

  Creates an ACP session and, optionally, hands the agent MCP servers it can
  call itself. `expose_bus: true` injects the agent-bus endpoint pre-addressed
  to this agent, so a spawned subagent can message its peers without being
  driven through the orchestrator.
  """

  alias Anubis.Server.Response
  use Anubis.Server.Component, type: :tool

  def name, do: "agent_new_session"

  @impl true
  def description do
    """
    Create a new ACP session on an initialized agent. Returns the session ID.

    MCP servers:
    - `expose_bus: true` — give the agent the ExHub agent bus (identity,
      messaging, rooms), scoped to `/agent-bus/mcp?as=<agent_id>`. This is what
      lets subagents talk to each other instead of relaying through you.
    - `mcp_servers` — extra servers as `{name, uri}` (HTTP) entries, passed
      through verbatim to the ACP runtime.

    HTTP MCP support depends on the runtime; if an agent ignores the URL, pass
    a stdio bridge through `mcp_servers` instead.
    """
  end

  schema do
    field(:agent_id, {:required, :string}, description: "ID of the initialized agent")
    field(:cwd, :string, description: "Working directory for the session")

    field(:expose_bus, :boolean,
      description: "Inject the ExHub agent bus, addressed to this agent_id",
      default: false
    )

    field(:mcp_servers, {:list, %{name: :string, uri: {:required, :string}}},
      description: "Additional MCP servers: [{name, uri}]"
    )
  end

  @impl true
  def execute(params, frame) do
    agent_id = Map.get(params, :agent_id)
    cwd = Map.get(params, :cwd, File.cwd!())
    mcp_servers = build_mcp_servers(agent_id, params)

    case Exhub.MCP.Agent.Store.get(agent_id) do
      {:ok, entry} ->
        case ExMCP.ACP.Client.new_session(entry.client, cwd, mcp_servers: mcp_servers) do
          {:ok, result} ->
            session_id = Map.get(result, "sessionId")
            if session_id, do: Exhub.MCP.Agent.Store.add_session(agent_id, session_id)

            slim =
              result
              |> Map.take(["sessionId", "status", "cwd", "mode"])
              |> Map.put("mcp_servers", mcp_servers)

            resp = Response.tool() |> Response.text(Jason.encode!(slim))
            {:reply, resp, frame}

          {:error, reason} ->
            {:reply,
             Response.tool() |> Response.error("Failed to create session: #{inspect(reason)}"),
             frame}
        end

      {:error, :not_found} ->
        {:reply, Response.tool() |> Response.error("Agent '#{agent_id}' is not running."), frame}
    end
  end

  defp build_mcp_servers(agent_id, params) do
    extra =
      params
      |> Map.get(:mcp_servers, [])
      |> List.wrap()
      |> Enum.filter(&valid_server?/1)

    if Map.get(params, :expose_bus, false) do
      [bus_server(agent_id) | extra]
    else
      extra
    end
  end

  defp valid_server?(%{uri: uri}), do: is_binary(uri) and uri != ""
  defp valid_server?(%{"uri" => uri}), do: is_binary(uri) and uri != ""
  defp valid_server?(_), do: false

  defp bus_server(agent_id) do
    port = Application.get_env(:exhub, :port, 9069)

    %{
      "name" => "exhub-bus",
      "uri" => "http://127.0.0.1:#{port}/agent-bus/mcp?as=#{agent_id}"
    }
  end
end
