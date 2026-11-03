defmodule Exhub.MCP.Tools.AgentBus.Post do
  @moduledoc "MCP Tool: agent_post — post to a shared room log."
  alias Exhub.Agent.Bus
  alias Exhub.MCP.Tools.AgentBus.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "agent_post"

  @impl true
  def description do
    """
    Post a message to a room. Creates the room and joins you if it does not exist.

    Use a room when several agents need the same information (a shared finding,
    a decision, a plan) — it beats N direct messages because every member reads
    it once, and nobody has to relay it through the orchestrator.
    """
  end

  schema do
    field(:room, {:required, :string}, description: "Room name")
    field(:body, {:required, :string}, description: "Message text (max 32 KB)")

    field(:kind, {:enum, ~w(ask tell reply handoff)},
      description: "Message intent (default: tell)",
      default: "tell"
    )

    field(:reply_to, :string, description: "Id of the message this answers")

    field(:idempotency_key, :string,
      description: "Retry-safe key; reuse returns the original message"
    )

    field(:as, :string, description: "Caller id; optional when the bus URL carries `?as=<id>`")
  end

  @impl true
  def execute(params, frame) do
    with {:ok, from} <- Helpers.require_caller(params, frame) do
      attrs = %{
        "from" => from,
        "room" => Map.get(params, :room),
        "body" => Map.get(params, :body),
        "kind" => Map.get(params, :kind, "tell"),
        "reply_to" => Map.get(params, :reply_to),
        "idempotency_key" => Map.get(params, :idempotency_key)
      }

      case Bus.post(attrs) do
        {:ok, message, status} -> Helpers.ok(frame, %{status: status, message: message})
        {:error, reason} -> Helpers.error(frame, Helpers.error_message(reason))
      end
    else
      {:error, reason} -> Helpers.error(frame, reason)
    end
  end
end
