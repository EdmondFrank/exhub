defmodule Exhub.MCP.Tools.AgentBus.Send do
  @moduledoc """
  MCP Tool: agent_send

  Direct message to one participant. This is what removes the star topology:
  two subagents can talk without routing every word through the orchestrator.
  """

  alias Exhub.Agent.Bus
  alias Exhub.MCP.Tools.AgentBus.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "agent_send"

  @impl true
  def description do
    """
    Send a direct message to another participant.

    kind:
    - `ask`     — you need an answer; the peer should reply with `reply` and reply_to.
    - `tell`    — information only, no answer expected.
    - `reply`   — the answer to an `ask`; set `reply_to` to the ask's message id.
    - `handoff` — you are handing work over; include what changed, where it is,
                  what proves it, what is uncertain, and what to do next.

    The message is durable (appended to disk before this call returns) and is
    delivered when the peer next calls agent_inbox. Pass `idempotency_key` when
    retrying so a lost response cannot duplicate the message.
    """
  end

  schema do
    field(:to, {:required, :string}, description: "Recipient agent id")
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
        "to" => Map.get(params, :to),
        "body" => Map.get(params, :body),
        "kind" => Map.get(params, :kind, "tell"),
        "reply_to" => Map.get(params, :reply_to),
        "idempotency_key" => Map.get(params, :idempotency_key)
      }

      case Bus.send_message(attrs) do
        {:ok, message, status} ->
          Helpers.ok(frame, %{
            status: status,
            message: message,
            hint: "The peer reads this on its next agent_inbox call."
          })

        {:error, reason} ->
          Helpers.error(frame, Helpers.error_message(reason))
      end
    else
      {:error, reason} -> Helpers.error(frame, reason)
    end
  end
end
