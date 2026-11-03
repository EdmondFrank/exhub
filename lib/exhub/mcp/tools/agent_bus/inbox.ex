defmodule Exhub.MCP.Tools.AgentBus.Inbox do
  @moduledoc """
  MCP Tool: agent_inbox

  Drains the caller's unread messages across its direct stream and joined
  rooms. Reading advances the read position; `peek: true` inspects without
  consuming.

  With `wait_ms > 0` the call blocks until a message arrives or the window
  expires, so an agent can wait for a peer without a polling loop.
  """

  alias Exhub.Agent.Bus
  alias Exhub.MCP.Tools.AgentBus.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "agent_inbox"

  @impl true
  def description do
    """
    Read unread messages addressed to you (direct) or to rooms you joined.

    Long-poll: set `wait_ms` to block until something arrives instead of
    calling this in a tight loop. `peek: true` leaves messages unread.

    Each message carries `id`, `from`, `kind`, `body`, `seq` and `reply_to`.
    Answer an `ask` with agent_send kind `reply` and reply_to set to its id.
    """
  end

  schema do
    field(:as, :string, description: "Caller id; optional when the bus URL carries `?as=<id>`")

    field(:wait_ms, :integer,
      description: "Block up to this many ms when there is nothing unread (max 120000)",
      default: 0
    )

    field(:limit, :integer,
      description: "Max messages to return (default 50, max 200)",
      default: 50
    )

    field(:peek, :boolean, description: "Do not advance the read position", default: false)
  end

  @impl true
  def execute(params, frame) do
    with {:ok, id} <- Helpers.require_caller(params, frame) do
      opts = [
        wait_ms: Map.get(params, :wait_ms, 0),
        limit: Map.get(params, :limit, 50),
        peek: Map.get(params, :peek, false)
      ]

      case Bus.inbox(id, opts) do
        {:ok, payload} -> Helpers.ok(frame, payload)
        {:error, reason} -> Helpers.error(frame, Helpers.error_message(reason))
      end
    else
      {:error, reason} -> Helpers.error(frame, reason)
    end
  end
end
