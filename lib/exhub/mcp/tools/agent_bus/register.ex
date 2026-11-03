defmodule Exhub.MCP.Tools.AgentBus.Register do
  @moduledoc """
  MCP Tool: agent_register

  Creates or updates the caller's participant record. Registration is an
  idempotent upsert: sending only `id` twice is harmless, and partial updates
  never clear fields you did not mention.
  """

  alias Exhub.Agent.Registry
  alias Exhub.MCP.Tools.AgentBus.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "agent_register"

  @impl true
  def description do
    """
    Create or update a participant record on the agent bus (idempotent upsert).

    Register before you message anyone: peers route by id, and `agent_list`
    shows your description and capabilities to agents that need to pick a
    collaborator. Registration creates a private home directory with a
    MEMORY.md you own.

    Omitted fields are left unchanged. `status` must be one of:
    starting, idle, working, blocked, waiting_input, done, error, offline.
    """
  end

  schema do
    field(:id, {:required, :string},
      description: "Stable participant id (lowercase, digits, '.', '_', '-')"
    )

    field(:name, :string, description: "Human-readable name (defaults to id)")

    field(:description, :string,
      description: "What this agent is for; used for routing decisions"
    )

    field(:capabilities, {:list, :string},
      description: "Short capability tags, e.g. [\"elixir\", \"tests\", \"code-review\"]"
    )

    field(:kind, {:enum, ["acp", "sagents", "external", "human"]},
      description: "Where the participant runs (default: acp)",
      default: "acp"
    )

    field(:status, {:enum, ~w(starting idle working blocked waiting_input done error offline)},
      description: "Current status (default: idle); detail explains it in <=200 chars"
    )

    field(:detail, :string, description: "One-line explanation of the status (max 200 chars)")
  end

  @impl true
  def execute(params, frame) do
    attrs =
      params
      |> Map.take([:id, :name, :description, :capabilities, :kind, :status, :detail])
      |> Map.new(fn {key, value} -> {to_string(key), value} end)

    case Registry.register(attrs) do
      {:ok, record} ->
        Helpers.ok(frame, %{
          agent: record,
          memory_path: Registry.memory_path(record.id),
          hint: "Call agent_identity to confirm, then agent_send / agent_inbox."
        })

      {:error, reason} ->
        Helpers.error(frame, Helpers.error_message(reason))
    end
  end
end
