defmodule Exhub.MCP.Tools.Memory.Supersede do
  @moduledoc "MCP Tool: memory_supersede — replace a memory with a newer one."

  alias Exhub.Memory.Review
  alias Exhub.MCP.Tools.Memory.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "memory_supersede"

  @impl true
  def description do
    "Mark an existing memory superseded by a newer, approved memory. The old " <>
      "note is kept and linked (`superseded_by`), so the review history shows " <>
      "what changed rather than losing it."
  end

  schema do
    field(:memory_id, {:required, :string}, description: "The memory being replaced")
    field(:replacement_id, {:required, :string}, description: "The replacement memory id")
    field(:reason, :string, description: "Which memory already covers it / why")
  end

  @impl true
  def execute(params, frame) do
    case Review.supersede(
           Map.get(params, :memory_id),
           Map.get(params, :replacement_id),
           Map.get(params, :reason)
         ) do
      {:ok, result} -> Helpers.json(frame, result)
      {:error, reason} -> Helpers.error(frame, reason)
    end
  end
end
