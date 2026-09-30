defmodule Exhub.MCP.Tools.Memory.Candidates do
  @moduledoc "MCP Tool: memory_candidates — list the memory review queue."

  alias Exhub.Memory.Store
  alias Exhub.MCP.Tools.Memory.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "memory_candidates"

  @impl true
  def description do
    "List memory notes awaiting review (default status: candidate), optionally " <>
      "scoped by project or kind."
  end

  schema do
    field(:status, :string,
      description: "Lifecycle status to list (default: candidate; pass 'all' for any)",
      default: "candidate"
    )

    field(:project, :string, description: "Only memories scoped to this project")
    field(:kind, :string, description: "Only memories of this kind")

    field(:limit, :integer, description: "Max results (default: 20)", default: 20)
  end

  @impl true
  def execute(params, frame) do
    status = Map.get(params, :status, "candidate")
    status = if status in [nil, "all", ""], do: nil, else: status

    records =
      Store.list(
        status: status,
        project: Map.get(params, :project),
        kind: Map.get(params, :kind)
      )

    limit = Map.get(params, :limit, 20) || 20

    rows =
      records
      |> Enum.take(limit)
      |> Enum.map(fn record ->
        meta = record.meta

        %{
          "memory_id" => meta["memory_id"],
          "status" => meta["status"],
          "kind" => meta["kind"],
          "title" => meta["title"],
          "applicability" => meta["applicability"],
          "project" => meta["project"],
          "promoted" => get_in(meta, ["evaluation", "promoted"]),
          "file" => record.file
        }
      end)

    Helpers.json(frame, %{"count" => length(rows), "total" => length(records), "memories" => rows})
  end
end
