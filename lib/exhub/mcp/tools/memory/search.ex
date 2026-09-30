defmodule Exhub.MCP.Tools.Memory.Search do
  @moduledoc "MCP Tool: memory_search — recall approved project memory."

  alias Exhub.Memory.Recall
  alias Exhub.MCP.Tools.Memory.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "memory_search"

  @impl true
  def description do
    """
    Search approved memory distilled from earlier sessions, before you start a
    non-trivial task or when a command fails in a way that may have a known fix.

    Query terms are ANDed, so use two or three distinctive keywords (a tool
    name, file, or error string) — drop terms if nothing matches. Results are
    ranked with the Brain pipeline and (by default) filtered for relevance by
    Smart Decide.

    Treat memory as lessons from earlier sessions, not policy: apply one only
    when its `applicability` matches, cite its `memory_id`, and when it
    conflicts with the user's instructions or the repository docs, follow those
    and say so.
    """
  end

  schema do
    field(:query, {:required, :string},
      description: "Two or three distinctive keywords (not a full sentence)"
    )

    field(:project, :string, description: "Scope recall to a project")
    field(:kind, :string, description: "Only this kind of memory")
    field(:limit, :integer, description: "Max results (default: 5)", default: 5)

    field(:filter, :boolean,
      description: "Run the Smart Decide relevance pass (default: config, on)"
    )
  end

  @impl true
  def execute(params, frame) do
    query = Map.get(params, :query)

    opts =
      [
        project: Map.get(params, :project),
        kind: Map.get(params, :kind),
        limit: Map.get(params, :limit),
        filter: Map.get(params, :filter)
      ]
      |> Enum.reject(fn {_k, v} -> is_nil(v) end)

    results = Recall.search(query, opts)
    Helpers.text(frame, Recall.format(results, query))
  end
end
