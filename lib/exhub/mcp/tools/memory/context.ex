defmodule Exhub.MCP.Tools.Memory.Context do
  @moduledoc "MCP Tool: memory_context — recall memory relevant to a task description."

  alias Exhub.Memory.Recall
  alias Exhub.MCP.Tools.Memory.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "memory_context"

  @impl true
  def description do
    """
    Retrieve up to a few approved memories relevant to the current task, to read
    once before planning a non-trivial change (Beacon's `get_memory_context`).

    Pass two or three distinctive keywords from the task (a tool, file, or error
    string) rather than a full sentence — terms are ANDed. If nothing comes back,
    carry on; do not report an empty result unless the user asked.
    """
  end

  schema do
    field(:task, {:required, :string},
      description: "Two or three distinctive keywords from the current task"
    )

    field(:project, :string, description: "Scope recall to a project")
    field(:limit, :integer, description: "Max memories (default: 5)", default: 5)
  end

  @impl true
  def execute(params, frame) do
    task = Map.get(params, :task)

    opts =
      [project: Map.get(params, :project), limit: Map.get(params, :limit)]
      |> Enum.reject(fn {_k, v} -> is_nil(v) end)

    results = Recall.search(task, opts)
    Helpers.text(frame, Recall.format(results, task))
  end
end
