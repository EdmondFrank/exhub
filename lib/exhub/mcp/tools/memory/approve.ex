defmodule Exhub.MCP.Tools.Memory.Approve do
  @moduledoc "MCP Tool: memory_approve — approve a candidate into project memory."

  alias Exhub.Memory.Review
  alias Exhub.MCP.Tools.Memory.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "memory_approve"

  @impl true
  def description do
    """
    Approve a memory candidate into reusable project memory. This is the only
    path to `status: approved`, and it is never automatic — approve only what a
    person has reviewed.

    A body is required (approving the placeholder is refused). Pass `body` (and
    optionally `title`/`kind`/`applicability`/`tags`) to record the reviewed
    lesson text, which supersedes any draft stored on the candidate. Nothing
    that looks like a credential may be approved.
    """
  end

  schema do
    field(:memory_id, {:required, :string}, description: "The candidate memory id")
    field(:body, :string, description: "Reviewed lesson body to store with the approval")
    field(:title, :string, description: "Reviewed lesson title")
    field(:kind, :string, description: "Reviewed kind")
    field(:applicability, :string, description: "Reviewed applicability")
    field(:project, :string, description: "Project scope")
    field(:tags, :any, description: "Tags (list or comma-separated string)")
    field(:reason, :string, description: "Review note")
  end

  @impl true
  def execute(params, frame) do
    opts =
      [
        body: Map.get(params, :body),
        title: Map.get(params, :title),
        kind: Map.get(params, :kind),
        applicability: Map.get(params, :applicability),
        project: Map.get(params, :project),
        tags: Map.get(params, :tags) && Helpers.str_list(Map.get(params, :tags)),
        reason: Map.get(params, :reason)
      ]
      |> Enum.reject(fn {_k, v} -> is_nil(v) end)

    case Review.approve(Map.get(params, :memory_id), opts) do
      {:ok, meta} ->
        Helpers.json(frame, %{"memory_id" => meta["memory_id"], "status" => meta["status"]})

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
