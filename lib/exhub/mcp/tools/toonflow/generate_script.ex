defmodule Exhub.MCP.Tools.Toonflow.GenerateScript do
  @moduledoc """
  MCP Tool: `toonflow_generate_script` — novel/chapter → short-drama script.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Script

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_generate_script"

  @impl true
  def description do
    """
    Adapt a chapter (or, with no `chapter_id`, the whole novel) into a
    short-drama script using the LLM and the chapter's extracted event graph.

    Each call stores a new immutable version. Use `instructions` to steer the
    adaptation, and `toonflow_update_script` to edit the result as a new version.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:chapter_id, :string, description: "Adapt a single chapter (default: the whole novel).")
    field(:instructions, :string, description: "Extra writing/directing guidance.")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:chapter_id, Helpers.opt(params, :chapter_id))
      |> Helpers.put_opt(:instructions, Helpers.opt(params, :instructions))

    case Script.generate_script(project, opts) do
      {:ok, script} ->
        resp = Response.tool() |> Response.structured(Map.put(script, "success", true))
        {:reply, resp, frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
