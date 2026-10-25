defmodule Exhub.MCP.Tools.Toonflow.PromptRevert do
  @moduledoc """
  MCP Tool: `toonflow_prompt_revert` — restore an earlier prompt version.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Prompts

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_prompt_revert"

  @impl true
  def description do
    """
    Restore an earlier version of a project's prompt override.

    The old content is re-saved as a **new** version (noted `revert to vN`), so
    the history stays linear and the revert itself is auditable. Get the version
    numbers from `toonflow_prompt_history`.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:name, {:required, :string}, description: "Template name.")
    field(:version, {:required, :integer}, description: "The version to restore.")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:name, Helpers.opt(params, :name))
      |> Helpers.put_opt(:version, Map.get(params, :version))

    case Prompts.revert(project, opts) do
      {:ok, override} ->
        {:reply, Response.tool() |> Response.structured(Map.put(override, "success", true)),
         frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
