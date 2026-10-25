defmodule Exhub.MCP.Tools.Toonflow.PromptSet do
  @moduledoc """
  MCP Tool: `toonflow_prompt_set` — override a prompt template for one project.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Prompts

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_prompt_set"

  @impl true
  def description do
    """
    Save a new version of a project's prompt override — the *effective* template
    for that project from now on, until a newer version (or a revert) replaces it.

    Versions are immutable and numbered, so nothing is lost: inspect them with
    `toonflow_prompt_history` and restore one with `toonflow_prompt_revert`.
    Keep the `{{key}}` placeholders the shipped template uses (read it first with
    `toonflow_prompt_get`) or the stage will render with empty values.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")

    field(:name, {:required, :string}, description: "Template name, e.g. \"storyboard.system\".")

    field(:content, {:required, :string}, description: "The full template text.")
    field(:note, :string, description: "Why this revision (shown in the history).")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:name, Helpers.opt(params, :name))
      |> Helpers.put_opt(:content, Helpers.opt(params, :content))
      |> Helpers.put_opt(:note, Helpers.opt(params, :note))

    case Prompts.set(project, opts) do
      {:ok, override} ->
        summary = Map.put(override, "success", true)
        {:reply, Response.tool() |> Response.structured(summary), frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
