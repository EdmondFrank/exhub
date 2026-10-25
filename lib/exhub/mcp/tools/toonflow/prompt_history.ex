defmodule Exhub.MCP.Tools.Toonflow.PromptHistory do
  @moduledoc """
  MCP Tool: `toonflow_prompt_history` — a prompt override's version history.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Prompts

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_prompt_history"

  @impl true
  def description do
    """
    List every saved version of a project's prompt override, newest first, with
    its note, timestamp and content. Use `toonflow_prompt_revert` to restore one.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:name, {:required, :string}, description: "Template name.")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)
    name = Helpers.opt(params, :name)

    case Prompts.history(name, project) do
      {:ok, versions} ->
        summary = %{"name" => name, "count" => length(versions), "versions" => versions}
        {:reply, Response.tool() |> Response.structured(summary), frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
