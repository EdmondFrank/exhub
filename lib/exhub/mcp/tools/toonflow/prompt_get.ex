defmodule Exhub.MCP.Tools.Toonflow.PromptGet do
  @moduledoc """
  MCP Tool: `toonflow_prompt_get` — the effective text of a prompt template.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Prompts

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_prompt_get"

  @impl true
  def description do
    """
    Read the effective text of a prompt template for a project — the project's
    override when one exists, else the shipped `priv/toonflow/prompts/<name>.md`
    — plus its `source` (`"override"` / `"shipped"`) and `version`.

    Template names: `events.system`, `events.user`, `script.system`,
    `script.user`, `assets.system`, `assets.user`, `storyboard.system`,
    `storyboard.user`. `{{key}}` placeholders stay unsubstituted.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")

    field(:name, {:required, :string}, description: "Template name, e.g. \"storyboard.system\".")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)
    name = Helpers.opt(params, :name)

    case Prompts.current(name, project) do
      {:ok, prompt} -> {:reply, Response.tool() |> Response.structured(prompt), frame}
      {:error, reason} -> Helpers.error(frame, reason)
    end
  end
end
