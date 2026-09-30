defmodule Exhub.MCP.Tools.Toonflow.ListCharacters do
  @moduledoc """
  MCP Tool: `toonflow_list_characters` — list the project's characters.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Assets

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_list_characters"

  @impl true
  def description do
    """
    List the characters extracted from a project's script(s): name, appearance,
    role, reference images and metadata. Populate with `toonflow_extract_assets`.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:name, :string, description: "Filter to a single character name.")
    field(:limit, :integer, description: "Maximum number of characters to return.")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:name, Helpers.opt(params, :name))
      |> Helpers.put_opt(:limit, Helpers.opt(params, :limit))

    case Assets.list_characters(project, opts) do
      {:ok, characters} ->
        summary = %{"count" => length(characters), "characters" => characters}
        {:reply, Response.tool() |> Response.structured(summary), frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
