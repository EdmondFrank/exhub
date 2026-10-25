defmodule Exhub.MCP.Tools.Toonflow.ListVariations do
  @moduledoc """
  MCP Tool: `toonflow_list_variations` — a character's wardrobe variations.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Assets

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_list_variations"

  @impl true
  def description do
    """
    List wardrobe variations (name, visual prompt, reference images). Filter to
    one character with `character`. Create them with
    `toonflow_generate_variation` and select one per shot with
    `toonflow_set_shot_variations`.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:character, :string, description: "Filter to one character name.")
    field(:limit, :integer, description: "Maximum number of variations to return.")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:character, Helpers.opt(params, :character))
      |> Helpers.put_opt(:limit, Helpers.opt(params, :limit))

    case Assets.list_variations(project, opts) do
      {:ok, variations} ->
        summary = %{"count" => length(variations), "variations" => variations}
        {:reply, Response.tool() |> Response.structured(summary), frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
