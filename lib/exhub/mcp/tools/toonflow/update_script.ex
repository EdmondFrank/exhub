defmodule Exhub.MCP.Tools.Toonflow.UpdateScript do
  @moduledoc """
  MCP Tool: `toonflow_update_script` — edit a script as a new version.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Script

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_update_script"

  @impl true
  def description do
    """
    Store an edited script as a new version, keeping the previous one intact.
    Provide the full new `content` and the `script_id` it derives from.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:script_id, {:required, :string}, description: "Script id the edit derives from.")
    field(:content, {:required, :string}, description: "The full edited script content.")
    field(:note, :string, description: "Optional note describing the edit.")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:script_id, Helpers.opt(params, :script_id))
      |> Helpers.put_opt(:content, Helpers.opt(params, :content))
      |> Helpers.put_opt(:note, Helpers.opt(params, :note))

    case Script.update_script(project, opts) do
      {:ok, script} ->
        resp = Response.tool() |> Response.structured(Map.put(script, "success", true))
        {:reply, resp, frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
