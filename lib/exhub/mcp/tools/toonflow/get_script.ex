defmodule Exhub.MCP.Tools.Toonflow.GetScript do
  @moduledoc """
  MCP Tool: `toonflow_get_script` — fetch a stored script version.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Script

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_get_script"

  @impl true
  def description do
    """
    Fetch a script by `script_id`, or by `chapter_id` (+ optional `version`).
    With neither, the project's most recently created script is returned.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:script_id, :string, description: "Fetch an exact script version by id.")
    field(:chapter_id, :string, description: "Latest script for a chapter (or `version`).")
    field(:version, :integer, description: "Specific version number with `chapter_id`.")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:script_id, Helpers.opt(params, :script_id))
      |> Helpers.put_opt(:chapter_id, Helpers.opt(params, :chapter_id))
      |> Helpers.put_opt(:version, Helpers.opt(params, :version))

    case Script.get_script(project, opts) do
      {:ok, script} ->
        resp = Response.tool() |> Response.structured(script)
        {:reply, resp, frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
