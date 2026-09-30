defmodule Exhub.MCP.Tools.Toonflow.Export do
  @moduledoc """
  MCP Tool: `toonflow_export` — assemble and package an episode.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Assemble, as: Assembler

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_export"

  @impl true
  def description do
    """
    Assemble a script's clips and package the result as a named episode under
    `output/`, returning a manifest (video, subtitle and output-dir paths plus a
    count of project assets).

    Runs the same assembly as `toonflow_assemble`; `filename` sets the final
    video name (a `.mp4` extension is added if missing), defaulting to the script
    id. `scene` limits the assembly to one scene; `subtitles` soft-muxes the
    `.srt` (default from `assembly.subtitles`).
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:script_id, :string, description: "Script to export (default: the latest).")
    field(:scene, :string, description: "Only export shots from this scene.")
    field(:filename, :string, description: "Output video name (default: the script id).")

    field(:subtitles, :boolean,
      description: "Soft-mux the generated .srt into the video (default: configured)."
    )

    field(:mix_audio, :boolean, description: "Mix per-shot voiceover audio (default: false).")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:script_id, Helpers.opt(params, :script_id))
      |> Helpers.put_opt(:scene, Helpers.opt(params, :scene))
      |> Helpers.put_opt(:filename, Helpers.opt(params, :filename))
      |> Helpers.put_opt(:subtitles, Map.get(params, :subtitles))
      |> Helpers.put_opt(:mix_audio, Map.get(params, :mix_audio))

    case Assembler.export(project, opts) do
      {:ok, result} ->
        {:reply, Response.tool() |> Response.structured(Map.put(result, "success", true)), frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
