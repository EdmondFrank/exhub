defmodule Exhub.MCP.Tools.Toonflow.Assemble do
  @moduledoc """
  MCP Tool: `toonflow_assemble` — concatenate a script's clips into an episode.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Assemble, as: Assembler

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_assemble"

  @impl true
  def description do
    """
    Assemble a script's storyboard clips (in order) into a single video under the
    project's `output/`, optionally mixing per-shot voiceover and writing an
    `.srt` subtitle track derived from each shot's dialogue.

    Every shot in the script must already have a generated clip
    (`toonflow_generate_video`). `subtitles` soft-muxes the `.srt` (default from
    `assembly.subtitles`); `mix_audio` mixes the shots' voice clips (default
    false). `scene` limits the assembly to one scene; `name` sets the artifact
    stem (default: the script id). Returns the video and subtitle paths.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:script_id, :string, description: "Script to assemble (default: the latest).")
    field(:scene, :string, description: "Only assemble shots from this scene.")

    field(:subtitles, :boolean,
      description: "Soft-mux the generated .srt into the video (default: configured)."
    )

    field(:mix_audio, :boolean, description: "Mix per-shot voiceover audio (default: false).")
    field(:name, :string, description: "Output file stem (default: the script id).")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:script_id, Helpers.opt(params, :script_id))
      |> Helpers.put_opt(:scene, Helpers.opt(params, :scene))
      |> Helpers.put_opt(:name, Helpers.opt(params, :name))
      |> Helpers.put_opt(:subtitles, Map.get(params, :subtitles))
      |> Helpers.put_opt(:mix_audio, Map.get(params, :mix_audio))

    case Assembler.assemble(project, opts) do
      {:ok, result} ->
        {:reply, Response.tool() |> Response.structured(Map.put(result, "success", true)), frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
