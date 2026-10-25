defmodule Exhub.MCP.Tools.Toonflow.GenerateVoice do
  @moduledoc """
  MCP Tool: `toonflow_generate_voice` — synthesize speech for a shot (or text).
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.{Jobs, Voice}

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_generate_voice"

  @impl true
  def description do
    """
    Synthesize speech, saving it under `assets/audio/` and recording an asset row.
    By default this streams from MoArk's WebSocket TTS endpoint with
    `TeleTTS-Bidirectional-Mandarin` (`Exhub.TTS.Realtime`), which returns PCM
    and is saved as `.wav`; any non-`TeleTTS-*` model falls back to the Gitee AI
    synchronous `/v1/audio/speech` API (`Step-Audio-TTS-3B`). The saved file
    extension is corrected to the detected container (e.g. `.wav`).

    Pass `shot_id` to use the shot's dialogue (`meta.dialogue` / `meta.台词`, else
    the shot description), or `text` for an explicit line. `voice`, `model` and
    the clone params `prompt_audio_url` / `prompt_text` override the defaults.
    Returns the local `path` of the audio.

    Voice resolution: an explicit `voice` wins, else the shot's speaker
    (`meta.speaker` / `meta.说话人`, else its only character) selects a
    `media.voice_map` entry — a voice name, or a map with `voice` / `speech_rate`
    / `volume` / `sample_rate` — else `media.tts_voice`. A shot with no dialogue
    is skipped (`skipped: true`) unless `media.tts_narration` is on, so
    narration never gets a voiceover.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:shot_id, :string, description: "Use this shot's dialogue as the text.")

    field(:text, :string, description: "Explicit text to synthesize (alternative to shot_id).")

    field(:voice, :string, description: "Voice name (default: the configured media tts_voice).")

    field(:model, :string,
      description:
        "TTS model (default: the configured media tts_model, TeleTTS-Bidirectional-Mandarin)."
    )

    field(:prompt_audio_url, :string,
      description: "Reference audio URL for the clone models (IndexTTS-2, GLM-TTS)."
    )

    field(:prompt_text, :string, description: "Transcript of `prompt_audio_url` (optional).")

    field(:speaker, :string, description: "Alias for `voice` (legacy).")
    field(:output_format, :string, description: "Nominal format; corrected to the detected one.")
    field(:language, :string, description: "Optional language hint.")
    field(:instruction, :string, description: "Optional style instruction.")

    field(:speech_rate, :float,
      description:
        "Realtime (TeleTTS) models only: speech rate. Default: the shot's `media.voice_map` entry, else 1."
    )

    field(:volume, :integer,
      description:
        "Realtime (TeleTTS) models only: output volume. Default: the voice_map entry, else 50."
    )

    field(:sample_rate, :integer,
      description: "Realtime (TeleTTS) models only: output sample rate in Hz (default 24000)."
    )
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:shot_id, Helpers.opt(params, :shot_id))
      |> Helpers.put_opt(:text, Helpers.opt(params, :text))
      |> Helpers.put_opt(:voice, Helpers.opt(params, :voice))
      |> Helpers.put_opt(:speaker, Helpers.opt(params, :speaker))
      |> Helpers.put_opt(:model, Helpers.opt(params, :model))
      |> Helpers.put_opt(:prompt_audio_url, Helpers.opt(params, :prompt_audio_url))
      |> Helpers.put_opt(:prompt_text, Helpers.opt(params, :prompt_text))
      |> Helpers.put_opt(:output_format, Helpers.opt(params, :output_format))
      |> Helpers.put_opt(:language, Helpers.opt(params, :language))
      |> Helpers.put_opt(:instruction, Helpers.opt(params, :instruction))
      |> Helpers.put_opt(:speech_rate, Helpers.opt(params, :speech_rate))
      |> Helpers.put_opt(:volume, Helpers.opt(params, :volume))
      |> Helpers.put_opt(:sample_rate, Helpers.opt(params, :sample_rate))

    job_params = %{"shot_id" => Map.get(params, :shot_id)}

    case Jobs.run(project, "voice", job_params, fn -> Voice.generate_voice(project, opts) end) do
      {:ok, asset} ->
        {:reply, Response.tool() |> Response.structured(Map.put(asset, "success", true)), frame}

      {:skip, reason} ->
        {:reply,
         Response.tool()
         |> Response.structured(%{
           "success" => true,
           "skipped" => true,
           "reason" => to_string(reason)
         }), frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
