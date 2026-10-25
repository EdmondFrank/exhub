defmodule Exhub.Toonflow.Voice do
  @moduledoc """
  Voice (TTS) generation for Toonflow (Phase 3).

  `generate_voice/3` resolves dialogue for a shot (from the shot's
  `meta[\"dialogue\"]` / `meta[\"台词\"]`, or an explicit
  `text`), calls the configured `Exhub.Toonflow.Voice.Client`, saves the audio
  under the project's `assets/audio/`, and records an `assets` row of kind
  `audio`.

  ## Per-character voices

  The voice is chosen from the shot's speaker — its explicit `meta["speaker"]`
  (e.g. `漩涡鸣人`), else its only character — via `media.voice_map`, where a
  value is a voice name or a `%{"voice" =>, "speech_rate" =>, "volume" =>,
  "sample_rate" =>}` map (handy to tell apart several characters sharing one
  timbre). Explicit `:voice` / `:speaker` / `:speech_rate` / `:volume` options
  always win, and the configured `media.tts_voice` is the final fallback.

  ## Narration

  A shot with no dialogue is **skipped** (`{:skip, :no_dialogue}`) so narration
  never gets a voiceover — unless `media.tts_narration` is true, in which case
  its `shot_desc` is voiced as before.

  The client is injectable — `Application.put_env(:exhub, :toonflow_voice_client,
  Mod)` — so tests never hit the network. The default
  (`Exhub.Toonflow.Voice.Default`) routes by model: the MoArk/TeleTTS
  WebSocket models stream PCM (`Exhub.TTS.Realtime`, saved as WAV), everything
  else uses the Gitee AI synchronous speech endpoint via `Exhub.TTS.Sync`
  (`Step-Audio-TTS-3B`).
  """

  alias Exhub.Toonflow
  alias Exhub.Toonflow.{Config, Media, Store, Storyboard}

  @default_model Exhub.TTS.Realtime.default_model()
  @default_format "mp3"

  @doc "The configured voice client implementation module."
  @spec voice_client() :: module()
  def voice_client,
    do: Application.get_env(:exhub, :toonflow_voice_client, Exhub.Toonflow.Voice.Default)

  @doc """
  Generate a voice clip.

  `opts`: `:shot_id` (resolve the dialogue from the shot), `:text` (explicit,
  used as-is), `:voice`, `:speaker`, `:model`, `:output_format`, `:language`,
  `:instruction`, `:volume`, `:speech_rate`, `:sample_rate`. Returns
  `{:ok, asset}`, `{:skip, :no_dialogue}` (nothing to voice), or
  `{:error, reason}`.
  """
  @spec generate_voice(String.t(), keyword(), GenServer.server()) ::
          {:ok, map()} | {:skip, atom()} | {:error, term()}
  def generate_voice(project, opts \\ [], server \\ Store) do
    shot_id = Toonflow.blank(Keyword.get(opts, :shot_id))
    text = Toonflow.blank(Keyword.get(opts, :text))

    with {:ok, meta} <- Store.get_project(project, server),
         {:ok, shot} <- load_shot(project, shot_id, server),
         {:ok, final_text} <- resolve_text(text, shot) do
      model = Keyword.get(opts, :model) || media_config()["tts_model"] || @default_model
      format = Keyword.get(opts, :output_format) || default_format(model)

      # The shot's speaker (explicit `meta.speaker`, else its only character)
      # selects a `media.voice_map` entry; explicit opts always win.
      speaker = speaker_of(shot)
      spec = Config.voice_spec(Config.voice_map(), speaker)

      voice =
        Keyword.get(opts, :voice) || Keyword.get(opts, :speaker) || spec["voice"] ||
          media_config()["tts_voice"]

      key = shot_id || Toonflow.new_id("vox")
      out_path = voice_path(meta["root_dir"], key, format)

      client_opts =
        [model: model, output_format: format, out_path: out_path]
        |> put_opt(:voice, voice)
        |> put_opt(:speaker, Keyword.get(opts, :speaker))
        |> put_opt(:language, Keyword.get(opts, :language))
        |> put_opt(:instruction, Keyword.get(opts, :instruction))
        |> put_opt(:prompt_audio_url, Keyword.get(opts, :prompt_audio_url))
        |> put_opt(:prompt_text, Keyword.get(opts, :prompt_text))
        |> put_opt(:volume, Keyword.get(opts, :volume) || spec["volume"])
        |> put_opt(:speech_rate, Keyword.get(opts, :speech_rate) || spec["speech_rate"])
        |> put_opt(:sample_rate, Keyword.get(opts, :sample_rate) || spec["sample_rate"])

      case voice_client().generate_voice(final_text, client_opts) do
        {:ok, result} -> record(project, shot_id, speaker, final_text, result, server)
        {:error, reason} -> {:error, {:voice_failed, reason}}
      end
    end
  end

  @doc "Local output path for a generated voice clip (pure)."
  @spec voice_path(String.t(), String.t(), String.t() | nil) :: String.t()
  def voice_path(project_dir, key, format) do
    ext = if format == "wav", do: "wav", else: @default_format
    Path.join([project_dir, "assets", "audio", Toonflow.file_key(key) <> "." <> ext])
  end

  # --- text resolution ---

  defp load_shot(_project, nil, _server), do: {:ok, nil}

  defp load_shot(project, shot_id, server), do: Storyboard.get_shot(project, shot_id, server)

  defp resolve_text(text, _shot) when is_binary(text), do: {:ok, text}
  defp resolve_text(_text, nil), do: {:error, :missing_text}

  defp resolve_text(_text, shot) do
    meta = shot["meta"] || %{}

    case first_blank([meta["dialogue"], meta["台词"]]) do
      nil -> narration_text(shot)
      text -> {:ok, text}
    end
  end

  # A shot with no dialogue is only voiced (from its description) when
  # `media.tts_narration` is on; otherwise it is skipped so narration never
  # gets a voiceover.
  defp narration_text(shot) do
    if Config.tts_narration?() do
      case first_blank([shot["shot_desc"]]) do
        nil -> {:skip, :no_dialogue}
        desc -> {:ok, desc}
      end
    else
      {:skip, :no_dialogue}
    end
  end

  # --- speaker resolution ---

  defp speaker_of(nil), do: nil

  defp speaker_of(shot) do
    meta = shot["meta"] || %{}
    first_blank([meta["speaker"], meta["说话人"]]) || only_character(shot)
  end

  defp only_character(shot) do
    case Storyboard.characters_of(shot) do
      [name] -> name
      _ -> nil
    end
  end

  defp first_blank(values), do: Enum.find(values, &(is_binary(&1) and String.trim(&1) != ""))

  # --- persistence ---

  defp record(project, shot_id, speaker, text, result, server) do
    meta =
      result
      |> Map.take([
        "model",
        "voice",
        "speaker",
        "segments",
        "output_format",
        "speech_rate",
        "volume"
      ])
      |> put_speaker(speaker)

    Media.insert_asset(
      project,
      [
        shot_id: shot_id,
        kind: "audio",
        path: result["path"],
        url: result["url"],
        prompt: text,
        meta: meta
      ],
      server
    )
  end

  defp put_speaker(meta, nil), do: meta

  defp put_speaker(meta, speaker) do
    case meta["speaker"] do
      kept when is_binary(kept) and kept != "" -> meta
      _ -> Map.put(meta, "speaker", speaker)
    end
  end

  defp put_opt(opts, _key, nil), do: opts
  defp put_opt(opts, key, value), do: Keyword.put(opts, key, value)

  # TeleTTS (WebSocket) clips are WAV/PCM; the sync models return MP3.
  defp default_format(model) do
    if Exhub.TTS.Realtime.model?(model), do: "wav", else: @default_format
  end

  defp media_config, do: Config.media()
end

defmodule Exhub.Toonflow.Voice.Client do
  @moduledoc "Behaviour for Toonflow voice (TTS) generation backends."

  @callback generate_voice(text :: String.t(), opts :: keyword()) ::
              {:ok, map()} | {:error, term()}
end

defmodule Exhub.Toonflow.Voice.Default do
  @moduledoc """
  Default voice client — routes by model.

    * the MoArk/TeleTTS WebSocket models (`TeleTTS-*`) stream PCM via
      `Exhub.TTS.Realtime` and are saved as WAV;
    * everything else uses `Exhub.TTS.Sync` against the OpenAI-compatible
      `/v1/audio/speech` endpoint (`Step-Audio-TTS-3B` by default).

  The API key is read from `:exhub, :giteeai_api_key`. The `:out_path`
  extension is corrected to the detected container (CosyVoice2 advertises
  `audio/mp3` but emits RIFF/WAVE bytes, and the WebSocket backend returns raw
  PCM wrapped as WAV).
  """

  @behaviour Exhub.Toonflow.Voice.Client

  alias Exhub.TTS.{Realtime, Sync}

  @impl true
  def generate_voice(text, opts) do
    model = Keyword.get(opts, :model) || Sync.default_model()

    if Realtime.model?(model) do
      realtime(text, model, opts)
    else
      sync(text, model, opts)
    end
  end

  defp realtime(text, model, opts) do
    rt_opts =
      [
        model: model,
        voice: Keyword.get(opts, :voice),
        volume: Keyword.get(opts, :volume),
        speech_rate: Keyword.get(opts, :speech_rate),
        sample_rate: Keyword.get(opts, :sample_rate)
      ]
      |> reject_nil()

    case Realtime.synthesize(text, rt_opts) do
      {:ok, audio} -> save(opts, audio)
      {:error, reason} -> {:error, reason}
    end
  end

  defp sync(text, model, opts) do
    sync_opts =
      [
        model: model,
        voice: Keyword.get(opts, :voice),
        prompt_audio_url: Keyword.get(opts, :prompt_audio_url),
        prompt_text: Keyword.get(opts, :prompt_text)
      ]
      |> reject_nil()

    case Sync.synthesize(text, sync_opts) do
      {:ok, audio} -> save(opts, audio)
      {:error, reason} -> {:error, reason}
    end
  end

  defp save(opts, audio) do
    path = Sync.with_format(Keyword.fetch!(opts, :out_path), audio.format)

    with :ok <- File.mkdir_p(Path.dirname(path)),
         :ok <- File.write(path, audio.body) do
      result =
        %{
          "path" => path,
          "url" => nil,
          "model" => audio.model,
          "voice" => audio.voice,
          "output_format" => audio.format,
          "segments" => 1
        }
        |> maybe_put("speech_rate", Keyword.get(opts, :speech_rate))
        |> maybe_put("volume", Keyword.get(opts, :volume))

      {:ok, result}
    end
  end

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)

  defp reject_nil(opts), do: Enum.reject(opts, fn {_key, value} -> is_nil(value) end)
end
