defmodule Exhub.Toonflow.Voice do
  @moduledoc """
  Voice (TTS) generation for Toonflow (Phase 3).

  `generate_voice/3` resolves dialogue for a shot (from the shot's
  `meta[\"dialogue\"]` / `meta[\"台词\"]`, else its description, or an explicit
  `text`), calls the configured `Exhub.Toonflow.Voice.Client`, saves the audio
  under the project's `assets/audio/`, and records an `assets` row of kind
  `audio`.

  The client is injectable — `Application.put_env(:exhub, :toonflow_voice_client,
  Mod)` — so tests never hit the network. The default
  (`Exhub.Toonflow.Voice.Default`) drives the Gitee AI synchronous speech
  endpoint (`CosyVoice2`) via `Exhub.TTS.Sync`.
  """

  alias Exhub.Toonflow
  alias Exhub.Toonflow.{Config, Media, Store, Storyboard}

  @default_model "CosyVoice2"
  @default_format "mp3"

  @doc "The configured voice client implementation module."
  @spec voice_client() :: module()
  def voice_client,
    do: Application.get_env(:exhub, :toonflow_voice_client, Exhub.Toonflow.Voice.Default)

  @doc """
  Generate a voice clip.

  `opts`: `:shot_id` (resolve the dialogue from the shot), `:text` (explicit,
  used as-is), `:model`, `:speaker`, `:output_format`, `:language`,
  `:instruction`. Returns `{:ok, asset}` or `{:error, reason}`.
  """
  @spec generate_voice(String.t(), keyword(), GenServer.server()) ::
          {:ok, map()} | {:error, term()}
  def generate_voice(project, opts \\ [], server \\ Store) do
    shot_id = Toonflow.blank(Keyword.get(opts, :shot_id))
    text = Toonflow.blank(Keyword.get(opts, :text))

    with {:ok, meta} <- Store.get_project(project, server),
         {:ok, shot} <- load_shot(project, shot_id, server),
         {:ok, final_text} <- resolve_text(text, shot) do
      format = Keyword.get(opts, :output_format) || @default_format
      model = Keyword.get(opts, :model) || media_config()["tts_model"] || @default_model

      voice =
        Keyword.get(opts, :voice) || Keyword.get(opts, :speaker) || media_config()["tts_voice"]

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

      case voice_client().generate_voice(final_text, client_opts) do
        {:ok, result} -> record(project, shot_id, final_text, result, server)
        {:error, reason} -> {:error, {:voice_failed, reason}}
      end
    end
  end

  @doc "Local output path for a generated voice clip (pure)."
  @spec voice_path(String.t(), String.t(), String.t() | nil) :: String.t()
  def voice_path(project_dir, key, format) do
    ext = if format == "wav", do: "wav", else: @default_format
    Path.join([project_dir, "assets", "audio", sanitize(key) <> "." <> ext])
  end

  # --- text resolution ---

  defp load_shot(_project, nil, _server), do: {:ok, nil}

  defp load_shot(project, shot_id, server), do: Storyboard.get_shot(project, shot_id, server)

  defp resolve_text(text, _shot) when is_binary(text), do: {:ok, text}
  defp resolve_text(_text, nil), do: {:error, :missing_text}

  defp resolve_text(_text, shot) do
    meta = shot["meta"] || %{}

    case first_blank([meta["dialogue"], meta["台词"], shot["shot_desc"]]) do
      nil -> {:error, :missing_text}
      text -> {:ok, text}
    end
  end

  defp first_blank(values), do: Enum.find(values, &(is_binary(&1) and String.trim(&1) != ""))

  # --- persistence ---

  defp record(project, shot_id, text, result, server) do
    Media.insert_asset(
      project,
      [
        shot_id: shot_id,
        kind: "audio",
        path: result["path"],
        url: result["url"],
        prompt: text,
        meta: Map.take(result, ["model", "voice", "speaker", "segments", "output_format"])
      ],
      server
    )
  end

  defp put_opt(opts, _key, nil), do: opts
  defp put_opt(opts, key, value), do: Keyword.put(opts, key, value)

  defp media_config, do: Config.get("media", %{}) || %{}
  defp sanitize(key), do: String.replace(to_string(key), ~r/[^A-Za-z0-9._-]/, "_")
end

defmodule Exhub.Toonflow.Voice.Client do
  @moduledoc "Behaviour for Toonflow voice (TTS) generation backends."

  @callback generate_voice(text :: String.t(), opts :: keyword()) ::
              {:ok, map()} | {:error, term()}
end

defmodule Exhub.Toonflow.Voice.Default do
  @moduledoc """
  Default voice client — Gitee AI synchronous speech (`CosyVoice2`).

  Uses `Exhub.TTS.Sync` against the OpenAI-compatible `/v1/audio/speech`
  endpoint; the API key is read from `:exhub, :giteeai_api_key`.

  The `:out_path` extension is corrected to the detected container: CosyVoice2
  advertises `audio/mp3` but emits RIFF/WAVE bytes, so the clip is saved with
  a `.wav` extension.
  """

  @behaviour Exhub.Toonflow.Voice.Client

  alias Exhub.TTS.Sync

  @impl true
  def generate_voice(text, opts) do
    out_path = Keyword.fetch!(opts, :out_path)

    sync_opts =
      [
        model: Keyword.get(opts, :model) || Sync.default_model(),
        voice: Keyword.get(opts, :voice),
        prompt_audio_url: Keyword.get(opts, :prompt_audio_url),
        prompt_text: Keyword.get(opts, :prompt_text)
      ]
      |> Enum.reject(fn {_key, value} -> is_nil(value) end)

    case Sync.synthesize(text, sync_opts) do
      {:ok, audio} ->
        path = Sync.with_format(out_path, audio.format)

        with :ok <- File.mkdir_p(Path.dirname(path)),
             :ok <- File.write(path, audio.body) do
          {:ok,
           %{
             "path" => path,
             "url" => nil,
             "model" => audio.model,
             "voice" => audio.voice,
             "output_format" => audio.format,
             "segments" => 1
           }}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end
end
