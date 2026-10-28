defmodule Exhub.Toonflow.Config do
  @moduledoc """
  Configuration for the `Exhub.Toonflow` subsystem.

  Reads the `:exhub, :toonflow` application environment — a string-keyed map,
  matching the convention used by `:brain_rag` and `:memory` — deep-merged over
  the defaults below. See `docs/modules/toonflow.md`.
  """

  @defaults %{
    "root_dir" => nil,
    "agents" => %{
      "script" => "deepseek-v4.1-flash",
      "director" => "deepseek-v4.1-flash",
      "qa" => "deepseek-v4.1-flash"
    },
    "llm" => %{
      # Receive timeout for Toonflow's LLM calls. GenClaw's 3-minute default is
      # short for a whole script or storyboard JSON, and the request also
      # traverses ExHub's upstream proxy.
      "receive_timeout" => 300_000
    },
    "media" => %{
      "image_model" => "qwen-image-2.0",
      "video_model" => "MiniMax-H3",
      # Delivery canvas. Keyframes and clips are generated at the same ratio, so
      # the image model composes for the frame the clip is cut into. A square
      # keyframe inside a 16:9 clip forces the video model to re-frame every
      # shot, which reads as subject-size jumps between cuts.
      "aspect_ratio" => "16:9",
      # Keyframe pixel size. `nil` derives it from `aspect_ratio`
      # (16:9 → 1536x1024, 9:16 → 1024x1536, 1:1 → 1024x1024).
      "image_size" => nil,
      # TTS default: MoArk's streaming WebSocket endpoint (`Exhub.TTS.Realtime`).
      # Verified live 2026-10-25: `TeleTTS-Bidirectional-Mandarin` honours the
      # full voice catalogue; `TeleTTS-MultiDialect` answered `TELEAI_WS_CLOSED`
      # (1002) and `TeleTTS-Mandarin` rejects `voice`. The Gitee AI sync models
      # (`Step-Audio-TTS-3B`, ...) remain available via `Exhub.TTS.Sync`.
      "tts_model" => "TeleTTS-Bidirectional-Mandarin",
      "tts_voice" => "guangzhounv",
      # Per-character voices: a shot's `meta.speaker` (or its only character)
      # selects an entry here. A value is either a voice name, or a map with
      # `voice` / `speech_rate` / `volume` / `sample_rate` for finer control —
      # e.g. several characters sharing the one male voice (`lucas`), told
      # apart by `speech_rate`.
      "voice_map" => %{},
      # A shot without dialogue (`meta.dialogue` / `meta.台词`) is *not* voiced
      # unless this is true, so narration never gets a voiceover.
      "tts_narration" => false,
      # Phase 7: start/end keyframes. `keyframes` enables the pipeline stage;
      # `last_frame` lets `fl2va` use a shot's end frame as the interpolation
      # target.
      "keyframes" => true,
      "last_frame" => true,
      # Phase 8.5: default cell count for a candidate grid (9 → 3×3).
      "candidates" => 9,
      # `videos` polls the MoArk async task for a result; a MiniMax-H3 clip can
      # take 8+ minutes, so the interval/budget are configurable here.
      "video_poll_interval_ms" => 10_000,
      "video_poll_attempts" => 110
    },
    "prompts" => %{
      # Per-project prompt overrides (Phase 8): when enabled, a project's latest
      # `prompt_overrides` row wins over the shipped `priv/toonflow/prompts` file.
      "overrides" => true
    },
    "memory" => %{
      # Semantic memory recall (Phase 4). The index reuses the Brain RAG
      # embedding stack, so the effective provider/model/dim come from
      # `:exhub -> :brain_rag`; these values mirror its defaults for reference.
      "enabled" => true,
      "index_path" => nil,
      "top_k" => 5,
      "scope" => "project",
      "batch_size" => 16,
      "rebuild_timeout" => 600_000,
      "search_timeout" => 60_000,
      "embedding_model" => "Qwen3-Embedding-4B",
      "dim" => 1024
    },
    "assembly" => %{
      "ffmpeg_path" => "ffmpeg",
      "ffprobe_path" => "ffprobe",
      "subtitles" => true,
      # Cross-dissolve between clips (0 = hard cuts). Keep it a whole number of
      # frames so the joins do not stretch the episode.
      "transition_seconds" => 0.125,
      # Gain on each clip's own audio, mixed under the voiceover as room tone.
      "ambience_gain" => 0.15
    },
    "ui" => %{
      # Canvas web view + live progress websocket (Phase 5).
      # `require_local` restricts the mutating REST endpoints (create/run) to
      # loopback clients, since the app is also reachable over the VPN.
      "enabled" => true,
      "tick_ms" => 5000,
      "require_local" => true
    }
  }

  @doc "The effective Toonflow configuration (defaults deep-merged with app env)."
  @spec config() :: map()
  def config do
    deep_merge(@defaults, normalize(Application.get_env(:exhub, :toonflow, %{})))
  end

  @doc "Fetch a top-level config value, with an optional default."
  @spec get(String.t(), term()) :: term()
  def get(key, default \\ nil), do: Map.get(config(), key, default)

  @doc """
  The workspace root directory.

  Falls back to `~/.config/exhub/toonflow` when `root_dir` is unset or blank.
  """
  @spec root_dir() :: String.t()
  def root_dir do
    case get("root_dir") do
      dir when is_binary(dir) and dir != "" -> Path.expand(dir)
      _ -> default_root_dir()
    end
  end

  @doc "The default workspace root directory."
  @spec default_root_dir() :: String.t()
  def default_root_dir do
    Path.join([System.user_home!(), ".config", "exhub", "toonflow"])
  end

  @doc "The merged `media` configuration."
  @spec media() :: map()
  def media, do: get("media", %{}) || %{}

  @doc "The delivery canvas (`media.aspect_ratio`), default `\"16:9\"`."
  @spec aspect_ratio() :: String.t()
  def aspect_ratio do
    case media() do
      %{"aspect_ratio" => value} when is_binary(value) and value != "" -> String.trim(value)
      _ -> "16:9"
    end
  end

  @doc """
  The configured keyframe pixel size (`media.image_size`), or `nil` to derive it
  from `aspect_ratio` (see `Exhub.Toonflow.Media.canvas_size/1`).
  """
  @spec image_size() :: String.t() | nil
  def image_size do
    case media() do
      %{"image_size" => value} when is_binary(value) and value != "" -> String.trim(value)
      _ -> nil
    end
  end

  @doc """
  Per-character voice specs (`media.voice_map`): character name → a voice name
  or a `%{"voice" =>, "speech_rate" =>, "volume" =>, "sample_rate" =>}` map.
  """
  @spec voice_map() :: map()
  def voice_map, do: media() |> Map.get("voice_map", %{}) |> normalize_voice_map()

  @doc "Whether shots without dialogue are voiced (`media.tts_narration`)."
  @spec tts_narration?() :: boolean()
  def tts_narration?, do: media()["tts_narration"] == true

  @doc "The normalized (map) voice spec for `speaker`, or `%{}` when absent."
  @spec voice_spec(map(), String.t() | nil) :: map()
  def voice_spec(voice_map, speaker) when is_map(voice_map) and is_binary(speaker) do
    case Map.get(voice_map, speaker) do
      spec when is_binary(spec) -> %{"voice" => spec}
      spec when is_map(spec) -> spec
      _ -> %{}
    end
  end

  def voice_spec(_voice_map, _speaker), do: %{}

  # --- helpers ---

  defp normalize_voice_map(map) when is_map(map) do
    Map.new(map, fn {name, spec} -> {to_string(name), normalize_spec(spec)} end)
  end

  defp normalize_voice_map(_), do: %{}

  defp normalize_spec(spec) when is_map(spec),
    do: Map.new(spec, fn {k, v} -> {to_string(k), v} end)

  defp normalize_spec(spec), do: spec

  defp normalize(cfg) when is_map(cfg), do: cfg
  defp normalize(_), do: %{}

  defp deep_merge(left, right) when is_map(left) and is_map(right) do
    Map.merge(left, right, fn _key, l, r ->
      if is_map(l) and is_map(r), do: deep_merge(l, r), else: r
    end)
  end
end
