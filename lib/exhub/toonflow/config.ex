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
    "media" => %{
      "image_model" => "qwen-image-2.0",
      "video_model" => "MiniMax-H3",
      "tts_model" => "CosyVoice2",
      "tts_voice" => "alloy"
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
      "subtitles" => true
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

  # --- helpers ---

  defp normalize(cfg) when is_map(cfg), do: cfg
  defp normalize(_), do: %{}

  defp deep_merge(left, right) when is_map(left) and is_map(right) do
    Map.merge(left, right, fn _key, l, r ->
      if is_map(l) and is_map(r), do: deep_merge(l, r), else: r
    end)
  end
end
