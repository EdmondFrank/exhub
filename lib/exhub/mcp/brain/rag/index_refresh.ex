defmodule Exhub.BrainIndexRefresh do
  @moduledoc """
  Daily index refresh for the Brain RAG vector index using Quantum scheduler.

  Periodically scans the Obsidian vault for `.md` files and calls
  `VectorIndex.rebuild/1` — the underlying change-detection (SHA-256
  content signatures) ensures only files whose content changed since the
  last rebuild are re-chunked and re-embedded, so each run is cheap.

  ## Schedule

  Configured under `:exhub -> Exhub.BrainIndexRefresh -> :jobs`:

      config :exhub, Exhub.BrainIndexRefresh,
        jobs: [
          daily: [schedule: "0 3 * * *", task: {Exhub.BrainIndexRefresh, :run_refresh, []}]
        ]
  """

  use Quantum, otp_app: :exhub

  require Logger

  alias Exhub.MCP.Brain.Helpers
  alias Exhub.MCP.Brain.RAG.VectorIndex

  @doc """
  Scans the vault and incrementally rebuilds the vector index.

  Called by the Quantum scheduler on the configured cron schedule.
  """
  def run_refresh do
    vault = Helpers.vault_path()

    files =
      vault
      |> Helpers.list_md_files(vault,
        gitignore_patterns: Helpers.load_gitignore_patterns(vault)
      )
      |> Enum.map(&Path.join(vault, &1))

    if files == [] do
      Logger.warning("[BrainIndexRefresh] No markdown files found in vault: #{vault}")
      {:error, :no_files}
    else
      Logger.info("[BrainIndexRefresh] Scanning #{length(files)} files in #{vault}")

      case VectorIndex.rebuild(files) do
        {:ok, stats} ->
          Logger.info("[BrainIndexRefresh] Index refreshed: #{inspect(stats)}")
          {:ok, stats}

        {:error, reason} ->
          Logger.error("[BrainIndexRefresh] Index refresh failed: #{reason}")
          {:error, reason}
      end
    end
  end
end