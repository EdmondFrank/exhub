defmodule Exhub.MCP.BrainServer do
  @moduledoc """
  MCP Server for Obsidian brain vault operations.

  ## Purpose
  Provides tools for interacting with an Obsidian vault as a "second brain".
  Designed for AI agents that need to search and navigate notes and directories
  within the vault.

  ## Configuration
  Set the vault path in your config:

      config :exhub, :obsidian_vault_path, "~/Documents/Obsidian/MyVault"

  Defaults to `~/Documents/Obsidian` if not configured.
  Both `~` and `~/...` paths are expanded automatically.

  ## Tools

  ### Search & Navigation
  - `brain_list_notes`   — List notes and directories in the vault or a subfolder.
                           Supports recursive/flat mode, directory entries (trailing `/`),
                           and absolute path output (`abs_path: true`).
  - `brain_search_vault` — Search notes by content, filename, or tags.
                           Supports `content` / `filename` / `both` search types,
                           hierarchical tag search (`tag:` prefix), scoped search,
                           case-sensitive mode, and absolute path output.

  All responses include the vault root path (`Vault: <path>`) for caller orientation.

  The server is accessible at `/brain/mcp`.

  ## See Also
  - `Exhub.MCP.Brain.Helpers` — shared vault path resolution and file utilities
  - `docs/modules/brain.md`   — full user-facing documentation
  """

  use Anubis.Server,
    name: "exhub-brain-server",
    version: "1.0.0",
    capabilities: [:tools]

  # Search & navigation
  component Exhub.MCP.Tools.Brain.SearchVault
  component Exhub.MCP.Tools.Brain.ListNotes

  @impl true
  def init(client_info, frame) do
    _ = client_info
    {:ok, frame}
  end
end
