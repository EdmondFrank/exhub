defmodule Exhub.MCP.BrainServer do
  @moduledoc """
  MCP Server for Obsidian brain vault operations.

  ## Purpose
  Provides a comprehensive set of tools for interacting with an Obsidian vault
  as a "second brain". Designed for AI agents that need to read, write, search,
  and manage notes, tags, and directories within the vault.

  ## Configuration
  Set the vault path in your config:

      config :exhub, :obsidian_vault_path, "~/Documents/Obsidian/MyVault"

  Defaults to `~/Documents/Obsidian` if not configured.

  ## Tools
  ### Search & Navigation
  - `brain_search_vault`    — Search notes by content, filename, or tags
  - `brain_list_notes`      — List notes in the vault or a subfolder
  The server is accessible at `/brain/mcp`.
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
