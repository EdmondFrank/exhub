defmodule Exhub.MCP.LspServer do
  @moduledoc """
  MCP server for language-server intelligence, available at `/lsp/mcp`.

  Wraps `Exhub.LspBridge` — the same sessions and language-server processes
  Emacs uses — behind a workspace-agnostic tool surface: every tool takes a
  file path and resolves that file's own project root (git toplevel, then the
  server's build markers). There is no "current workspace" to be confined to,
  which is what makes these tools usable from an agent working anywhere on the
  machine (AiderDesk, a script, another editor).

  ## Tools

  ### Diagnostics
  - `lsp_diagnostics` — errors/warnings for one file or a batch, re-read from
                        disk. The compile/type check to run after an edit.
  - `lsp_ledger`      — everything language servers have reported since boot,
                        per file, including files nothing has open (proactive:
                        "what did I break that I have not looked at").
  - `lsp_status`      — running sessions, capabilities and the ledger summary;
                        `shutdown` stops a wedged session.

  ### Navigation & inspection
  - `lsp_definition`  — go-to-definition (`textDocument/definition`).
  - `lsp_references`  — type-aware find-references, workspace-wide.
  - `lsp_hover`       — inferred type/signature/doc comment at a position.
  - `lsp_symbols`     — a file's outline, or `workspace/symbol` search.
  - `lsp_code_actions`— quick fixes/refactors for a range (edits returned, not
                        applied — nothing is written to disk).

  Positions are 1-based (`1:1` is the first character) so they can be fed
  straight back from tool output into the next call.

  ## See Also
  - `Exhub.LspBridge.Headless` — root resolution, disk sync, raw requests
  - `Exhub.LspBridge` — the lsp-bridge port itself (`docs/modules/lsp-bridge.md`)
  - `docs/modules/lsp.md` — user-facing documentation
  """

  use Anubis.Server,
    name: "exhub-lsp-server",
    version: "1.0.0",
    capabilities: [:tools]

  # Diagnostics
  component(Exhub.MCP.Tools.Lsp.Diagnostics)
  component(Exhub.MCP.Tools.Lsp.Ledger)
  component(Exhub.MCP.Tools.Lsp.Status)

  # Navigation & inspection
  component(Exhub.MCP.Tools.Lsp.Definition)
  component(Exhub.MCP.Tools.Lsp.References)
  component(Exhub.MCP.Tools.Lsp.Hover)
  component(Exhub.MCP.Tools.Lsp.Symbols)
  component(Exhub.MCP.Tools.Lsp.CodeActions)

  @impl true
  def init(client_info, frame) do
    _ = client_info
    {:ok, frame}
  end

  @impl true
  def handle_request(request, frame) do
    Exhub.MCP.ServerHelpers.handle_request_with_filtered_tools(__MODULE__, request, frame)
  end
end
