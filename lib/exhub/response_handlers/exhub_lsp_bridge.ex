defmodule Exhub.ResponseHandlers.ExhubLspBridge do
  @moduledoc """
  WebSocket response handler for `lsp-bridge` commands from Emacs.

  Dispatches `["func", ["lsp-bridge", action, ...args]]` messages to
  `Exhub.LspBridge.ClientManager`, which manages language-server processes and
  pushes results back to Emacs over the WebSocket — the exhub-pattern port of
  lsp-bridge's EPC RPC surface.

  ## Supported actions (P1)

  - `"ping"` — liveness probe, replies `(exhub-lsp-pong)`
  - `"open-file"` — `path`, `content`, `opts` — resolve project/server, `didOpen`
  - `"change-file"` — `path`, `change` — `didChange` (+ pull diagnostics)
  - `"save-file"` / `"close-file"` — `path` — `didSave` / `didClose`
  - `"change-cursor"` — `path`, `position` — record cursor time
  - `"request"` / `"notify"` — `path`, `method`, `params` — raw JSON-RPC routed
    to the buffer's server(s)
  - `"diagnostics"` / `"list-diagnostics"` — `path`, `opts` — merged diagnostics
  - `"shutdown"` — `path` — stop the session and its servers
  - `"start-server"` / `"stop-server"` — `name`, `project_path` — raw control

  See `Exhub.LspBridge.ClientManager` for the full command/callback surface.
  """

  alias Exhub.LspBridge.ClientManager

  def call(["lsp-bridge" | args]) do
    ClientManager.handle_command(args)
    nil
  end

  def call(args) do
    ClientManager.handle_command(args)
    nil
  end
end
