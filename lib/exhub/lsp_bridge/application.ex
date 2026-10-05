defmodule Exhub.LspBridge.Application do
  @moduledoc """
  Supervisor subtree for the lsp-bridge port.

  A single child added to `Exhub.Supervisor` that owns everything the port
  needs, so the top-level application tree stays tidy and the whole feature can
  be hot-reloaded as a unit:

    * `Exhub.LspBridge.Registry` — unique-key registry of sessions
      (`{:session, root, profile}`), servers (`{:server, root, name}`) and open
      documents (`{:doc, filepath}` → owning session);
    * `Exhub.LspBridge.Config` — lazy loader/cacher of `langserver/*.json`;
    * `Exhub.LspBridge.Supervisor` — `DynamicSupervisor` fanning out one
      `Exhub.LspBridge.Server` per language-server process;
    * `Exhub.LspBridge.SessionSupervisor` — `DynamicSupervisor` for
      `Exhub.LspBridge.Session` instances (one per project + server profile);
    * `Exhub.LspBridge.ClientManager` — WebSocket command coordinator.

  Servers are started on demand (from Emacs commands), never eagerly, so boot
  cost is ~zero until a buffer actually asks for a language server.
  """

  use Supervisor

  def start_link(opts \\ []) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    children = [
      {Registry, keys: :unique, name: Exhub.LspBridge.Registry},
      {Exhub.LspBridge.Config, []},
      {DynamicSupervisor, strategy: :one_for_one, name: Exhub.LspBridge.Supervisor},
      {DynamicSupervisor, strategy: :one_for_one, name: Exhub.LspBridge.SessionSupervisor},
      {Exhub.LspBridge.ClientManager, []}
    ]

    Supervisor.init(children, strategy: :one_for_all)
  end
end
