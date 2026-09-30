defmodule Exhub.MCP.MemoryServer do
  @moduledoc """
  MCP server for the ExHub memory layer — a Beacon-style, review-gated memory
  loop built on the Brain (Obsidian) vault and Smart Decide (System One).

  Session knowledge becomes durable project memory through five stages:

    1. **Distill** — `memory_distill` turns a session/fix into a candidate,
       evaluated by the three-question System One gate (`Exhub.Memory.Evaluator`).
    2. **Review** — a person (or an agent acting on an explicit instruction)
       approves, rejects or supersedes with `memory_approve` / `memory_reject` /
       `memory_supersede`. Candidates are never approved automatically.
    3. **Reuse** — `memory_search` / `memory_context` recall approved memory,
       scoped by project and ranked through the Brain pipeline.
    4. **Inspect** — `memory_show` returns a memory's body with its evidence,
       evaluation and supersede links; `memory_candidates` lists the queue.
    5. **Promote** — `memory_promote` installs an approved memory as an Agent
       Skill note inside the vault so future agents load it automatically.

  Memories are stored as markdown notes under the configured
  `:exhub -> :memory -> :vault_folder` (default `memory/`) in the Brain vault,
  so they stay greppable, linkable and visible to every existing Brain tool.

  Endpoint: `/memory/mcp` (built-in hub name `memory`).
  """

  use Anubis.Server,
    name: "exhub-memory-server",
    version: "1.0.0",
    capabilities: [:tools]

  # Lifecycle
  component(Exhub.MCP.Tools.Memory.Distill)
  component(Exhub.MCP.Tools.Memory.Candidates)
  component(Exhub.MCP.Tools.Memory.Show)
  component(Exhub.MCP.Tools.Memory.Approve)
  component(Exhub.MCP.Tools.Memory.Reject)
  component(Exhub.MCP.Tools.Memory.Supersede)

  # Recall
  component(Exhub.MCP.Tools.Memory.Search)
  component(Exhub.MCP.Tools.Memory.Context)

  # Promotion
  component(Exhub.MCP.Tools.Memory.Promote)

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
