defmodule Exhub.MCP.Tools.Memory.Promote do
  @moduledoc "MCP Tool: memory_promote — install an approved memory as an Agent Skill."

  alias Exhub.Memory.Promote, as: MemoryPromote
  alias Exhub.MCP.Tools.Memory.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "memory_promote"

  @impl true
  def description do
    """
    Install an approved memory as an Agent Skill note inside the vault
    (`memory/skills/<slug>/SKILL.md` by default) so skill-capable harnesses load
    the lesson automatically, without a memory lookup.

    Promote what earns it: `workflow` and `convention` memories that apply to
    many tasks, or a `debugging_pattern` for a recurring failure. Leave one-off
    gotchas in memory. Refuses to overwrite an existing skill unless `force: true`.
    """
  end

  schema do
    field(:memory_id, {:required, :string}, description: "The approved memory id")

    field(:force, :boolean,
      description: "Overwrite an existing skill file (default: false)",
      default: false
    )
  end

  @impl true
  def execute(params, frame) do
    case MemoryPromote.promote(Map.get(params, :memory_id),
           force: Map.get(params, :force, false) == true
         ) do
      {:ok, result} -> Helpers.json(frame, result)
      {:error, reason} -> Helpers.error(frame, reason)
    end
  end
end
