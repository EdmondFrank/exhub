defmodule Exhub.Memory.Promote do
  @moduledoc """
  Promote an approved memory into an Agent Skill file inside the vault.

  Beacon promotes approved memory into `.agents/skills/<slug>/SKILL.md` so every
  skill-capable harness loads the lesson automatically. Here the skill is
  written under the vault (`memory/skills/<slug>/SKILL.md` by default), keeping
  memory and its promoted form local-first and co-located. Provenance
  (`exhub_memory_id`, kind, project) travels in the skill frontmatter so a
  reviewer can trace it back to the approved memory.
  """

  alias Exhub.Memory.Store

  @doc """
  Write `SKILL.md` for an approved memory. Refuses to overwrite an existing
  skill unless `force: true`.
  """
  @spec promote(String.t(), keyword()) :: {:ok, map()} | {:error, term()}
  def promote(memory_id, opts \\ []) do
    force? = Keyword.get(opts, :force, false)

    with {:ok, record} <- fetch(memory_id),
         :ok <- ensure_approved(record) do
      meta = record.meta
      title = meta["title"] || memory_id
      slug = slugify(title)
      dir = Path.join([Store.config() |> Keyword.get(:skill_folder, "memory/skills"), slug])

      path = Path.join(dir, "SKILL.md")
      full = Path.join(Exhub.MCP.Brain.Helpers.vault_path(), path)

      cond do
        File.exists?(full) and not force? ->
          {:error, "skill already exists at #{path} — pass force: true to replace"}

        true ->
          with :ok <- File.mkdir_p(Path.dirname(full)),
               :ok <- File.write(full, skill_content(meta, record.body)) do
            {:ok, %{path: path, slug: slug, memory_id: memory_id}}
          end
      end
    end
  end

  # ── private ──────────────────────────────────────────────────────────────

  defp fetch(memory_id) do
    case Store.read(memory_id) do
      {:ok, record} -> {:ok, record}
      {:error, :not_found} -> {:error, "memory not found: #{memory_id}"}
    end
  end

  defp ensure_approved(%{meta: %{"status" => "approved"}}), do: :ok
  defp ensure_approved(_), do: {:error, "only approved memory can be promoted"}

  defp skill_content(meta, body) do
    title = meta["title"] || ""
    applicability = meta["applicability"] || ""

    description =
      [title, applicability]
      |> Enum.reject(&(&1 in [nil, ""]))
      |> Enum.join(". Use when ")

    frontmatter = [
      "name: #{slugify(title)}",
      "description: #{description}",
      "metadata:",
      "  exhub_memory_id: #{meta["memory_id"]}",
      "  exhub_kind: #{meta["kind"]}",
      "  exhub_project: #{meta["project"]}"
    ]

    "---\n" <> Enum.join(frontmatter, "\n") <> "\n---\n\n" <> body
  end

  defp slugify(text) do
    text
    |> to_string()
    |> String.downcase()
    |> String.replace(~r/[^a-z0-9]+/, "-")
    |> String.trim("-")
    |> case do
      "" -> "memory"
      slug -> String.slice(slug, 0, 60)
    end
  end
end
