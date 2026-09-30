defmodule Exhub.Memory.Review do
  @moduledoc """
  Review-gated lifecycle transitions for memory.

  Candidates are never approved automatically. A human (or an agent acting on an
  explicit instruction) calls `approve/2`, `reject/2` or `supersede/3`; only
  `approve/2` moves a memory to `status: approved`, and it refuses a memory with
  no lesson body (the Claude-style "never approve the placeholder" boundary).
  Superseding keeps the old note for provenance and links the two, rather than
  overwriting it.
  """

  alias Exhub.Memory.SecretScan
  alias Exhub.Memory.Store

  @placeholder "no lesson text was extracted"

  @doc """
  Approve a candidate, optionally replacing its title/kind/applicability/tags
  and body in the same step.
  """
  @spec approve(String.t(), keyword()) :: {:ok, map()} | {:error, term()}
  def approve(memory_id, opts \\ []) do
    with {:ok, record} <- fetch(memory_id),
         :ok <- ensure_lesson(record, opts),
         :ok <- ensure_no_secrets(record, opts) do
      changes =
        %{"status" => "approved", "reviewed_at" => Store.now_iso()}
        |> maybe_put("reason", opts[:reason])
        |> merge_edits(opts)

      Store.update(memory_id, changes, opts[:body])
    end
  end

  @doc "Reject a candidate with a reason."
  @spec reject(String.t(), String.t() | nil) :: {:ok, map()} | {:error, term()}
  def reject(memory_id, reason \\ nil) do
    with {:ok, _record} <- fetch(memory_id) do
      Store.update(memory_id, %{
        "status" => "rejected",
        "reason" => reason,
        "reviewed_at" => Store.now_iso()
      })
    end
  end

  @doc """
  Mark `old_id` superseded by `new_id`, linking both notes so the review
  history shows what changed.
  """
  @spec supersede(String.t(), String.t(), String.t() | nil) :: {:ok, map()} | {:error, term()}
  def supersede(old_id, new_id, reason \\ nil) do
    with {:ok, _old} <- fetch(old_id),
         {:ok, _new} <- fetch(new_id) do
      {:ok, old_meta} =
        Store.update(old_id, %{
          "status" => "superseded",
          "superseded_by" => new_id,
          "reason" => reason,
          "reviewed_at" => Store.now_iso()
        })

      {:ok, _new_meta} = Store.update(new_id, %{"supersedes" => old_id})

      {:ok, %{superseded: old_id, replacement: new_id, meta: old_meta}}
    end
  end

  # ── private ──────────────────────────────────────────────────────────────

  defp fetch(memory_id) do
    case Store.read(memory_id) do
      {:ok, record} -> {:ok, record}
      {:error, :not_found} -> {:error, "memory not found: #{memory_id}"}
    end
  end

  defp ensure_lesson(record, opts) do
    body = opts[:body] || record.body || ""

    if String.trim(body) == "" or String.contains?(body, @placeholder) do
      {:error, "refusing to approve a memory with no lesson body — draft the lesson first"}
    else
      :ok
    end
  end

  defp ensure_no_secrets(record, opts) do
    values = [
      opts[:title] || record.meta["title"],
      opts[:body] || record.body,
      opts[:applicability] || record.meta["applicability"],
      opts[:tags] || record.meta["tags"]
    ]

    case SecretScan.scan_many(List.flatten(values)) do
      :ok ->
        :ok

      {:error, findings} ->
        {:error, "refusing to approve: possible secret(s): #{Enum.join(findings, ", ")}"}
    end
  end

  defp merge_edits(changes, opts) do
    [:title, :kind, :applicability, :project, :tags]
    |> Enum.reduce(changes, fn key, acc ->
      case opts[key] do
        nil -> acc
        value -> Map.put(acc, to_string(key), value)
      end
    end)
  end

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)
end
