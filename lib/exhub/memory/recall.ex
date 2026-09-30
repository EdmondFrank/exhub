defmodule Exhub.Memory.Recall do
  @moduledoc """
  Recall approved project memory through the Brain ranking pipeline.

  Recall is deliberately conservative, matching Beacon's model:

    * only `status: approved` memories are returned by default;
    * a query's terms are **ANDed** — every term must appear (in the title,
      body or tags) — because a full sentence over-constrains the search;
    * candidates are then ranked with the Brain scorers (BM25, title, tag,
      recency, link authority) and, when enabled, passed through the Smart
      Decide relevance filter for precision.

  Memories are lessons from earlier sessions, not policy: callers should apply
  one only when its `applicability` matches, and cite its `memory_id`.
  """

  alias Exhub.MCP.Brain.Helpers
  alias Exhub.MCP.Brain.Ranking.Ranker
  alias Exhub.MCP.Brain.Search.Relevance
  alias Exhub.Memory.Store

  @defaults [limit: 5, filter: true]

  @doc "Effective recall configuration."
  @spec config() :: keyword()
  def config, do: Keyword.merge(@defaults, Keyword.get(Store.config(), :recall, []) || [])

  @doc """
  Search memories.

  Options:

    * `:project` — scope to a project (explicit field or `project/<name>` tag)
    * `:kind`    — `workflow | correction | debugging_pattern | gotcha | convention`
    * `:status`  — lifecycle status (default `"approved"`; pass `nil` for any)
    * `:limit`   — max results (default 5)
    * `:filter`  — run the Smart Decide relevance pass (default config)
  """
  @spec search(String.t() | nil, keyword()) :: [map()]
  def search(query, opts \\ []) do
    cfg = config()
    limit = opts[:limit] || cfg[:limit]
    filter? = Keyword.get(opts, :filter, cfg[:filter])
    status = Keyword.get(opts, :status, "approved")
    project = opts[:project]
    kind = opts[:kind]

    query = query || ""
    terms = terms(query)

    notes =
      Store.list(status: status, kind: kind, project: project)
      |> Enum.map(&to_note(&1, terms))

    matched = if terms == [], do: notes, else: Enum.filter(notes, &all_terms?(&1, terms))

    context = build_context(matched, terms, query)
    ranked = Ranker.rank(matched, context: context)

    ranked =
      if filter? and terms != [] and ranked != [] do
        {kept, _stats} = Relevance.filter(query, ranked, Relevance.config())
        kept
      else
        ranked
      end

    ranked
    |> Enum.take(limit)
    |> Enum.map(&to_result/1)
  end

  @doc "Human/agent-friendly rendering of `search/2` results."
  @spec format([map()], String.t() | nil) :: String.t()
  def format([], _query), do: "No approved memory matched."

  def format(results, query) do
    header =
      case query do
        q when is_binary(q) and q != "" -> "Memory matching \"#{q}\":"
        _ -> "Recent approved memory:"
      end

    body =
      Enum.map_join(results, "\n\n", fn r ->
        "## #{r["memory_id"]} — #{r["title"]}\n" <>
          "- kind: #{r["kind"]}  project: #{r["project"] || "-"}\n" <>
          "- applies when: #{r["applicability"] || "-"}\n" <>
          "  #{r["excerpt"]}"
      end)

    header <> "\n\n" <> body
  end

  # ── note construction / scoring ────────────────────────────────────────────

  defp to_note(record, terms) do
    meta = record.meta
    body = record.body || ""

    %{
      id: meta["memory_id"],
      file: record.file,
      full_path: record.full_path,
      content: body,
      body: body,
      meta: meta,
      matches: term_matches(body, terms),
      length: String.length(body),
      tags: Map.get(meta, "tags", []) || [],
      mtime: Helpers.note_mtime(record.full_path),
      preview: excerpt(body)
    }
  end

  defp term_matches(body, terms) do
    body
    |> String.split("\n")
    |> Enum.with_index(1)
    |> Enum.filter(fn {line, _idx} ->
      Enum.any?(terms, &String.contains?(String.downcase(line), &1))
    end)
    |> Enum.map(fn {line, idx} -> %{line: idx, text: String.trim(line)} end)
  end

  defp all_terms?(note, terms) do
    haystack =
      String.downcase((note.body || "") <> " " <> title(note) <> " " <> Enum.join(note.tags, " "))

    Enum.all?(terms, &String.contains?(haystack, &1))
  end

  defp build_context(notes, terms, query) do
    docs =
      Map.new(notes, fn note ->
        {note.file, %{length: note.length, terms: extract_terms(note.body), tags: note.tags}}
      end)

    vault = Helpers.vault_path()

    %{
      query: query,
      query_terms: terms,
      is_tag_search: false,
      vault: vault,
      docs_data: docs,
      avgdl: avgdl(docs),
      doc_count: max(map_size(docs), 1),
      doc_freq: doc_freq(docs, terms),
      backlinks: Helpers.count_backlinks(vault, Enum.map(notes, & &1.file))
    }
  end

  defp avgdl(docs) when map_size(docs) == 0, do: 1.0

  defp avgdl(docs) do
    total = docs |> Enum.map(fn {_, d} -> d.length end) |> Enum.sum()
    total / map_size(docs)
  end

  defp doc_freq(docs, terms) do
    Enum.count(docs, fn {_, d} ->
      Enum.any?(terms, fn term ->
        Enum.any?(d.terms, &String.contains?(&1, term))
      end)
    end)
  end

  defp extract_terms(content) when is_binary(content) do
    content
    |> String.downcase()
    |> String.split(~r/[^\w\s]/u, trim: true)
    |> Enum.flat_map(&String.split(&1, ~r/\s+/, trim: true))
    |> Enum.reject(&(&1 == ""))
  end

  defp extract_terms(_), do: []

  # ── results ────────────────────────────────────────────────────────────────

  defp to_result(note) do
    %{
      "memory_id" => note.meta["memory_id"],
      "kind" => note.meta["kind"],
      "title" => title(note),
      "applicability" => note.meta["applicability"],
      "project" => note.meta["project"],
      "tags" => note.tags,
      "status" => note.meta["status"],
      "score" => Map.get(note, :final_score, 0.0),
      "file" => note.file,
      "excerpt" => note.preview
    }
  end

  defp title(note) do
    note.meta["title"] || note.file |> Path.basename() |> Path.rootname()
  end

  defp terms(nil), do: []
  defp terms(query), do: query |> String.downcase() |> String.split(~r/\s+/, trim: true)

  defp excerpt(body) when is_binary(body) do
    body
    |> String.split("\n")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.take(2)
    |> Enum.join(" ")
    |> String.slice(0, 240)
  end

  defp excerpt(_), do: ""
end
