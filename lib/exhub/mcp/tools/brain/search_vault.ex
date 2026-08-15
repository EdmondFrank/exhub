defmodule Exhub.MCP.Tools.Brain.SearchVault do
  @moduledoc """
  MCP Tool: brain_search_vault

  Advanced search for notes in the Obsidian vault. Candidate notes are
  retrieved by content, filename, or tags (BM25 prefilter), then ranked by a
  configurable multi-signal pipeline of scorers (BM25, title/heading match,
  tag match, recency, backlink authority) fused via `weighted_sum`, RRF, or
  `max`.

  Ranking is tunable via optional `fusion`, `weights`, and `min_score`
  parameters; defaults live in config (`:exhub -> :brain_ranking`).
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Brain.Helpers
  alias Exhub.MCP.Brain.RAG.VectorIndex
  alias Exhub.MCP.Brain.Ranking.Ranker
  alias Exhub.MCP.Brain.Ranking.Scorers
  alias Exhub.MCP.Brain.Search.Policies
  alias Exhub.MCP.Brain.Search.Selector

  require Logger

  use Anubis.Server.Component, type: :tool

  def name, do: "brain_search_vault"

  @impl true
  def description do
    """
    Search for notes in the Obsidian brain vault with configurable ranking.

    Search types:
    - content:  Search within note content (default)
    - filename: Search by filename/path
    - both:     Search both content and filenames

    Tag search: prefix query with "tag:" to search by tag (e.g. "tag:project/active")

    Ranking options:
    - fusion: "weighted_sum" (default), "rrf" (Reciprocal Rank Fusion), or "max"
    - weights: map of scorer name to weight, e.g. {"freshness": 0.3, "bm25": 0.5}
    - min_score: drop results below this final score (default 0.0)

    Search policies (policy):
    - "auto" (default): pick the policy from query heuristics
    - "balanced", "keyword", "semantic", "recency", "filename": built-ins
    - custom policy name configured under :exhub -> :brain_search
    - inline map, e.g. {"semantic": "on", "weights": {"semantic": 0.9}, "top_n": 5}
    Explicit params (search_type/semantic/fusion/weights/min_score) override the policy.

    Scorers: bm25, title_match, tag_match, freshness, link_authority, semantic

    Examples:
    - Content search:  { "query": "meeting notes" }
    - Filename search: { "query": "journal", "search_type": "filename" }
    - Tag search:      { "query": "tag:status/active" }
    - Recency-first:   { "query": "meeting", "fusion": "weighted_sum", "weights": {"freshness": 0.8} }
    - Semantic search: { "query": "how do we handle logins", "semantic": true }
    - Scoped search:   { "query": "todo", "path": "projects" }
    - Absolute paths:  { "query": "meeting", "abs_path": true }
    """
  end

  schema do
    field(:query, {:required, :string},
      description: "Search query. Use 'tag:' prefix for tag search (e.g. 'tag:project')"
    )

    field(:path, :string,
      description: "Optional subfolder path within the vault to limit search scope"
    )

    field(:search_type, :string,
      description: "Type of search: 'content' (default), 'filename', or 'both'",
      default: "content"
    )

    field(:case_sensitive, :boolean,
      description: "Whether to perform case-sensitive search (default: false)",
      default: false
    )

    field(:abs_path, :boolean,
      description: "Return absolute paths instead of relative (default: false)",
      default: false
    )

    field(:fusion, :string,
      description: "Ranking fusion: 'weighted_sum' (default), 'rrf', or 'max'"
    )

    field(:weights, :map,
      description:
        "Scorer weights, e.g. %{\"freshness\" => 0.3, \"bm25\" => 0.5}. Merged over defaults."
    )

    field(:min_score, :float,
      description: "Drop results with final score below this threshold (default 0.0)",
      default: 0.0
    )

    field(:semantic, :boolean,
      description:
        "Enable vector/semantic search (RAG). Requires OpenAI/Gitee AI embedding API key. Combines keyword + vector results.",
      default: false
    )

    field(:semantic_limit, :integer,
      description: "Max notes to return from vector search when semantic is enabled (default 10)",
      default: 10
    )
  end

  @impl true
  def execute(params, frame) do
    query = Map.get(params, :query)
    scope_path = Map.get(params, :path)
    case_sensitive = Map.get(params, :case_sensitive, false)
    abs_path = Map.get(params, :abs_path, false)
    semantic_limit = Map.get(params, :semantic_limit, 10)

    # Resolve the active search policy (explicit param, config default, or
    # auto-selected via query heuristics) and derive retrieval + ranking
    # decisions from it. Explicit params override the policy.
    policy = resolve_policy(params, query)
    search_type = effective_search_type(params, policy)
    semantic = effective_semantic(policy, params, query)
    fusion = effective_fusion(policy, params)
    weights = effective_weights(policy, params)
    min_score = effective_min_score(policy, params)

    vault = Helpers.vault_path()
    search_dir = if scope_path, do: Path.join(vault, scope_path), else: vault

    gitignore_patterns = Helpers.load_gitignore_patterns(vault)

    with :ok <- Helpers.validate_in_vault(vault, search_dir) do
      files = Helpers.list_md_files(vault, search_dir, gitignore_patterns: gitignore_patterns)

      is_tag_search = String.starts_with?(query, "tag:")
      tag_query = if is_tag_search, do: normalize_tag(String.slice(query, 4..-1//1)), else: nil
      query_terms = split_query_into_words(if is_tag_search, do: "", else: query)

      notes =
        cond do
          search_type == "filename" ->
            search_filenames(vault, files, query, case_sensitive)

          search_type == "both" ->
            fn_notes = search_filenames(vault, files, query, case_sensitive)
            ct_notes = search_content(vault, files, query_terms, is_tag_search, tag_query, case_sensitive)
            merge_notes(fn_notes, ct_notes)

          true ->
            search_content(vault, files, query_terms, is_tag_search, tag_query, case_sensitive)
        end

      context =
        build_context(query, vault, files, query_terms, is_tag_search, tag_query, case_sensitive)

      {notes, context, semantic_scorers} =
        maybe_semantic(notes, context, vault, files, semantic, semantic_limit)

      scorers = semantic_scorers || policy.scorers

      rank_opts =
        [fusion: fusion, weights: weights, min_score: min_score, context: context]
        |> maybe_put_scorers(scorers)

      ranked =
        Ranker.rank(notes, rank_opts)

      ranked = maybe_top_n(ranked, policy.top_n)
      ranked = if abs_path, do: absolutize(ranked, vault), else: ranked

      total_matches =
        Enum.reduce(ranked, 0, fn r, acc ->
          # Semantic-only notes carry no keyword matches; count their preview
          # so the total isn't reported as zero while files are shown.
          n = if r.matches == [] and Map.get(r, :preview), do: 1, else: length(r.matches)
          acc + n
        end)
      output = format_results(ranked, total_matches, vault)
      resp = Response.tool() |> Response.text(output)
      {:reply, resp, frame}
    else
      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end

  # ── search policy resolution ─────────────────────────────────────────────

  defp resolve_policy(params, query) do
    policy_param = Map.get(params, :policy)

    if Policies.auto?(policy_param) do
      Policies.get(auto_select(query))
    else
      Policies.resolve(policy_param)
    end
  end

  # The Selector picks the concrete policy for `auto` mode; a semantic pick is
  # demoted to balanced when the operator disabled semantic autodetect.
  defp auto_select(query) do
    selected = Selector.select(query)

    if selected == "semantic" and not Policies.semantic_autodetect?() do
      "balanced"
    else
      selected
    end
  end

  # Explicit `search_type` wins; otherwise derive from the policy's retrieval
  # channels. The schema default is "content", so only treat a non-content
  # value as an explicit override (a policy can still request content).
  defp effective_search_type(params, policy) do
    case Map.get(params, :search_type, "content") do
      "content" -> retrieval_to_search_type(policy.retrieval)
      explicit -> explicit
    end
  end

  defp retrieval_to_search_type([:filename]), do: "filename"
  defp retrieval_to_search_type([:both]), do: "both"
  defp retrieval_to_search_type(_), do: "content"

  defp effective_semantic(policy, params, query) do
    cond do
      Map.get(params, :semantic, false) == true -> true
      policy.semantic == :on -> true
      policy.semantic == :off -> false
      true ->
        not String.starts_with?(query, "tag:") and
          Selector.semantic_query?(query) and
          Policies.semantic_autodetect?()
    end
  end

  defp effective_fusion(policy, params), do: Map.get(params, :fusion) || policy.fusion

  defp effective_weights(policy, params) do
    case Map.get(params, :weights) do
      nil -> policy.weights || %{}
      w when is_map(w) and map_size(w) == 0 -> policy.weights || %{}
      w -> w
    end
  end

  defp effective_min_score(policy, params) do
    min_score = Map.get(params, :min_score, 0.0)

    if is_number(min_score) and min_score == 0.0 do
      policy.min_score || 0.0
    else
      min_score
    end
  end

  defp maybe_top_n(results, nil), do: results
  defp maybe_top_n(results, n) when is_integer(n) and n > 0, do: Enum.take(results, n)
  defp maybe_top_n(results, _), do: results

  # ── semantic / vector search ───────────────────────────────────────────────

  defp maybe_semantic(notes, context, _vault, _files, false, _limit) do
    {notes, context, nil}
  end

  defp maybe_semantic(notes, context, _vault, _files, true, _limit)
       when is_map_key(context, :is_tag_search) and context.is_tag_search do
    {notes, context, nil}
  end

  defp maybe_semantic(notes, context, vault, files, true, limit) do
    # Build/index the vault (only changed files), then run vector search.
    server = VectorIndex.registry_name()
    full_files = Enum.map(files, &Path.join(vault, &1))
    _ = VectorIndex.rebuild(full_files, server)

    case VectorIndex.search(context.query, top_k: limit, server: server) do
      {:ok, results} ->
        # VectorIndex stores absolute paths; convert to vault-relative to line
        # up with keyword-search candidates, then drop any result outside the
        # requested scope (`files` is already constrained to search_dir).
        scope_files = MapSet.new(files)

        results =
          results
          |> relativize_results(vault)
          |> Enum.filter(&MapSet.member?(scope_files, &1.file))

        # Merge vector-discovered notes into candidates (dedup by file).
        # `matches` is left empty so BM25/etc. don't fabricate a score for
        # semantic-only notes; the preview is carried separately for display.
        semantic_notes =
          Enum.map(results, fn r ->
            %{
              id: r.file,
              file: r.file,
              full_path: Path.join(vault, r.file),
              matches: [],
              preview: semantic_preview(r),
              mtime: Helpers.note_mtime(Path.join(vault, r.file)),
              content: nil,
              length: 1,
              terms: [],
              tags: []
            }
          end)

        merged = merge_notes(notes, semantic_notes)
        context = Map.put(context, :semantic_results, results)
        # Ensure the Semantic scorer is included in the ranking pipeline.
        scorers = add_semantic_scorer()
        {merged, context, scorers}

      {:error, reason} ->
        Logger.warning("[BrainSearch] Semantic search unavailable: #{reason}")
        {notes, context, nil}
    end
  end

  defp relativize_results(results, vault) do
    Enum.map(results, fn r ->
      %{r | file: Path.relative_to(r.file, vault)}
    end)
  end

  defp semantic_preview(%{text: text}) when is_binary(text) do
    preview = text |> String.split("\n") |> Enum.reject(&(&1 == "")) |> List.first() || ""
    "Semantic match: #{String.slice(preview, 0, 120)}"
  end

  defp semantic_preview(_), do: "Semantic match"

  defp add_semantic_scorer do
    default = [
      Scorers.BM25,
      Scorers.TitleMatch,
      Scorers.TagMatch,
      Scorers.Freshness,
      Scorers.LinkAuthority
    ]

    if Scorers.Semantic in default do
      default
    else
      default ++ [Scorers.Semantic]
    end
  end

  defp maybe_put_scorers(opts, nil), do: opts
  defp maybe_put_scorers(opts, scorers), do: Keyword.put(opts, :scorers, scorers)

  # ── ranking context ─────────────────────────────────────────────────────────

  defp build_context(query, vault, files, query_terms, is_tag_search, tag_query, case_sensitive) do
    docs_data = build_docs_data(vault, files)
    avgdl = calculate_avgdl(docs_data)
    doc_count = length(files)

    doc_freq =
      matching_doc_count(docs_data, query_terms, case_sensitive, is_tag_search, tag_query)

    backlinks = Helpers.count_backlinks(vault, files)

    %{
      query: query,
      query_terms: query_terms,
      is_tag_search: is_tag_search,
      tag_query: tag_query,
      vault: vault,
      docs_data: docs_data,
      avgdl: avgdl,
      doc_count: doc_count,
      doc_freq: doc_freq,
      backlinks: backlinks
    }
  end

  # ── candidate retrieval ─────────────────────────────────────────────────────

  defp search_filenames(vault, files, query, case_sensitive) do
    q = if case_sensitive, do: query, else: String.downcase(query)

    Enum.flat_map(files, fn file ->
      target = if case_sensitive, do: file, else: String.downcase(file)

      if String.contains?(target, q) do
        full = Path.join(vault, file)
        matches = [%{line: 0, text: "Filename match: #{file}"}]
        [build_note(file, full, matches, nil)]
      else
        []
      end
    end)
  end

  defp search_content(vault, files, query_terms, is_tag_search, tag_query, case_sensitive) do
    Enum.flat_map(files, fn rel ->
      full = Path.join(vault, rel)

      case File.read(full) do
        {:ok, content} ->
          matches =
            if is_tag_search do
              case find_tag_matches(content, tag_query) do
                [] ->
                  if Enum.any?(Helpers.extract_tags(content), &tag_matches?(&1, tag_query)) do
                    [%{line: 0, text: "Tag match: ##{tag_query}"}]
                  else
                    []
                  end

                m ->
                  m
              end
            else
              find_text_matches(content, query_terms, case_sensitive)
            end

          if matches == [], do: [], else: [build_note(rel, full, matches, content)]

        {:error, _} ->
          []
      end
    end)
  end

  defp build_note(rel, full, matches, content) do
    base = %{
      id: rel,
      file: rel,
      full_path: full,
      matches: matches,
      mtime: Helpers.note_mtime(full)
    }

    if is_nil(content) do
      Map.merge(base, %{content: nil, length: 1, terms: [], tags: []})
    else
      Map.merge(base, %{
        content: content,
        length: String.length(content),
        terms: extract_terms(content),
        tags: Helpers.extract_tags(content)
      })
    end
  end

  defp merge_notes(a, b) do
    (a ++ b)
    |> Enum.group_by(& &1.file)
    |> Enum.map(fn {_file, entries} ->
      # Prefer the entry that carries real content (the content-search note)
      # over the filename-only stub, so length/terms/tags are not lost.
      base = Enum.find(entries, &(not is_nil(&1.content))) || List.first(entries)
      matches = entries |> Enum.flat_map(& &1.matches) |> Enum.uniq_by(& &1.line)
      merged = %{base | matches: matches}

      # Keep the semantic preview even when a keyword note is chosen as base.
      case Enum.find(entries, &Map.get(&1, :preview)) do
        nil -> merged
        preview_note -> Map.put(merged, :preview, preview_note.preview)
      end
    end)
  end

  defp find_text_matches(content, query_terms, case_sensitive) do
    content
    |> String.split("\n")
    |> Enum.with_index(1)
    |> Enum.flat_map(fn {line, idx} ->
      target = if case_sensitive, do: line, else: String.downcase(line)

      if Enum.any?(query_terms, fn term ->
           q = if case_sensitive, do: term, else: String.downcase(term)
           String.contains?(target, q)
         end) do
        [%{line: idx, text: String.trim(line)}]
      else
        []
      end
    end)
  end

  defp find_tag_matches(content, tag_query) do
    content
    |> String.split("\n")
    |> Enum.with_index(1)
    |> Enum.flat_map(fn {line, idx} ->
      tags =
        Regex.scan(~r/(?:^|\s)#([\w\/]+)/, line)
        |> Enum.map(fn [_, t] -> normalize_tag(t) end)

      if Enum.any?(tags, &tag_matches?(&1, tag_query)) do
        [%{line: idx, text: String.trim(line)}]
      else
        []
      end
    end)
  end

  defp split_query_into_words(query) do
    query
    |> String.split(~r/\s+/, trim: true)
    |> Enum.reject(&(&1 == ""))
  end

  defp build_docs_data(vault, files) do
    Enum.reduce(files, %{}, fn rel, acc ->
      case File.read(Path.join(vault, rel)) do
        {:ok, content} ->
          Map.put(acc, rel, %{
            length: String.length(content),
            terms: extract_terms(content),
            tags: Helpers.extract_tags(content)
          })

        {:error, _} ->
          acc
      end
    end)
  end

  defp extract_terms(content) do
    content
    |> String.downcase()
    |> String.split(~r/[^\w\s]/u, trim: true)
    |> Enum.flat_map(&String.split(&1, ~r/\s+/, trim: true))
    |> Enum.reject(&(&1 == ""))
  end

  defp calculate_avgdl(docs_data) do
    if map_size(docs_data) == 0 do
      1.0
    else
      total = Enum.sum(Enum.map(docs_data, fn {_, v} -> v.length end))
      total / map_size(docs_data)
    end
  end

  defp matching_doc_count(docs_data, query_terms, case_sensitive, is_tag_search, tag_query) do
    Enum.count(docs_data, fn {_, data} ->
      if is_tag_search do
        Enum.any?(data.tags || [], &tag_matches?(normalize_tag(&1), tag_query))
      else
        Enum.any?(query_terms, fn term ->
          q = if case_sensitive, do: term, else: String.downcase(term)

          Enum.any?(data.terms, fn t ->
            t = if case_sensitive, do: t, else: String.downcase(t)
            String.contains?(t, q)
          end)
        end)
      end
    end)
  end

  defp normalize_tag(tag), do: String.downcase(tag)

  defp tag_matches?(tag, query) do
    tag == query or String.starts_with?(tag, query <> "/")
  end

  # ── output helpers ──────────────────────────────────────────────────────────

  defp absolutize(results, vault) do
    Enum.map(results, fn result ->
      file = Path.join(vault, result.file)

      new_matches =
        Enum.map(result.matches, fn
          %{line: 0, text: "Filename match: " <> _} = m ->
            %{m | text: "Filename match: #{file}"}

          m ->
            m
        end)

      %{result | file: file, full_path: file, matches: new_matches}
    end)
  end

  defp format_results([], _total, vault) do
    "Vault: #{vault}\n\nNo results found."
  end

  defp format_results(results, total, vault) do
    header = "Vault: #{vault}\n\nFound #{total} match(es) in #{length(results)} file(s):\n\n"

    body =
      Enum.map_join(results, "\n", fn result ->
        file = result.file
        score = result.final_score
        scores = Map.get(result, :scores, %{})

        signals =
          scores
          |> Enum.sort_by(fn {_, v} -> v end, :desc)
          |> Enum.map_join(", ", fn {name, v} -> "#{name}=#{:erlang.float_to_binary(v, decimals: 3)}" end)

        match_lines =
          Enum.map_join(result.matches, "\n", fn %{line: line, text: text} ->
            if line == 0, do: "  #{text}", else: "  L#{line}: #{text}"
          end)

        preview_line =
          case Map.get(result, :preview) do
            nil -> ""
            preview -> "  #{preview}"
          end

        lines =
          [preview_line, match_lines]
          |> Enum.reject(&(&1 == ""))
          |> Enum.join("\n")

        "#{file} (score: #{:erlang.float_to_binary(score, decimals: 4)}) [#{signals}]:\n#{lines}"
      end)

    header <> body
  end
end