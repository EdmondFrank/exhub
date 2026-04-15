defmodule Exhub.MCP.Tools.Brain.SearchVault do
  @moduledoc """
  MCP Tool: brain_search_vault

  Search for notes in the Obsidian vault by content, filename, or tags.
  Results are ranked by BM25 score for relevance.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Brain.Helpers

  use Anubis.Server.Component, type: :tool

  # BM25 parameters
  @k1 1.2
  @b 0.75

  def name, do: "brain_search_vault"

  @impl true
  def description do
    """
    Search for notes in the Obsidian brain vault.

    Search types:
    - content:  Search within note content (default)
    - filename: Search by filename/path
    - both:     Search both content and filenames

    Tag search: prefix query with "tag:" to search by tag (e.g. "tag:project/active")

    Examples:
    - Content search:  { "query": "meeting notes" }
    - Filename search: { "query": "journal", "search_type": "filename" }
    - Tag search:      { "query": "tag:status/active" }
    - Scoped search:   { "query": "todo", "path": "projects" }
    - Case sensitive:  { "query": "TODO", "case_sensitive": true }
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
  end

  @impl true
  def execute(params, frame) do
    query = Map.get(params, :query)
    scope_path = Map.get(params, :path)
    search_type = Map.get(params, :search_type, "content")
    case_sensitive = Map.get(params, :case_sensitive, false)
    abs_path = Map.get(params, :abs_path, false)

    vault = Helpers.vault_path()
    search_dir = if scope_path, do: Path.join(vault, scope_path), else: vault

    with :ok <- Helpers.validate_in_vault(vault, search_dir) do
      files = Helpers.list_md_files(vault, search_dir)

      results =
        cond do
          search_type == "filename" ->
            search_filenames(files, query, case_sensitive)

          search_type == "both" ->
            fn_results = search_filenames(files, query, case_sensitive)
            ct_results = search_content(vault, search_dir, files, query, case_sensitive)
            merge_results(fn_results, ct_results)

          true ->
            search_content(vault, search_dir, files, query, case_sensitive)
        end

      # Fallback: if no results, split query into words and search again
      results =
        if results == [] and search_type in ["content", "both"] do
          words = split_query_into_words(query)
          if length(words) > 1 do
            search_content_with_words(vault, search_dir, files, words, case_sensitive)
          else
            results
          end
        else
          results
        end

      # Sort by BM25 score descending
      results = Enum.sort_by(results, & &1.score, :desc)

      results = if abs_path do
        Enum.map(results, fn %{file: file, matches: matches} = result ->
          abs_file = Path.join(vault, file)
          new_matches = Enum.map(matches, fn
            %{line: 0, text: "Filename match: " <> _} = m ->
              %{m | text: "Filename match: #{abs_file}"}
            m -> m
          end)
          %{result | file: abs_file, matches: new_matches}
        end)
      else
        results
      end

      total_matches = Enum.reduce(results, 0, fn r, acc -> acc + length(r.matches) end)

      output = format_results(results, total_matches, vault)
      resp = Response.tool() |> Response.text(output)
      {:reply, resp, frame}
    else
      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end

  # ── filename search ──────────────────────────────────────────────────────────

  defp search_filenames(files, query, case_sensitive) do
    q = if case_sensitive, do: query, else: String.downcase(query)

    Enum.flat_map(files, fn file ->
      target = if case_sensitive, do: file, else: String.downcase(file)

      if String.contains?(target, q) do
        [%{file: file, matches: [%{line: 0, text: "Filename match: #{file}"}], score: 1.0}]
      else
        []
      end
    end)
  end

  # ── content search ───────────────────────────────────────────────────────────

  defp search_content(vault, _search_dir, files, query, case_sensitive) do
    is_tag_search = String.starts_with?(query, "tag:")
    tag_query = if is_tag_search, do: String.slice(query, 4..-1//1) |> normalize_tag(), else: nil

    # Pre-calculate document statistics for BM25
    docs_data = build_docs_data(vault, files)
    avgdl = calculate_avgdl(docs_data)
    n = length(files)

    Enum.flat_map(files, fn rel_path ->
      full_path = Path.join(vault, rel_path)

      case File.read(full_path) do
        {:ok, content} ->
          matches =
            if is_tag_search do
              find_tag_matches(content, tag_query)
            else
              find_text_matches(content, query, case_sensitive)
            end

          if matches == [] do
            []
          else
            doc_length = docs_data[rel_path][:length] || 1
            term_freq = length(matches)
            doc_freq = calculate_doc_freq(docs_data, query, case_sensitive, is_tag_search, tag_query)
            score = calculate_bm25(term_freq, doc_length, avgdl, n, doc_freq)
            [%{file: rel_path, matches: matches, score: score}]
          end

        {:error, _} ->
          []
      end
    end)
  end

  # Search with multiple words (OR logic) and aggregate scores
  defp search_content_with_words(vault, _search_dir, files, words, case_sensitive) do
    docs_data = build_docs_data(vault, files)
    avgdl = calculate_avgdl(docs_data)
    n = length(files)

    # Search for each word and aggregate results
    words_results =
      Enum.flat_map(words, fn word ->
        Enum.flat_map(files, fn rel_path ->
          full_path = Path.join(vault, rel_path)

          case File.read(full_path) do
            {:ok, content} ->
              matches = find_text_matches(content, word, case_sensitive)

              if matches == [] do
                []
              else
                doc_length = docs_data[rel_path][:length] || 1
                term_freq = length(matches)
                doc_freq = calculate_doc_freq(docs_data, word, case_sensitive, false, nil)
                score = calculate_bm25(term_freq, doc_length, avgdl, n, doc_freq)
                [%{file: rel_path, matches: matches, word: word, score: score}]
              end

            {:error, _} ->
              []
          end
        end)
      end)

    # Group by file and aggregate scores
    words_results
    |> Enum.group_by(& &1.file)
    |> Enum.map(fn {file, entries} ->
      all_matches = Enum.flat_map(entries, & &1.matches) |> Enum.uniq_by(& &1.line)
      total_score = Enum.sum(Enum.map(entries, & &1.score))
      %{file: file, matches: all_matches, score: total_score}
    end)
  end

  defp split_query_into_words(query) do
    query
    |> String.split(~r/\s+/, trim: true)
    |> Enum.reject(&(&1 == ""))
  end

  defp build_docs_data(vault, files) do
    Enum.reduce(files, %{}, fn rel_path, acc ->
      full_path = Path.join(vault, rel_path)

      case File.read(full_path) do
        {:ok, content} ->
          length = String.length(content)
          terms = extract_terms(content)
          Map.put(acc, rel_path, %{length: length, terms: terms})

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
      total_length = Enum.sum(Enum.map(docs_data, fn {_, v} -> v.length end))
      total_length / map_size(docs_data)
    end
  end

  defp calculate_doc_freq(docs_data, query, case_sensitive, is_tag_search, tag_query) do
    Enum.count(docs_data, fn {_, data} ->
      if is_tag_search do
        Enum.any?(data.terms, fn term ->
          normalized = normalize_tag(term)
          tag_matches?(normalized, tag_query)
        end)
      else
        q = if case_sensitive, do: query, else: String.downcase(query)
        Enum.any?(data.terms, fn term ->
          t = if case_sensitive, do: term, else: String.downcase(term)
          String.contains?(t, q)
        end)
      end
    end)
  end

  defp calculate_bm25(term_freq, doc_length, avgdl, n, doc_freq) do
    # Avoid division by zero
    doc_freq = max(doc_freq, 1)
    n = max(n, 1)

    # IDF calculation
    idf = :math.log((n - doc_freq + 0.5) / (doc_freq + 0.5) + 1.0)

    # Term frequency component
    tf_component = (term_freq * (@k1 + 1)) / (term_freq + @k1 * (1 - @b + @b * doc_length / avgdl))

    idf * tf_component
  end

  defp find_text_matches(content, query, case_sensitive) do
    q = if case_sensitive, do: query, else: String.downcase(query)

    content
    |> String.split("\n")
    |> Enum.with_index(1)
    |> Enum.flat_map(fn {line, idx} ->
      target = if case_sensitive, do: line, else: String.downcase(line)
      if String.contains?(target, q), do: [%{line: idx, text: String.trim(line)}], else: []
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

  defp normalize_tag(tag), do: String.downcase(tag)

  defp tag_matches?(tag, query) do
    tag == query or String.starts_with?(tag, query <> "/")
  end

  # ── helpers ──────────────────────────────────────────────────────────────────

  defp merge_results(a, b) do
    all = a ++ b

    all
    |> Enum.group_by(& &1.file)
    |> Enum.map(fn {file, entries} ->
      matches = Enum.flat_map(entries, & &1.matches) |> Enum.uniq_by(& &1.line)
      max_score = Enum.max(Enum.map(entries, & &1.score))
      %{file: file, matches: matches, score: max_score}
    end)
  end

  defp format_results([], _total, vault) do
    "Vault: #{vault}\n\nNo results found."
  end

  defp format_results(results, total, vault) do
    header = "Vault: #{vault}\n\nFound #{total} match(es) in #{length(results)} file(s):\n\n"

    body =
      Enum.map_join(results, "\n", fn %{file: file, matches: matches, score: score} ->
        match_lines =
          Enum.map_join(matches, "\n", fn %{line: line, text: text} ->
            if line == 0, do: "  #{text}", else: "  L#{line}: #{text}"
          end)

        "#{file} (score: #{:erlang.float_to_binary(score, decimals: 4)}):\n#{match_lines}"
      end)

    header <> body
  end
end
