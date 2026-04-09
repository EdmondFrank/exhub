defmodule Exhub.MCP.Tools.Brain.SearchVault do
  @moduledoc """
  MCP Tool: brain_search_vault

  Search for notes in the Obsidian vault by content, filename, or tags.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Brain.Helpers

  use Anubis.Server.Component, type: :tool

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
  end

  @impl true
  def execute(params, frame) do
    query = Map.get(params, :query)
    scope_path = Map.get(params, :path)
    search_type = Map.get(params, :search_type, "content")
    case_sensitive = Map.get(params, :case_sensitive, false)

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
        [%{file: file, matches: [%{line: 0, text: "Filename match: #{file}"}]}]
      else
        []
      end
    end)
  end

  # ── content search ───────────────────────────────────────────────────────────

  defp search_content(vault, _search_dir, files, query, case_sensitive) do
    is_tag_search = String.starts_with?(query, "tag:")
    tag_query = if is_tag_search, do: String.slice(query, 4..-1//1) |> normalize_tag(), else: nil

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

          if matches == [], do: [], else: [%{file: rel_path, matches: matches}]

        {:error, _} ->
          []
      end
    end)
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
      %{file: file, matches: matches}
    end)
  end

  defp format_results([], _total, vault) do
    "Vault: #{vault}\n\nNo results found."
  end

  defp format_results(results, total, vault) do
    header = "Vault: #{vault}\n\nFound #{total} match(es) in #{length(results)} file(s):\n\n"

    body =
      Enum.map_join(results, "\n", fn %{file: file, matches: matches} ->
        match_lines =
          Enum.map_join(matches, "\n", fn %{line: line, text: text} ->
            if line == 0, do: "  #{text}", else: "  L#{line}: #{text}"
          end)

        "#{file}:\n#{match_lines}"
      end)

    header <> body
  end
end
