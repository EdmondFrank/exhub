defmodule Exhub.MCP.Tools.Desktop.SearchFiles do
  @moduledoc """
  MCP Tool: search_files

  Search a codebase. The default `semantic` mode uses probe-backed semantic search
  (AST-aware ranking with BM25, returning complete code blocks); `glob` mode
  matches file and directory paths against a glob pattern, returning directories
  suffixed with "/" (mirroring aider-desk's `power_glob`), and `content` mode
  retains the ripgrep/grep/native literal search.

  `path` may name a directory (searched recursively) or a single file; a file
  scopes every mode to that one file.

  The `probe` binary is resolved from the `:exhub, :probe_binary` config, falling
  back to the first `probe` on the system `PATH`. Different probe builds can rank
  results differently (and run at different speeds), so pin `:probe_binary` when
  reproducible semantic results matter.

  Semantic results are filtered for relevance by the Smart Decide (System One)
  model: each code block is judged against the caller's `purpose` (falling back
  to `query`) and only the relevant blocks are kept. See
  `Exhub.MCP.Desktop.Search.Relevance`; disable per-call with `filter: false`.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers
  alias Exhub.MCP.Desktop.Search.Relevance

  use Anubis.Server.Component, type: :tool

  require Logger

  # Semantic search defaults — mirror aider-desk's SemanticSearchTool.
  @semantic_timeout_seconds 300
  @probe_exit_timeout_ms 310_000
  @default_max_tokens 5000

  # Glob search defaults — mirror aider-desk's `power_glob`.
  @default_glob_max_results 1000
  @default_content_max_results 50
  @glob_walk_limit 5000

  # Languages accepted by probe's `--language` flag (mutually exclusive with hints).
  @supported_languages ~w(
    rust rs javascript js jsx typescript ts tsx python py go
    c h cpp cc cxx hpp hxx java ruby rb php swift solidity sol
    crystal cr csharp cs yaml yml
  )

  def name, do: "search_files"

  @impl true
  def description do
    """
    Search a codebase. Three modes, selected with `search_type`:

    - semantic (default): AST-aware BM25 code search over `query`, returning whole
      code blocks. Supports Elasticsearch syntax (AND/OR/NOT, +required, -excluded,
      "phrases") and hints such as ext:ts, file:src/**/*.py, dir:tests,
      lang:typescript. Results are filtered for relevance by the Smart Decide model
      (on by default; set `filter: false` for the raw, unfiltered results),
      judged against the natural-language `purpose` (falling back to `query`).
    - glob: match file and directory paths against a glob `pattern` relative to
      `path` (e.g. `src/**/*.ts`, `*.md`), returning paths relative to `path`.
      Directories are included and suffixed with "/" (set `include_dirs: false`
      for files only; a `pattern` ending in "/" returns directories only).
      Hidden dotfiles and entries matched by .gitignore/.ignore/.rgignore are
      excluded unless `include_ignored` is true; pass `ignore` to exclude extra
      globs.
    - content: match file contents against `pattern`, with `context_lines` of
      surrounding context.

    `query` is required for search_type "semantic"; `pattern` is required for
    search_type "glob" or "content".

    Parameters:
    - path: Absolute path or ~ shorthand to the directory (or a single file) to search in
    - search_type: "semantic" (default), "glob" or "content"
    - query: Semantic search query with Elasticsearch syntax (required for search_type "semantic"). Use + for important terms.
    - purpose: Natural-language purpose of the semantic search, used by the Smart Decide relevance filter to judge each result. Falls back to `query` when omitted.
    - pattern: Required for search_type "glob" or "content". For "glob", a glob pattern relative to `path` (e.g. "src/**/*.ts", "*.md"); a trailing "/" returns directories only. For "content", a substring or regex.
    - allow_tests: Include test files in semantic search results (default false)
    - exact: Exact (tokenization-free, case-insensitive) semantic search (default false)
    - max_results: Maximum number of results to return (default 1000 for glob, 50 for content; optional for semantic)
    - max_tokens: Maximum tokens of code content returned by semantic search (default 5000)
    - language: Limit semantic search to a programming language (e.g. "typescript", "python", "rust")
    - file_pattern: Optional glob pattern to filter files (e.g. "*.ex"; content search only)
    - ignore: Glob patterns to exclude from glob results (e.g. ["**/node_modules/**"])
    - ignore_case: Case-insensitive matching for content search (default true)
    - context_lines: Number of context lines around content matches (default 2)
    - include_ignored: Include files excluded by .gitignore/.ignore/.rgignore and hidden dotfiles (default false)
    - include_dirs: Glob search only: include directories (suffixed with "/") in results (default true)
    - filter: Smart Decide relevance filtering of semantic results (default: true). Set `false` for the raw results
    """
  end

  schema do
    field(:path, {:required, :string},
      description: "Absolute path or ~ shorthand to the directory or file to search in"
    )

    field(:search_type, :string,
      description: "\"semantic\" (default), \"glob\" or \"content\"",
      default: "semantic"
    )

    field(:query, :string,
      description:
        "Semantic search query with Elasticsearch syntax (required for search_type \"semantic\"). Use + for important terms."
    )

    field(:purpose, :string,
      description:
        "Natural-language purpose of the semantic search, used by the Smart Decide relevance filter to judge each result. Falls back to `query` when omitted."
    )

    field(:pattern, :string,
      description:
        "Required for search_type \"glob\" or \"content\". For \"glob\", a glob pattern relative to `path` (e.g. \"src/**/*.ts\", \"*.md\"); for \"content\", a substring or regex."
    )

    field(:allow_tests, :boolean,
      description: "Include test files in semantic search results (default false)",
      default: false
    )

    field(:exact, :boolean,
      description: "Exact (tokenization-free, case-insensitive) semantic search (default false)",
      default: false
    )

    field(:max_results, :integer,
      description:
        "Maximum number of results to return (default 1000 for glob, 50 for content; optional for semantic)"
    )

    field(:max_tokens, :integer,
      description: "Maximum tokens of code content returned by semantic search (default 5000)",
      default: @default_max_tokens
    )

    field(:language, :string,
      description:
        "Limit semantic search to a programming language (e.g. \"typescript\", \"python\", \"rust\")"
    )

    field(:file_pattern, :string,
      description: "Optional glob pattern to filter files (e.g. \"*.ex\"; content search only)"
    )

    field(:ignore, {:list, :string},
      description:
        "Glob patterns to exclude from glob results (e.g. [\"**/node_modules/**\"]). Ignored files are already excluded unless `include_ignored` is true."
    )

    field(:ignore_case, :boolean,
      description: "Case-insensitive matching for content search (default true)",
      default: true
    )

    field(:include_ignored, :boolean,
      description:
        "Include files excluded by .gitignore/.ignore/.rgignore and hidden dotfiles (default false)",
      default: false
    )

    field(:include_dirs, :boolean,
      description:
        "Glob search only: include directories in the results (default true). Directories are suffixed with \"/\"; set false for files only.",
      default: true
    )

    field(:context_lines, :integer,
      description: "Number of context lines around content matches (default 2)",
      default: 2
    )

    field(:filter, :boolean,
      description:
        "Smart Decide relevance filtering of semantic results (default: true). Set `false` for the raw results"
    )
  end

  @impl true
  def execute(params, frame) do
    with {:ok, path} <- Map.get(params, :path) |> Helpers.validate_absolute_path() do
      search_type = Map.get(params, :search_type, "semantic")

      case search_type do
        "semantic" ->
          run_semantic(params, path, frame)

        type when type in ["glob", "content"] ->
          run_pattern_search(params, path, type, frame)

        other ->
          resp =
            Response.tool()
            |> Response.error(
              "Unknown search_type: #{other}. Use \"semantic\", \"glob\" or \"content\"."
            )

          {:reply, resp, frame}
      end
    else
      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end

  # ============================================================================
  # Filename / content search (ripgrep, with grep/native fallbacks)
  # ============================================================================

  defp run_pattern_search(params, path, search_type, frame) do
    pattern = Map.get(params, :pattern)

    if is_nil(pattern) or pattern == "" do
      resp =
        Response.tool()
        |> Response.error(
          "Missing required parameter: pattern (for search_type \"#{search_type}\")."
        )

      {:reply, resp, frame}
    else
      file_pattern = Map.get(params, :file_pattern)
      ignore = Map.get(params, :ignore, [])
      ignore_case = Map.get(params, :ignore_case, true)
      include_ignored = Map.get(params, :include_ignored, false)
      include_dirs = Map.get(params, :include_dirs, true)
      max_results = Map.get(params, :max_results) || default_max_results(search_type)
      context_lines = Map.get(params, :context_lines, 2)

      case do_search(
             path,
             pattern,
             search_type,
             file_pattern,
             ignore_case,
             include_ignored,
             max_results,
             context_lines,
             ignore,
             include_dirs
           ) do
        {:ok, results} ->
          results_response(
            path,
            pattern,
            search_type,
            results,
            limit_notice(results, max_results),
            frame
          )

        {:ok, results, extra} ->
          results_response(path, pattern, search_type, results, extra, frame)

        {:error, reason} ->
          resp = Response.tool() |> Response.error("Search failed: #{reason}")
          {:reply, resp, frame}
      end
    end
  end

  # ============================================================================
  # Semantic search (probe CLI)
  # ============================================================================

  defp run_semantic(params, path, frame) do
    query = Map.get(params, :query)
    binary = probe_binary()
    filter? = filter?(Map.get(params, :filter))

    cond do
      is_nil(query) or String.trim(query) == "" ->
        resp =
          Response.tool()
          |> Response.error("Missing required parameter: query (for search_type \"semantic\").")

        {:reply, resp, frame}

      is_nil(binary) ->
        resp =
          Response.tool()
          |> Response.error(
            "The 'probe' binary was not found. Install it (https://github.com/probelabs/probe), " <>
              "put it on the system PATH, or pin it via :probe_binary in the :exhub app config."
          )

        {:reply, resp, frame}

      filter? ->
        run_semantic_filtered(binary, path, query, params, frame)

      true ->
        case search_semantic(binary, path, query, params) do
          {:ok, output} ->
            {:reply, Response.tool() |> Response.text(output), frame}

          {:error, reason} ->
            resp = Response.tool() |> Response.error("Semantic search failed: #{reason}")
            {:reply, resp, frame}
        end
    end
  end

  # ── Smart Decide relevance filter ─────────────────────────────────────────

  # `filter: false` skips the Smart Decide pass; absent (nil) follows config.
  defp filter?(nil), do: Relevance.enabled?()
  defp filter?(value), do: value == true

  # The judged task is the caller's `purpose` when it carries text, else the raw
  # query; a blank `purpose` must not silently disable filtering.
  defp effective_purpose(params, query) do
    case Map.get(params, :purpose) do
      purpose when is_binary(purpose) ->
        if String.trim(purpose) == "", do: query, else: purpose

      _ ->
        query
    end
  end

  defp run_semantic_filtered(binary, path, query, params, frame) do
    case search_semantic_json(binary, path, query, params) do
      {:ok, results} ->
        task = effective_purpose(params, query)
        {relevant, stats} = Relevance.filter(task, results, Relevance.config())
        kept = narrow(relevant, positive_int(Map.get(params, :max_results)))
        output = format_semantic_results(kept, path, query, stats)
        {:reply, Response.tool() |> Response.text(output), frame}

      # Fail open: an unparsable pool (e.g. an older probe build) falls back to
      # the raw outline run rather than returning an empty result set.
      {:error, :unparsable} ->
        case search_semantic(binary, path, query, params) do
          {:ok, output} ->
            {:reply, Response.tool() |> Response.text(output), frame}

          {:error, reason} ->
            resp = Response.tool() |> Response.error("Semantic search failed: #{reason}")
            {:reply, resp, frame}
        end

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Semantic search failed: #{reason}")
        {:reply, resp, frame}
    end
  end

  # When filtering, widen the probe pool so the model has enough to choose from;
  # the caller's `max_results` narrows the judged set back afterwards.
  defp request_max_results(params, true) do
    configured = Keyword.get(Relevance.config(), :candidate_limit, 20)
    max(positive_int(Map.get(params, :max_results)) || 0, configured)
  end

  defp request_max_results(params, false), do: positive_int(Map.get(params, :max_results))

  defp search_semantic(binary, path, query, params) do
    with :ok <- check_searchable_path(path) do
      args =
        ["search"]
        |> add_flag(Map.get(params, :exact, false), "--exact")
        |> add_flag(Map.get(params, :allow_tests, false), "--allow-tests")
        |> put_arg("--max-results", request_max_results(params, false))
        |> put_arg(
          "--max-tokens",
          positive_int(Map.get(params, :max_tokens)) || @default_max_tokens
        )
        |> put_arg("--language", supported_language(Map.get(params, :language)))
        |> put_arg("--timeout", @semantic_timeout_seconds)
        |> Kernel.++(["--", query, path])

      run_probe(binary, args)
    end
  end

  # Structured probe run used by the Smart Decide pass; `-o json` yields the
  # candidate maps (file, code, lines, owner_symbol) the filter judges.
  defp search_semantic_json(binary, path, query, params) do
    with :ok <- check_searchable_path(path) do
      args =
        ["search"]
        |> add_flag(Map.get(params, :exact, false), "--exact")
        |> add_flag(Map.get(params, :allow_tests, false), "--allow-tests")
        |> put_arg("--max-results", request_max_results(params, true))
        |> put_arg(
          "--max-tokens",
          positive_int(Map.get(params, :max_tokens)) || @default_max_tokens
        )
        |> put_arg("--language", supported_language(Map.get(params, :language)))
        |> put_arg("--timeout", @semantic_timeout_seconds)
        |> put_arg("-o", "json")
        |> Kernel.++(["--", query, path])

      case run_probe(binary, args) do
        {:ok, output} -> parse_probe_results(output)
        {:error, reason} -> {:error, reason}
      end
    end
  end

  # Probe prints human-readable preamble lines before the pretty-printed JSON
  # object, which starts with `{` on its own line; decode from there.
  defp parse_probe_results(output) do
    with {:ok, json} <- extract_json(output),
         {:ok, decoded} <- Jason.decode(json) do
      {:ok, extract_results(decoded)}
    else
      _ -> {:error, :unparsable}
    end
  end

  defp extract_json(output) do
    lines = String.split(output, "\n")

    case Enum.find_index(lines, &(&1 == "{")) do
      nil -> :error
      index -> {:ok, lines |> Enum.drop(index) |> Enum.join("\n") |> String.trim_trailing()}
    end
  end

  defp extract_results(decoded) when is_map(decoded) do
    case Map.get(decoded, "results") do
      results when is_list(results) -> Enum.filter(results, &is_map/1)
      _ -> []
    end
  end

  defp extract_results(_decoded), do: []

  defp narrow(results, max) when is_integer(max) and max > 0, do: Enum.take(results, max)
  defp narrow(results, _max), do: results

  defp format_semantic_results([], _path, _query, _stats), do: "No results found."

  defp format_semantic_results(results, path, query, stats) do
    header = ["Pattern: #{query}", "Path: #{path}"] ++ filter_lines(stats) ++ ["---"]
    body = Enum.map_join(results, "\n---\n", &render_result/1)
    Enum.join(header, "\n") <> "\n" <> body
  end

  defp render_result(result) do
    file = Map.get(result, "file", "unknown")
    code = Map.get(result, "code", "")
    symbol = Map.get(result, "owner_symbol")
    lines = Map.get(result, "lines", [])

    header =
      case {symbol, lines} do
        {s, [first, last]} when is_binary(s) and s != "" ->
          "File: #{file} (#{s}, lines #{first}-#{last})"

        {_, [first, last]} ->
          "File: #{file} (lines #{first}-#{last})"

        _ ->
          "File: #{file}"
      end

    "#{header}\n\n#{code}"
  end

  defp filter_lines(%{filtered: true, fallback: true}) do
    ["Smart Decide relevance filter found nothing relevant — showing ranked results."]
  end

  defp filter_lines(%{filtered: true, skipped: skipped} = stats) when skipped > 0 do
    [
      "Smart Decide relevance filter judged #{stats.relevant - skipped}/#{stats.candidates - skipped} " <>
        "result(s) relevant and kept #{skipped} oversized result(s) unjudged."
    ]
  end

  defp filter_lines(%{filtered: true} = stats) do
    [
      "Smart Decide relevance filter judged #{stats.relevant}/#{stats.candidates} result(s) relevant."
    ]
  end

  defp filter_lines(_stats), do: []

  defp add_flag(args, true, flag), do: args ++ [flag]
  defp add_flag(args, _value, _flag), do: args

  defp put_arg(args, _flag, nil), do: args
  defp put_arg(args, flag, value), do: args ++ [flag, to_string(value)]

  defp positive_int(n) when is_integer(n) and n > 0, do: n
  defp positive_int(_), do: nil

  defp supported_language(language) when language in @supported_languages, do: language
  defp supported_language(_language), do: nil

  defp probe_binary do
    Application.get_env(:exhub, :probe_binary) || System.find_executable("probe")
  end

  defp run_probe(binary, args) do
    {stdout, stderr, exit_code} =
      Exile.stream([binary | args],
        stderr: :consume,
        env: Helpers.clean_env(),
        exit_timeout: @probe_exit_timeout_ms
      )
      |> Enum.reduce({"", "", nil}, fn
        {:stdout, data}, {out, err, code} -> {out <> data, err, code}
        {:stderr, data}, {out, err, code} -> {out, err <> data, code}
        {:exit, {:status, code}}, {out, err, _} -> {out, err, code}
        {:exit, :epipe}, {out, err, _} -> {out, err, 0}
        _, acc -> acc
      end)

    cond do
      exit_code not in [nil, 0, 1] ->
        {:error, "probe (#{binary}) exited with code #{exit_code}: #{String.trim(stderr)}"}

      true ->
        output = String.trim_trailing(stdout)
        {:ok, if(output == "", do: "No results found.", else: output)}
    end
  rescue
    e -> {:error, Exception.message(e)}
  end

  # ============================================================================
  # File name search (find files by name)
  # ============================================================================

  defp do_search(
         path,
         pattern,
         "glob",
         _file_pattern,
         _ignore_case,
         include_ignored,
         max_results,
         _context_lines,
         ignore,
         include_dirs
       ) do
    with :ok <- check_searchable_path(path) do
      search_glob(path, pattern, ignore, include_ignored, include_dirs, max_results)
    end
  end

  # ============================================================================
  # Content search (search inside files)
  # ============================================================================

  defp do_search(
         path,
         pattern,
         "content",
         file_pattern,
         ignore_case,
         include_ignored,
         max_results,
         context_lines,
         _ignore,
         _include_dirs
       ) do
    with :ok <- check_searchable_path(path) do
      cond do
        ripgrep_available?() ->
          search_content_ripgrep(
            path,
            pattern,
            file_pattern,
            ignore_case,
            include_ignored,
            max_results,
            context_lines
          )

        grep_available?() ->
          search_content_grep(
            path,
            pattern,
            file_pattern,
            ignore_case,
            max_results,
            context_lines
          )

        true ->
          search_content_native(
            path,
            pattern,
            file_pattern,
            ignore_case,
            max_results,
            context_lines
          )
      end
    end
  end

  defp do_search(_path, _pattern, search_type, _, _, _, _, _, _, _) do
    {:error, "Unknown search_type: #{search_type}. Use \"glob\" or \"content\"."}
  end

  # ============================================================================
  # Directory validation
  # ============================================================================

  # Accepts a directory or a single regular file; a file path scopes the search
  # to that one file (all three search modes support it).
  defp check_searchable_path(path) do
    case File.stat(path) do
      {:ok, %File.Stat{type: type}} when type in [:directory, :regular] -> :ok
      {:ok, _} -> {:error, "Not a file or directory: #{path}"}
      {:error, :enoent} -> {:error, "Path not found: #{path}"}
      {:error, reason} -> {:error, inspect(reason)}
    end
  end

  # ============================================================================
  # Tool availability checks
  # ============================================================================

  defp ripgrep_available? do
    case System.find_executable("rg") do
      nil -> false
      _ -> true
    end
  end

  defp grep_available? do
    case System.find_executable("grep") do
      nil -> false
      _ -> true
    end
  end

  # ============================================================================
  # Ripgrep implementations
  # ============================================================================

  defp search_glob(path, pattern, ignore, include_ignored, include_dirs, max_results) do
    # A trailing slash is the npm-glob convention for "directories only".
    dirs_only = String.ends_with?(pattern, "/")
    glob = String.trim_trailing(pattern, "/")
    want_dirs = include_dirs or dirs_only
    want_files = not dirs_only

    with {:ok, regex} <- compile_glob(glob),
         {:ok, entries} <- glob_entries(path, include_ignored) do
      ignore_regexes =
        ignore
        |> Enum.map(&compile_glob/1)
        |> Enum.flat_map(fn
          {:ok, regex} -> [regex]
          {:error, _reason} -> []
        end)

      walked =
        entries
        |> Enum.filter(fn {entry, type} ->
          Regex.match?(regex, entry) and
            ((type == :dir and want_dirs) or (type == :file and want_files))
        end)
        |> Enum.reject(fn {entry, _type} ->
          Enum.any?(ignore_regexes, &Regex.match?(&1, entry))
        end)
        |> Enum.map(fn {entry, type} -> if type == :dir, do: entry <> "/", else: entry end)
        |> Enum.sort()
        |> Enum.take(@glob_walk_limit)

      {results, extra} = finalize_glob(walked, max_results)

      {:ok, results, extra}
    end
  end

  # Candidate entries (`{relative_path, :file | :dir}`) under `path`. ripgrep
  # lists files only, so directories are inferred as the ancestors of the files
  # ripgrep reports, which inherits its .gitignore/.ignore/hidden pruning for
  # free. A directory with no non-ignored file (e.g. an empty one) is therefore
  # not discoverable this way.
  defp glob_entries(path, include_ignored) do
    cond do
      # An explicitly-named file is its own candidate set: ripgrep's `--files`
      # walk needs a directory to descend into, and explicit paths bypass ignore
      # rules, so no gitignore/hidden pruning is applied here.
      File.regular?(path) ->
        {:ok, [{Path.basename(path), :file}]}

      ripgrep_available?() ->
        case rg_file_list(path, include_ignored) do
          {:ok, files} ->
            dirs = files |> Enum.flat_map(&ancestor_dirs/1) |> MapSet.new() |> MapSet.to_list()

            {:ok, Enum.map(files, &{&1, :file}) ++ Enum.map(dirs, &{&1, :dir})}

          {:error, reason} ->
            Logger.warning(
              "[SearchFiles] ripgrep glob listing failed: #{inspect(reason)}, falling back to native"
            )

            {:ok, native_entries(path, include_ignored)}
        end

      true ->
        {:ok, native_entries(path, include_ignored)}
    end
  end

  # Run ripgrep from `path` (searching `.`) so entries come back relative to the
  # search root; `--sort path` keeps the walk order deterministic.
  defp rg_file_list(path, include_ignored) do
    args =
      ["--files", "--sort", "path"]
      |> Kernel.++(rg_ignore_flags(include_ignored))
      |> Kernel.++(["--", "."])

    Logger.debug("[SearchFiles] rg glob argv (cd=#{path}): " <> Enum.join(["rg" | args], " "))

    case run_exile_command(["rg" | args], cd: path) do
      {:ok, output} ->
        {:ok,
         output
         |> String.split("\n")
         |> Enum.reject(&(&1 == ""))
         |> Enum.map(&strip_dot_prefix/1)}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp ancestor_dirs(relative_file), do: ancestor_chain(Path.dirname(relative_file), [])

  defp ancestor_chain(dir, acc) when dir in [".", "", "/"], do: acc
  defp ancestor_chain(dir, acc), do: ancestor_chain(Path.dirname(dir), [dir | acc])

  # Cap the walk, then the returned count, and report truncation rather than
  # silently dropping entries (mirrors `power_glob`'s walk/result limits).
  defp finalize_glob(walked, max_results) do
    total = length(walked)

    extra =
      cond do
        total >= @glob_walk_limit ->
          %{
            "limit_reached" => true,
            "notice" =>
              "walk limit of #{@glob_walk_limit} entries reached (results may be incomplete). Refine your pattern for more specific results."
          }

        total > max_results ->
          %{
            "limit_reached" => true,
            "notice" =>
              "results truncated at #{max_results} entries. Refine your pattern for more specific results."
          }

        true ->
          %{}
      end

    {Enum.take(walked, max_results), extra}
  end

  # Native fallback when ripgrep is unavailable: `Path.wildcard/2` supports the
  # same `*`, `**`, `?`, `[...]` and `{a,b}` syntax and yields directories too.
  # It cannot honor gitignore rules, so `include_ignored` only toggles hidden
  # (`match_dot`) matching here.
  defp native_entries(dir, include_ignored) do
    dir
    |> Path.join("**")
    |> Path.wildcard(match_dot: include_ignored)
    |> Enum.map(fn abs ->
      {relative_path(abs, dir), if(File.dir?(abs), do: :dir, else: :file)}
    end)
  end

  # Translate a glob into an anchored regexp over a path relative to the search
  # root. Supports `*` (within a segment), `**` (across segments), `?`, `[...]`
  # and `{a,b}`. A pattern without a slash is anchored, so `*.ex` matches only
  # root-level entries (mirrors npm glob / `power_glob`).
  defp compile_glob(glob), do: Regex.compile("^" <> glob_source(glob) <> "$")

  defp glob_source("**/" <> rest), do: "(?:[^/]+/)*" <> glob_source(rest)
  defp glob_source("**" <> rest), do: ".*" <> glob_source(rest)
  defp glob_source("*" <> rest), do: "[^/]*" <> glob_source(rest)
  defp glob_source("?" <> rest), do: "[^/]" <> glob_source(rest)

  defp glob_source("{" <> rest) do
    case split_at(rest, "}") do
      {alternatives, tail} ->
        "(?:" <>
          Enum.map_join(String.split(alternatives, ","), "|", &glob_source/1) <>
          ")" <> glob_source(tail)

      :error ->
        "\\{" <> glob_source(rest)
    end
  end

  defp glob_source("[" <> rest) do
    case split_at(rest, "]") do
      {inner, tail} -> "[" <> class_body(inner) <> "]" <> glob_source(tail)
      :error -> "\\[" <> glob_source(rest)
    end
  end

  defp glob_source(<<char::utf8, rest::binary>>),
    do: Regex.escape(<<char::utf8>>) <> glob_source(rest)

  defp glob_source(""), do: ""

  # `[!abc]` is a negated glob class; everything else passes through verbatim.
  defp class_body("!" <> rest), do: "^" <> rest
  defp class_body(inner), do: inner

  defp split_at(string, delimiter) do
    case :binary.match(string, delimiter) do
      {index, 1} ->
        <<head::binary-size(index), _delimiter::binary-size(1), tail::binary>> = string
        {head, tail}

      :nomatch ->
        :error
    end
  end

  defp relative_path(file, root) do
    String.replace_prefix(file, String.trim_trailing(root, "/") <> "/", "")
  end

  defp strip_dot_prefix("./" <> rest), do: rest
  defp strip_dot_prefix(path), do: path

  defp search_content_ripgrep(
         path,
         pattern,
         file_pattern,
         ignore_case,
         include_ignored,
         max_results,
         context_lines
       ) do
    args =
      [
        "--no-heading",
        # Force the filename prefix even for a single explicit file, which
        # ripgrep would otherwise omit, breaking the line parser below.
        "--with-filename",
        "--line-number",
        "--color",
        "never",
        "--max-columns",
        "2000",
        "--max-columns-preview",
        "--sort",
        "path",
        "-C",
        to_string(context_lines)
      ]
      |> add_ripgrep_case_flag(ignore_case)
      |> add_ripgrep_max_count(max_results)
      |> Kernel.++(rg_ignore_flags(include_ignored))

    # -g preserves ripgrep's glob semantics for file_pattern, but it also
    # overrides .gitignore/hidden rules, so ignored results are dropped
    # post-hoc below unless the caller opted into them.
    args = if file_pattern, do: args ++ ["-g", file_pattern], else: args
    args = args ++ ["--", pattern, path]

    Logger.debug("[SearchFiles] rg content argv: " <> Enum.join(["rg" | args], " "))

    case run_exile_command(["rg" | args]) do
      {:ok, output} ->
        results =
          output
          |> parse_ripgrep_output(max_results, context_lines)
          |> drop_ignored(path, file_pattern, include_ignored)

        {:ok, results}

      {:error, reason} ->
        Logger.warning("[SearchFiles] ripgrep content search failed: #{reason}, falling back")

        search_content_native(
          path,
          pattern,
          file_pattern,
          ignore_case,
          max_results,
          context_lines
        )
    end
  end

  defp add_ripgrep_case_flag(args, true), do: args ++ ["-i"]
  defp add_ripgrep_case_flag(args, false), do: args

  defp add_ripgrep_max_count(args, max) when max > 0, do: args ++ ["-m", to_string(max)]
  defp add_ripgrep_max_count(args, _), do: args

  defp parse_ripgrep_output(output, max_results, context_lines) do
    parse_grep_like_output(output, max_results, context_lines)
  end

  # ============================================================================
  # Grep implementation (fallback for content search)
  # ============================================================================

  defp search_content_grep(path, pattern, file_pattern, ignore_case, max_results, context_lines) do
    # Build find command to locate files, then grep through them
    find_args = [path, "-type", "f"]

    find_args =
      if file_pattern do
        find_args ++ ["-name", file_pattern]
      else
        find_args
      end

    grep_flags = if ignore_case, do: "-inH", else: "-nH"
    context_flag = "-C#{context_lines}"

    # Use find piped to xargs grep for efficiency
    cmd =
      "find #{escape_shell_args(find_args)} | xargs grep #{grep_flags} #{context_flag} -- #{escape_shell_pattern(pattern)} 2>/dev/null || true"

    case run_shell_command(cmd) do
      {:ok, output} ->
        results = parse_grep_output(output, max_results, context_lines)
        {:ok, results}

      {:error, reason} ->
        Logger.warning("[SearchFiles] grep failed: #{reason}, falling back to native")

        search_content_native(
          path,
          pattern,
          file_pattern,
          ignore_case,
          max_results,
          context_lines
        )
    end
  end

  defp parse_grep_output(output, max_results, context_lines) do
    parse_grep_like_output(output, max_results, context_lines)
  end

  # ============================================================================
  # Shared parsing for grep-like output (ripgrep and grep)
  # ============================================================================

  defp parse_grep_like_output(output, max_results, context_lines) do
    lines =
      output
      |> String.split("\n")
      |> Enum.reject(&(&1 == ""))

    # Extract only match lines (skip context lines and "--" separators)
    entries =
      Enum.flat_map(lines, fn line ->
        if Regex.match?(~r/^(.+):(\d+):(.*)$/, line) do
          [_, file_path, line_num, content] = Regex.run(~r/^(.+):(\d+):(.*)$/, line)
          [{file_path, String.to_integer(line_num), content}]
        else
          []
        end
      end)

    # Group by file_path, take up to max_results files
    entries
    |> Enum.group_by(&elem(&1, 0), &{elem(&1, 1), elem(&1, 2)})
    |> Enum.take(max_results)
    |> Enum.map(fn {file_path, matches} ->
      # Read file content to build independent context windows
      file_lines =
        case File.read(file_path) do
          {:ok, content} -> String.split(content, "\n")
          {:error, _} -> []
        end

      total_lines = length(file_lines)

      sorted_matches =
        matches
        |> Enum.sort_by(&elem(&1, 0))
        |> Enum.map(fn {line_num, line_content} ->
          start_line = max(1, line_num - context_lines)
          end_line = min(total_lines, line_num + context_lines)

          context =
            file_lines
            |> Enum.slice((start_line - 1)..(end_line - 1))
            |> Enum.with_index(start_line)
            |> Enum.map(fn {l, n} ->
              if n == line_num do
                "=> #{n}: #{l}"
              else
                "   #{n}: #{l}"
              end
            end)
            |> Enum.join("\n")

          %{
            "line_number" => line_num,
            "line" => line_content,
            "context" => context
          }
        end)

      %{"path" => file_path, "matches" => sorted_matches}
    end)
  end

  # ============================================================================
  # Ignore handling / result limiting
  # ============================================================================

  # ripgrep's -g/--glob overrides .gitignore/hidden rules, so a content search
  # that passes file_pattern can surface ignored files. Drop any result ripgrep
  # would not search by default, unless the caller opted into ignored files.
  defp drop_ignored(results, _path, _file_pattern, true), do: results
  defp drop_ignored(results, _path, nil, _include_ignored), do: results

  defp drop_ignored(results, path, _file_pattern, false) do
    case visible_files(path) do
      :unknown -> results
      visible -> Enum.filter(results, fn %{"path" => p} -> MapSet.member?(visible, p) end)
    end
  end

  # Files ripgrep searches under default ignore rules (gitignore + hidden).
  defp visible_files(path) do
    case run_exile_command(["rg", "--files", "--", path]) do
      {:ok, output} ->
        output
        |> String.split("\n")
        |> Enum.reject(&(&1 == ""))
        |> MapSet.new()

      {:error, _reason} ->
        :unknown
    end
  end

  defp rg_ignore_flags(true), do: ["--no-ignore", "--hidden"]
  defp rg_ignore_flags(_include_ignored), do: []

  defp limit_notice(results, max_results)
       when is_integer(max_results) and max_results > 0 and length(results) >= max_results do
    %{
      "limit_reached" => true,
      "notice" =>
        "#{max_results} result limit reached. Use max_results=#{max_results * 2} for more, or refine pattern/file_pattern."
    }
  end

  defp limit_notice(_results, _max_results), do: %{}

  defp default_max_results("glob"), do: @default_glob_max_results
  defp default_max_results(_search_type), do: @default_content_max_results

  defp results_response(path, pattern, search_type, results, extra, frame) do
    payload =
      %{
        "path" => path,
        "pattern" => pattern,
        "search_type" => search_type,
        "results" => results,
        "count" => length(results)
      }
      |> Map.merge(extra)

    resp = Response.tool() |> Helpers.toon_response(payload)

    {:reply, resp, frame}
  end

  # ============================================================================
  # Native Elixir implementations (fallback)
  # ============================================================================

  defp search_content_native(dir, pattern, file_pattern, ignore_case, max_results, context_lines) do
    regex = build_regex(pattern, ignore_case)

    all_files = collect_files(dir, file_pattern)

    results =
      all_files
      |> Enum.reduce_while([], fn file_path, acc ->
        if length(acc) >= max_results do
          {:halt, acc}
        else
          matches = search_in_file(file_path, regex, context_lines)

          if matches == [] do
            {:cont, acc}
          else
            {:cont,
             [
               %{
                 "path" => file_path,
                 "matches" => matches
               }
               | acc
             ]}
          end
        end
      end)

    {:ok, results}
  end

  # A directly-named regular file is its own result set; a directory is walked
  # recursively (honoring file_pattern).
  defp collect_files(path, file_pattern) do
    case File.stat(path) do
      {:ok, %File.Stat{type: :regular}} ->
        if matches_file_pattern?(Path.basename(path), file_pattern), do: [path], else: []

      {:ok, %File.Stat{type: :directory}} ->
        collect_dir_files(path, file_pattern)

      _ ->
        []
    end
  end

  defp collect_dir_files(dir, file_pattern) do
    case File.ls(dir) do
      {:ok, names} ->
        Enum.flat_map(names, fn name ->
          full_path = Path.join(dir, name)

          case File.stat(full_path) do
            {:ok, %File.Stat{type: :directory}} ->
              collect_dir_files(full_path, file_pattern)

            {:ok, %File.Stat{type: :regular}} ->
              if matches_file_pattern?(name, file_pattern), do: [full_path], else: []

            _ ->
              []
          end
        end)

      {:error, _} ->
        []
    end
  end

  defp matches_file_pattern?(_name, nil), do: true
  defp matches_file_pattern?(_name, ""), do: true

  defp matches_file_pattern?(name, pattern) do
    regex_str =
      pattern
      |> Regex.escape()
      |> String.replace("\\*", ".*")
      |> String.replace("\\?", ".")

    case Regex.compile("^#{regex_str}$", [:caseless]) do
      {:ok, regex} -> Regex.match?(regex, name)
      _ -> true
    end
  end

  defp search_in_file(file_path, regex, context_lines) do
    case File.read(file_path) do
      {:ok, content} ->
        lines = String.split(content, "\n")
        total = length(lines)

        lines
        |> Enum.with_index(1)
        |> Enum.filter(fn {line, _idx} -> Regex.match?(regex, line) end)
        |> Enum.map(fn {line, idx} ->
          start_line = max(1, idx - context_lines)
          end_line = min(total, idx + context_lines)

          context =
            lines
            |> Enum.slice((start_line - 1)..(end_line - 1))
            |> Enum.with_index(start_line)
            |> Enum.map(fn {l, n} -> "#{n}: #{l}" end)
            |> Enum.join("\n")

          %{
            "line_number" => idx,
            "line" => line,
            "context" => context
          }
        end)

      {:error, _} ->
        []
    end
  end

  # ============================================================================
  # Helpers
  # ============================================================================

  defp build_regex(pattern, ignore_case) do
    flags = if ignore_case, do: [:caseless], else: []

    case Regex.compile(pattern, flags) do
      {:ok, regex} -> regex
      {:error, _} -> Regex.compile!(Regex.escape(pattern), flags)
    end
  end

  defp run_exile_command(argv, opts \\ []) do
    stream_opts = Keyword.merge([stderr: :consume, exit_timeout: 5000], opts)

    try do
      {stdout, stderr, exit_code} =
        Exile.stream(argv, stream_opts)
        |> Enum.reduce({"", "", 0}, fn
          {:stdout, data}, {out, err, code} -> {out <> data, err, code}
          {:stderr, data}, {out, err, code} -> {out, err <> data, code}
          {:exit, {:status, code}}, {out, err, _} -> {out, err, code}
          {:exit, :epipe}, {out, err, _} -> {out, err, 0}
          _, acc -> acc
        end)

      if exit_code in [0, 1] do
        # Exit code 1 for rg means no matches found, which is OK
        {:ok, stdout}
      else
        {:error, "Command failed with exit #{exit_code}: #{stderr}"}
      end
    rescue
      e -> {:error, Exception.message(e)}
    end
  end

  defp run_shell_command(cmd) do
    try do
      {stdout, _stderr, _exit_code} =
        Exile.stream(Helpers.shell_command_args(cmd, login: true),
          stderr: :consume,
          exit_timeout: 5000
        )
        |> Enum.reduce({"", "", 0}, fn
          {:stdout, data}, {out, err, code} -> {out <> data, err, code}
          {:stderr, data}, {out, err, code} -> {out, err <> data, code}
          {:exit, {:status, code}}, {out, err, _} -> {out, err, code}
          {:exit, :epipe}, {out, err, _} -> {out, err, 0}
          _, acc -> acc
        end)

      # Shell commands with || true always return 0
      {:ok, stdout}
    rescue
      e -> {:error, Exception.message(e)}
    end
  end

  defp escape_shell_args(args) do
    args |> Enum.map(&escape_shell_arg/1) |> Enum.join(" ")
  end

  defp escape_shell_arg(arg) do
    if Regex.match?(~r/['"\s\*\?\[\]\{\}]/, arg) do
      "'" <> String.replace(arg, "'", "'\"'\"'") <> "'"
    else
      arg
    end
  end

  defp escape_shell_pattern(pattern) do
    "'" <> String.replace(pattern, "'", "'\"'\"'") <> "'"
  end
end
