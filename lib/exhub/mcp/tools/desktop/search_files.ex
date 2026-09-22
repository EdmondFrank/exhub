defmodule Exhub.MCP.Tools.Desktop.SearchFiles do
  @moduledoc """
  MCP Tool: search_files

  Search a codebase. The default `semantic` mode uses probe-backed semantic search
  (AST-aware ranking with BM25, returning complete code blocks); `files` and
  `content` modes retain the ripgrep/grep/native filename and literal searches.

  The `probe` binary is resolved from the `:exhub, :probe_binary` config, falling
  back to the first `probe` on the system `PATH`. Different probe builds can rank
  results differently (and run at different speeds), so pin `:probe_binary` when
  reproducible semantic results matter.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers

  use Anubis.Server.Component, type: :tool

  require Logger

  # Semantic search defaults — mirror aider-desk's SemanticSearchTool.
  @semantic_timeout_seconds 300
  @probe_exit_timeout_ms 310_000
  @default_max_tokens 5000

  # Languages accepted by probe's `--language` flag (mutually exclusive with hints).
  @supported_languages ~w(
    rust rs javascript js jsx typescript ts tsx python py go
    c h cpp cc cxx hpp hxx java ruby rb php swift solidity sol
    crystal cr csharp cs yaml yml
  )

  # Rerankers accepted by probe's `--reranker` flag (BERT models need --features).
  @supported_rerankers ~w(
    bm25 hybrid hybrid2 tfidf ms-marco-tinybert ms-marco-minilm-l6 ms-marco-minilm-l12
  )

  # Output formats accepted by probe's `--format` flag (probe defaults to "outline").
  @supported_formats ~w(outline outline-xml terminal markdown plain json xml color)

  def name, do: "search_files"

  @impl true
  def description do
    """
    Search a codebase. Three search types are supported:

    - semantic (default): Probe-backed semantic code search. Treats the codebase as
      code (AST-aware), ranks with BM25 and returns complete code blocks rather than
      line fragments. Uses `query` with Elasticsearch-style syntax: boolean operators
      (AND/OR/NOT), +required/-excluded terms, "exact phrases", and file hints such as
      ext:ts, file:src/**/*.py, dir:tests, lang:typescript.
    - files: Find files/directories whose names match `pattern` (ripgrep, falling back
      to native matching).
    - content: Find files whose contents match `pattern` (ripgrep, falling back to
      grep/native), with configurable context lines.

    Parameters:
    - path: Absolute path or ~ shorthand to the directory to search in
    - search_type: "semantic" (default), "files" or "content"
    - query: Semantic search query (required for search_type "semantic")
    - pattern: Search pattern (required for "files"/"content"; substring or regex)
    - allow_tests: Include test files in semantic results (default false)
    - exact: Exact (untokenized, case-insensitive) semantic search (default false)
    - max_results: Maximum results (files/content default 50; optional for semantic)
    - max_tokens: Maximum tokens of code content returned by semantic search (default 5000)
    - language: Limit semantic search to a language (e.g. "typescript", "python", "rust")
    - reranker: Ranking algorithm for semantic results (probe default "bm25"; also
      "hybrid", "hybrid2", "tfidf", "ms-marco-tinybert", "ms-marco-minilm-l6", "ms-marco-minilm-l12")
    - files_only: Return only matching file names, skipping code blocks (default false)
    - ignore: Glob pattern(s) to exclude, in addition to .gitignore (semantic only)
    - exclude_filenames: Exclude files whose names match query words (default false)
    - frequency: Force frequency-based search with stemming/stopwords (probe default on; only passed when true)
    - strict_elastic_syntax: Require explicit AND/OR operators and quotes (default false)
    - max_bytes: Maximum total bytes of code content returned (semantic only)
    - no_merge: Disable merging of adjacent code blocks (default false)
    - merge_threshold: Max number of lines between blocks to merge (probe default 5)
    - session: Session ID for caching/paginating semantic results
    - format: probe output format (probe default "outline"; also "json", "xml", "markdown", "plain", "terminal", "color")
    - file_pattern: Optional glob pattern to filter files (e.g. "*.ex"), only for content search
    - ignore_case: Case-insensitive matching for files/content (default true)
    - context_lines: Number of context lines around content matches (default 2)
    """
  end

  schema do
    field(:path, {:required, :string},
      description: "Absolute path or ~ shorthand to the directory to search in"
    )

    field(:search_type, :string,
      description: "\"semantic\" (default), \"files\" or \"content\"",
      default: "semantic"
    )

    field(:query, :string,
      description:
        "Semantic search query with Elasticsearch syntax (required for search_type \"semantic\"). Use + for important terms."
    )

    field(:pattern, :string,
      description:
        "The search pattern (substring or regex). Required for search_type \"files\" or \"content\"."
    )

    field(:allow_tests, :boolean,
      description: "Include test files in semantic search results (default false)",
      default: false
    )

    field(:exact, :boolean,
      description: "Exact (tokenization-free, case-insensitive) semantic search (default false)",
      default: false
    )

    field(:max_tokens, :integer,
      description: "Maximum tokens of code content returned by semantic search (default 5000)",
      default: @default_max_tokens
    )

    field(:language, :string,
      description:
        "Limit semantic search to a programming language (e.g. \"typescript\", \"python\", \"rust\")"
    )

    field(:reranker, :string,
      description:
        "Ranking algorithm for semantic results (probe default \"bm25\"; also \"hybrid\", \"hybrid2\", \"tfidf\", \"ms-marco-tinybert\", \"ms-marco-minilm-l6\", \"ms-marco-minilm-l12\")"
    )

    field(:files_only, :boolean,
      description: "Return only matching file names, skipping code blocks (default false)",
      default: false
    )

    field(:ignore, {:list, :string},
      description: "Glob pattern(s) to exclude, in addition to .gitignore (semantic only)"
    )

    field(:exclude_filenames, :boolean,
      description: "Exclude files whose names match query words (default false)",
      default: false
    )

    field(:frequency, :boolean,
      description:
        "Force frequency-based search with stemming/stopwords (probe default on; only passed when true)",
      default: false
    )

    field(:strict_elastic_syntax, :boolean,
      description: "Require explicit AND/OR operators and quotes (default false)",
      default: false
    )

    field(:max_bytes, :integer,
      description: "Maximum total bytes of code content returned (semantic only)"
    )

    field(:no_merge, :boolean,
      description: "Disable merging of adjacent code blocks (default false)",
      default: false
    )

    field(:merge_threshold, :integer,
      description: "Max number of lines between blocks to merge (probe default 5)"
    )

    field(:session, :string, description: "Session ID for caching/paginating semantic results")

    field(:format, :string,
      description:
        "probe output format (probe default \"outline\"; also \"json\", \"xml\", \"markdown\", \"plain\", \"terminal\", \"color\")"
    )

    field(:file_pattern, :string,
      description: "Optional glob pattern to filter files (e.g. \"*.ex\")"
    )

    field(:ignore_case, :boolean,
      description: "Case-insensitive matching for files/content search (default true)",
      default: true
    )

    field(:max_results, :integer,
      description:
        "Maximum number of results to return (default 50 for files/content; optional for semantic)"
    )

    field(:context_lines, :integer,
      description: "Number of context lines around content matches (default 2)",
      default: 2
    )
  end

  @impl true
  def execute(params, frame) do
    with {:ok, path} <- Map.get(params, :path) |> Helpers.validate_absolute_path() do
      search_type = Map.get(params, :search_type, "semantic")

      case search_type do
        "semantic" ->
          run_semantic(params, path, frame)

        type when type in ["files", "content"] ->
          run_pattern_search(params, path, type, frame)

        other ->
          resp =
            Response.tool()
            |> Response.error(
              "Unknown search_type: #{other}. Use \"semantic\", \"files\" or \"content\"."
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
      ignore_case = Map.get(params, :ignore_case, true)
      max_results = Map.get(params, :max_results, 50)
      context_lines = Map.get(params, :context_lines, 2)

      case do_search(
             path,
             pattern,
             search_type,
             file_pattern,
             ignore_case,
             max_results,
             context_lines
           ) do
        {:ok, results} ->
          resp =
            Response.tool()
            |> Helpers.toon_response(%{
              "path" => path,
              "pattern" => pattern,
              "search_type" => search_type,
              "results" => results,
              "count" => length(results)
            })

          {:reply, resp, frame}

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

  defp search_semantic(binary, path, query, params) do
    with :ok <- check_directory(path) do
      max_tokens = Map.get(params, :max_tokens) || @default_max_tokens

      # Flag ordering mirrors aider-desk's probe wrapper (src/main/utils/probe.ts).
      args =
        ["search"]
        |> add_flag(Map.get(params, :files_only, false), "--files-only")
        |> add_ignores(Map.get(params, :ignore))
        |> add_flag(Map.get(params, :exclude_filenames, false), "--exclude-filenames")
        |> add_reranker(Map.get(params, :reranker))
        |> add_frequency(Map.get(params, :frequency, false))
        |> add_flag(Map.get(params, :exact, false), "--exact")
        |> add_flag(Map.get(params, :strict_elastic_syntax, false), "--strict-elastic-syntax")
        |> add_max_results(Map.get(params, :max_results))
        |> add_max_bytes(Map.get(params, :max_bytes))
        |> add_max_tokens(max_tokens)
        |> add_flag(Map.get(params, :allow_tests, false), "--allow-tests")
        |> add_flag(Map.get(params, :no_merge, false), "--no-merge")
        |> add_merge_threshold(Map.get(params, :merge_threshold))
        |> add_session(Map.get(params, :session))
        |> add_timeout(@semantic_timeout_seconds)
        |> add_language(Map.get(params, :language))
        |> add_format(Map.get(params, :format))
        |> Kernel.++(["--", query, path])

      run_probe(binary, args)
    end
  end

  defp add_flag(args, true, flag), do: args ++ [flag]
  defp add_flag(args, _value, _flag), do: args

  defp add_ignores(args, patterns) when is_list(patterns),
    do: Enum.reduce(patterns, args, fn pattern, acc -> add_ignore(acc, pattern) end)

  defp add_ignores(args, pattern), do: add_ignore(args, pattern)

  defp add_ignore(args, pattern) when is_binary(pattern) and pattern != "",
    do: args ++ ["--ignore", pattern]

  defp add_ignore(args, _), do: args

  defp add_reranker(args, reranker) when reranker in @supported_rerankers,
    do: args ++ ["--reranker", reranker]

  defp add_reranker(args, _), do: args

  defp add_frequency(args, true), do: args ++ ["--frequency"]
  defp add_frequency(args, _), do: args

  defp add_max_results(args, max) when is_integer(max) and max > 0,
    do: args ++ ["--max-results", to_string(max)]

  defp add_max_results(args, _), do: args

  defp add_max_bytes(args, max) when is_integer(max) and max > 0,
    do: args ++ ["--max-bytes", to_string(max)]

  defp add_max_bytes(args, _), do: args

  defp add_max_tokens(args, max) when is_integer(max) and max > 0,
    do: args ++ ["--max-tokens", to_string(max)]

  defp add_max_tokens(args, _), do: args

  defp add_merge_threshold(args, threshold) when is_integer(threshold) and threshold >= 0,
    do: args ++ ["--merge-threshold", to_string(threshold)]

  defp add_merge_threshold(args, _), do: args

  defp add_session(args, session) when is_binary(session) and session != "",
    do: args ++ ["--session", session]

  defp add_session(args, _), do: args

  defp add_timeout(args, seconds), do: args ++ ["--timeout", to_string(seconds)]

  defp add_language(args, language) when language in @supported_languages,
    do: args ++ ["--language", language]

  defp add_language(args, _), do: args

  defp add_format(args, format) when format in @supported_formats,
    do: args ++ ["--format", format]

  defp add_format(args, _), do: args

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

  defp do_search(path, pattern, "files", _file_pattern, ignore_case, max_results, _context_lines) do
    with :ok <- check_directory(path) do
      cond do
        ripgrep_available?() ->
          search_files_ripgrep(path, pattern, ignore_case, max_results)

        true ->
          search_files_native(path, pattern, ignore_case, max_results)
      end
    end
  end

  # ============================================================================
  # Content search (search inside files)
  # ============================================================================

  defp do_search(path, pattern, "content", file_pattern, ignore_case, max_results, context_lines) do
    with :ok <- check_directory(path) do
      cond do
        ripgrep_available?() ->
          search_content_ripgrep(
            path,
            pattern,
            file_pattern,
            ignore_case,
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

  defp do_search(_path, _pattern, search_type, _, _, _, _) do
    {:error, "Unknown search_type: #{search_type}. Use \"files\" or \"content\"."}
  end

  # ============================================================================
  # Directory validation
  # ============================================================================

  defp check_directory(path) do
    case File.stat(path) do
      {:ok, %File.Stat{type: :directory}} -> :ok
      {:ok, _} -> {:error, "Not a directory: #{path}"}
      {:error, :enoent} -> {:error, "Directory not found: #{path}"}
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

  defp search_files_ripgrep(path, pattern, ignore_case, max_results) do
    args =
      [
        "--files",
        "--sort",
        "path"
      ]
      |> add_ripgrep_case_flag(ignore_case)
      |> add_ripgrep_max_count(max_results)

    # Escape special regex characters for literal matching
    pattern = Regex.escape(pattern)
    args = args ++ ["-g", "*#{pattern}*", path]

    case run_exile_command(["rg" | args]) do
      {:ok, output} ->
        results =
          output
          |> String.split("\n")
          |> Enum.reject(&(&1 == ""))
          |> Enum.take(max_results)
          |> Enum.map(fn file_path ->
            %{
              "path" => file_path,
              "name" => Path.basename(file_path),
              "type" => if(File.dir?(file_path), do: "directory", else: "file")
            }
          end)

        {:ok, results}

      {:error, reason} ->
        Logger.warning("[SearchFiles] ripgrep failed: #{reason}, falling back to native")
        search_files_native(path, pattern, ignore_case, max_results)
    end
  end

  defp search_content_ripgrep(
         path,
         pattern,
         file_pattern,
         ignore_case,
         max_results,
         context_lines
       ) do
    args =
      [
        "--line-number",
        "--sort",
        "path",
        "-C",
        to_string(context_lines)
      ]
      |> add_ripgrep_case_flag(ignore_case)
      |> add_ripgrep_max_count(max_results)

    args =
      if file_pattern do
        args ++ ["-g", file_pattern]
      else
        args
      end

    args = args ++ [pattern, path]

    case run_exile_command(["rg" | args]) do
      {:ok, output} ->
        results = parse_ripgrep_output(output, max_results, context_lines)
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
      "find #{escape_shell_args(find_args)} | xargs grep #{grep_flags} #{context_flag} #{escape_shell_pattern(pattern)} 2>/dev/null || true"

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
  # Native Elixir implementations (fallback)
  # ============================================================================

  defp search_files_native(dir, pattern, ignore_case, max_results) do
    regex = build_regex(pattern, ignore_case)
    results = find_files_recursive(dir, regex, max_results, [])
    {:ok, Enum.take(results, max_results)}
  end

  defp find_files_recursive(_dir, _regex, 0, acc), do: acc

  defp find_files_recursive(dir, regex, remaining, acc) do
    case File.ls(dir) do
      {:ok, names} ->
        Enum.reduce_while(names, {remaining, acc}, fn name, {rem, results} ->
          full_path = Path.join(dir, name)

          new_results =
            if Regex.match?(regex, name) do
              case File.stat(full_path) do
                {:ok, stat} ->
                  [
                    %{
                      "path" => full_path,
                      "name" => name,
                      "type" => if(stat.type == :directory, do: "directory", else: "file")
                    }
                    | results
                  ]

                _ ->
                  results
              end
            else
              results
            end

          new_rem = rem - (length(new_results) - length(results))

          if new_rem <= 0 do
            {:halt, {0, new_results}}
          else
            final_results =
              case File.stat(full_path) do
                {:ok, %File.Stat{type: :directory}} ->
                  find_files_recursive(full_path, regex, new_rem, new_results)

                _ ->
                  new_results
              end

            {:cont, {new_rem - (length(final_results) - length(new_results)), final_results}}
          end
        end)
        |> elem(1)

      {:error, _} ->
        acc
    end
  end

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

  defp collect_files(dir, file_pattern) do
    case File.ls(dir) do
      {:ok, names} ->
        Enum.flat_map(names, fn name ->
          full_path = Path.join(dir, name)

          case File.stat(full_path) do
            {:ok, %File.Stat{type: :directory}} ->
              collect_files(full_path, file_pattern)

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

  defp run_exile_command(argv) do
    try do
      {stdout, stderr, exit_code} =
        Exile.stream(argv, stderr: :consume, exit_timeout: 5000)
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
