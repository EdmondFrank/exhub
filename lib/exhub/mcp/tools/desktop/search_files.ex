defmodule Exhub.MCP.Tools.Desktop.SearchFiles do
  @moduledoc """
  MCP Tool: search_files

  Search for files by name or search within file contents using ripgrep or grep.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers

  use Anubis.Server.Component, type: :tool

  require Logger

  def name, do: "search_files"

  @impl true
  def description do
    """
    Search for files by name or search within file contents.

    Two search types are supported:
    - files: Find files/directories whose names match the pattern
    - content: Find files whose contents contain the pattern

    Uses ripgrep (rg) if available, otherwise falls back to grep for fast searching.
    The pattern is treated as a case-insensitive substring by default.

    Parameters:
    - path: Absolute path to the directory to search in
    - pattern: The search pattern (substring or regex)
    - search_type: "files" (search by filename) or "content" (search inside files), default "files"
    - file_pattern: Optional glob pattern to filter files (e.g. "*.ex"), only for content search
    - ignore_case: Case-insensitive matching (default true)
    - max_results: Maximum number of results to return (default 50)
    - context_lines: Number of context lines around content matches (default 2)
    """
  end

  schema do
    field(:path, {:required, :string}, description: "Absolute path to the directory to search in")
    field(:pattern, {:required, :string}, description: "The search pattern (substring or regex)")
    field(:search_type, :string, description: "\"files\" or \"content\" (default \"files\")", default: "files")
    field(:file_pattern, :string, description: "Optional glob pattern to filter files (e.g. \"*.ex\")")
    field(:ignore_case, :boolean, description: "Case-insensitive matching (default true)", default: true)
    field(:max_results, :integer, description: "Maximum number of results to return (default 50)", default: 50)
    field(:context_lines, :integer, description: "Number of context lines around content matches (default 2)", default: 2)
  end

  @impl true
  def execute(params, frame) do
    path = Map.get(params, :path) |> Helpers.expand_path()
    pattern = Map.get(params, :pattern)
    search_type = Map.get(params, :search_type, "files")
    file_pattern = Map.get(params, :file_pattern)
    ignore_case = Map.get(params, :ignore_case, true)
    max_results = Map.get(params, :max_results, 50)
    context_lines = Map.get(params, :context_lines, 2)

    case do_search(path, pattern, search_type, file_pattern, ignore_case, max_results, context_lines) do
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
          search_content_ripgrep(path, pattern, file_pattern, ignore_case, max_results, context_lines)

        grep_available?() ->
          search_content_grep(path, pattern, file_pattern, ignore_case, max_results, context_lines)

        true ->
          search_content_native(path, pattern, file_pattern, ignore_case, max_results, context_lines)
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
        "--sort", "path"
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

  defp search_content_ripgrep(path, pattern, file_pattern, ignore_case, max_results, context_lines) do
    args =
      [
        "--line-number",
        "--sort", "path",
        "-C", to_string(context_lines)
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
        results = parse_ripgrep_output(output, max_results)
        {:ok, results}

      {:error, reason} ->
        Logger.warning("[SearchFiles] ripgrep content search failed: #{reason}, falling back")
        search_content_native(path, pattern, file_pattern, ignore_case, max_results, context_lines)
    end
  end

  defp add_ripgrep_case_flag(args, true), do: args ++ ["-i"]
  defp add_ripgrep_case_flag(args, false), do: args

  defp add_ripgrep_max_count(args, max) when max > 0, do: args ++ ["-m", to_string(max)]
  defp add_ripgrep_max_count(args, _), do: args

  defp parse_ripgrep_output(output, max_results) do
    output
    |> String.split("\n")
    |> Enum.reject(&(&1 == ""))
    |> Enum.reduce(%{current_file: nil, results: [], file_matches: %{}}, fn line, acc ->
      cond do
        # Match line: path:line:content
        Regex.match?(~r/^(.+):(\d+):(.*)$/, line) ->
          [_, file_path, _line_num, _content] = Regex.run(~r/^(.+):(\d+):(.*)$/, line)

          file_matches = Map.update(acc.file_matches, file_path, [line], &[line | &1])

          if map_size(file_matches) > max_results do
            acc
          else
            %{acc | file_matches: file_matches}
          end

        # Separator line (context separator)
        line == "--" ->
          acc

        # Context line: path-line-content
        Regex.match?(~r/^(.+)-(\d+)-(.*)$/, line) ->
          acc

        true ->
          acc
      end
    end)
    |> then(fn %{file_matches: matches} ->
      matches
      |> Enum.take(max_results)
      |> Enum.map(fn {file_path, lines} ->
        matches =
          lines
          |> Enum.reverse()
          |> Enum.map(fn line ->
            case Regex.run(~r/^(.+):(\d+):(.*)$/, line) do
              [_, _, line_num, content] ->
                %{
                  "line_number" => String.to_integer(line_num),
                  "line" => content,
                  "context" => content
                }

              _ ->
                nil
            end
          end)
          |> Enum.reject(&is_nil/1)

        %{
          "path" => file_path,
          "matches" => matches
        }
      end)
    end)
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
        results = parse_grep_output(output, max_results)
        {:ok, results}

      {:error, reason} ->
        Logger.warning("[SearchFiles] grep failed: #{reason}, falling back to native")
        search_content_native(path, pattern, file_pattern, ignore_case, max_results, context_lines)
    end
  end

  defp parse_grep_output(output, max_results) do
    output
    |> String.split("\n")
    |> Enum.reject(&(&1 == ""))
    |> Enum.reduce(%{}, fn line, acc ->
      # Parse grep -nH output: file:line:content
      case Regex.run(~r/^([^:]+):(\d+):(.*)$/, line) do
        [_, file_path, line_num, content] ->
          Map.update(acc, file_path, [content], fn matches ->
            if length(matches) < 100 do
              [%{"line_number" => String.to_integer(line_num), "line" => content, "context" => content} | matches]
            else
              matches
            end
          end)

        _ ->
          acc
      end
    end)
    |> Enum.take(max_results)
    |> Enum.map(fn {file_path, matches} ->
      %{
        "path" => file_path,
        "matches" => Enum.reverse(matches)
      }
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
        Exile.stream(["sh", "-c", cmd], stderr: :consume, exit_timeout: 5000)
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
    if Regex.match?(~r/['"\s]/, arg) do
      "'" <> String.replace(arg, "'", "'\"'\"'") <> "'"
    else
      arg
    end
  end

  defp escape_shell_pattern(pattern) do
    "'" <> String.replace(pattern, "'", "'\"'\"'") <> "'"
  end
end
