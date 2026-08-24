defmodule Exhub.BlinkSearch.Backend do
  @moduledoc """
  Behaviour for blink-search backends.

  Each backend implements a search source (e.g. Find File, Grep File, Buffer List).
  Backends are stateless modules; all mutable state lives in the Server GenServer.

  ## Candidate format

  Candidates are either:
  - A plain string (e.g. buffer name, file path)
  - A map with `:text` and optional `:matches` keys for highlighted results:
        %{text: "src/main.ex:10:5: defmodule Foo", matches: [[18, 21]]}
  """

  @typedoc "Backend name as displayed in the UI, e.g. \"Find File\""
  @type backend_name :: String.t()

  @typedoc "A search candidate — plain string or rich map with match positions"
  @type candidate ::
          String.t() | %{required(:text) => String.t(), optional(:matches) => [[integer()]]}

  @typedoc "Opaque per-backend state stored in the Server"
  @type backend_state :: map()

  @doc """
  Search for candidates matching `prefix`.

  Returns a list of candidates. Called inside a Task by the Server,
  so it may block on external commands (fd, rg) without stalling the coordinator.
  """
  @callback search_match(prefix :: String.t(), state :: backend_state()) :: [candidate()]

  @doc """
  Execute the primary action for a candidate (e.g. open file, switch buffer).

  Should send elisp to Emacs via `Exhub.send_message/1`.
  """
  @callback do_action(candidate :: candidate(), state :: backend_state()) :: :ok

  @doc """
  Copy the candidate's meaningful text to the kill ring.
  """
  @callback copy(candidate :: candidate(), state :: backend_state()) :: :ok

  @doc """
  Navigate to the parent context (e.g. parent directory).
  """
  @callback parent(candidate :: candidate(), state :: backend_state()) :: :ok

  @doc """
  Preview action triggered on candidate selection (M-n / M-p navigation).

  Defaults to `do_action/2` if not overridden.
  """
  @callback select(candidate :: candidate(), state :: backend_state()) :: :ok

  @doc """
  Continue search in a subdirectory (for Find File, Common Directory).

  Returns `{:ok, new_dir}` or `:error` if unsupported.
  """
  @callback continue_search(candidate :: candidate(), state :: backend_state()) ::
              {:ok, String.t()} | :error

  @doc """
  Clean up backend resources (kill subprocesses, remove temp files).
  """
  @callback clean(state :: backend_state()) :: backend_state()

  @doc """
  Update backend data pushed from Emacs (buffer list, recent files, symbols, imenu).
  """
  @callback update(items :: list(), state :: backend_state()) :: backend_state()

  @doc """
  Initialize the search directory for directory-aware backends.
  """
  @callback init_dir(search_dir :: String.t(), state :: backend_state()) :: backend_state()

  @doc """
  Return a display name for recording in history.

  Defaults to the candidate text.
  """
  @callback record_name(candidate :: candidate(), state :: backend_state()) :: String.t()

  # ---------------------------------------------------------------------------
  # Default implementations via __using__
  # ---------------------------------------------------------------------------

  defmacro __using__(_opts) do
    quote do
      @behaviour Exhub.BlinkSearch.Backend

      @impl true
      def copy(candidate, _state) do
        text = Exhub.BlinkSearch.Backend.candidate_text(candidate)
        Exhub.send_message(~s|(kill-new #{Exhub.BlinkSearch.Backend.elisp_quote(text)})|)

        Exhub.send_message(
          ~s|(message "[Blink-Search] Copy: #{Exhub.BlinkSearch.Backend.escape_message(text)}")|
        )

        :ok
      end

      @impl true
      def parent(candidate, state), do: do_action(candidate, state)

      @impl true
      def select(candidate, state), do: do_action(candidate, state)

      @impl true
      def continue_search(_candidate, _state), do: :error

      @impl true
      def clean(state), do: state

      @impl true
      def update(items, state) do
        Map.put(state, :items, items)
      end

      @impl true
      def init_dir(search_dir, state) do
        Map.put(state, :search_dir, search_dir)
      end

      @impl true
      def record_name(candidate, _state) do
        Exhub.BlinkSearch.Backend.candidate_text(candidate)
      end

      defoverridable copy: 2,
                     parent: 2,
                     select: 2,
                     continue_search: 2,
                     clean: 1,
                     update: 2,
                     init_dir: 2,
                     record_name: 2
    end
  end

  # ---------------------------------------------------------------------------
  # Shared helpers
  # ---------------------------------------------------------------------------

  @doc "Extract display text from a candidate (string or map)."
  @spec candidate_text(candidate()) :: String.t()
  def candidate_text(text) when is_binary(text), do: text
  def candidate_text(%{text: text}), do: text
  def candidate_text(%{"text" => text}), do: text

  @doc "Extract match positions from a candidate (nil for plain strings)."
  @spec candidate_matches(candidate()) :: [[integer()]] | nil
  def candidate_matches(text) when is_binary(text), do: nil
  def candidate_matches(%{matches: matches}), do: matches
  def candidate_matches(%{"matches" => matches}), do: matches
  def candidate_matches(_), do: nil

  @doc """
  Build a fuzzy regex from a search prefix.

  Splits on whitespace and joins with `.*` for subsequence matching.
  """
  @spec fuzzy_regex(String.t()) :: Regex.t()
  def fuzzy_regex(prefix) do
    pattern =
      prefix
      |> String.replace("*", "")
      |> String.split(~r/\s+/, trim: true)
      |> Enum.map(&Regex.escape/1)
      |> Enum.join(".*")

    Regex.compile!(".*#{pattern}", "i")
  end

  @doc """
  Check if `symbol` matches `prefix` using blink-search's matching rules:
  1. starts with prefix
  2. starts with prefix after removing hyphens
  3. contains prefix as substring
  4. matches fuzzy regex
  """
  @spec is_match?(String.t(), Regex.t(), String.t()) :: boolean()
  def is_match?(prefix, prefix_regexp, symbol) do
    clean_prefix = String.replace(prefix, "*", "")

    String.starts_with?(symbol, clean_prefix) or
      String.starts_with?(String.replace(symbol, "-", ""), clean_prefix) or
      String.contains?(symbol, clean_prefix) or
      Regex.match?(prefix_regexp, symbol)
  end

  @doc "Filter a list of string items by fuzzy match against prefix."
  @spec filter_match([String.t()], String.t()) :: [String.t()]
  def filter_match(items, prefix) do
    regex = fuzzy_regex(prefix)
    Enum.filter(items, &is_match?(prefix, regex, &1))
  end

  @doc """
  Quote a string as an elisp string literal.

  Wraps in double quotes and escapes backslashes and double quotes.
  """
  @spec elisp_quote(String.t()) :: String.t()
  def elisp_quote(str) do
    escaped =
      str
      |> String.replace("\\", "\\\\")
      |> String.replace("\"", "\\\"")

    "\"#{escaped}\""
  end

  @doc "Escape a string for safe embedding in an elisp message."
  @spec escape_message(String.t()) :: String.t()
  def escape_message(str) do
    str
    |> String.replace("\\", "\\\\")
    |> String.replace("\"", "\\\"")
  end

  @doc """
  Get the git project root for a directory, falling back to the directory itself.
  """
  @spec get_project_path(String.t()) :: String.t()
  def get_project_path(search_dir) do
    search_path = Path.expand(search_dir)

    case System.cmd("git", ["rev-parse", "--show-toplevel"],
           cd: search_path,
           stderr_to_stdout: true
         ) do
      {path, 0} -> String.trim(path)
      _ -> search_path
    end
  rescue
    _ -> Path.expand(search_dir)
  end

  @doc """
  Parse a ripgrep JSON line into a candidate map.

  Returns `nil` for non-match lines or parse errors.
  When `search_path` is given, file paths are made relative to it.
  """
  @spec parse_rg_line(String.t(), String.t() | nil) :: map() | nil
  def parse_rg_line(line, search_path \\ nil) do
    case Jason.decode(line) do
      {:ok, %{"type" => "match", "data" => data}} ->
        path_text = get_in(data, ["path", "text"]) || ""
        line_number = data["line_number"]
        submatches = data["submatches"] || []
        lines_data = data["lines"] || %{}

        # Extract the line text (first key in the lines map)
        line_text =
          case Map.values(lines_data) do
            [text | _] -> String.trim_trailing(text, "\n")
            _ -> ""
          end

        prefix =
          if search_path do
            rel_path = Path.relative_to(path_text, search_path)
            start_col = if submatches != [], do: hd(submatches)["start"], else: 0
            "#{rel_path}:#{line_number}:#{start_col}: "
          else
            start_col = if submatches != [], do: hd(submatches)["start"], else: 0
            "#{line_number}:#{start_col}: "
          end

        candidate_text = "#{prefix}#{line_text}"
        prefix_byte_len = byte_size(prefix)

        matches =
          Enum.map(submatches, fn match ->
            [match["start"] + prefix_byte_len, match["end"] + prefix_byte_len]
          end)

        %{text: candidate_text, matches: matches}

      _ ->
        nil
    end
  rescue
    _ -> nil
  end

  @doc """
  Run an external command and return stdout lines.

  Uses Exile for robust process management. Kills any previous subprocess
  stored in `state[:port]` before starting a new one.
  """
  @spec get_process_result([String.t()], keyword()) :: [String.t()]
  def get_process_result(command_list, opts \\ []) do
    cwd = Keyword.get(opts, :cd)

    exile_opts =
      [stderr: :consume, env: clean_env()]
      |> then(fn base -> if cwd, do: Keyword.put(base, :cd, cwd), else: base end)

    Exile.stream(command_list, exile_opts)
    |> Enum.reduce("", fn
      {:stdout, data}, acc -> acc <> data
      _, acc -> acc
    end)
    |> String.split("\n", trim: true)
    |> Enum.map(&String.trim/1)
  rescue
    e ->
      require Logger
      Logger.warning("get_process_result failed: #{inspect(e)}")
      []
  end

  @doc "Clean environment for child processes (strip RELEASE_* vars)."
  @spec clean_env() :: [{String.t(), String.t()}]
  def clean_env do
    System.get_env()
    |> Enum.reject(fn {k, _} ->
      String.starts_with?(k, "RELEASE") or k in ["PROGNAME", "ROOTDIR", "BINDIR"]
    end)
    |> Enum.to_list()
  end
end
