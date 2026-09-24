defmodule Exhub.MCP.Desktop.Helpers do
  @moduledoc """
  Shared helpers for Desktop MCP tools.

  Provides TOON-encoded responses to reduce LLM token consumption by 30-60%
  compared to JSON, while maintaining full readability and semantic equivalence.

  ## Usage

      alias Exhub.MCP.Desktop.Helpers
      alias Anubis.Server.Response

      resp =
        Response.tool()
        |> Helpers.toon_response(%{"success" => true, "count" => 42})

      {:reply, resp, frame}
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Encoding

  @doc """
  Encode a map as TOON and add it as text content to a tool response.

  Falls back to JSON encoding if TOON encoding fails for any reason.
  """
  @spec toon_response(Response.t(), map()) :: Response.t()
  def toon_response(%Response{} = resp, data) when is_map(data) do
    Response.text(resp, toon_encode(data))
  end

  @doc """
  Encode a map as TOON, falling back to JSON if TOON encoding fails.

  Useful for embedding structured data inside a larger text payload (e.g. an
  error message that must also carry partial output).
  """
  @spec toon_encode(map()) :: String.t()
  def toon_encode(data) when is_map(data) do
    data = Encoding.sanitize_utf8(data)

    try do
      Toon.encode!(data)
    rescue
      _ -> Jason.encode!(data)
    end
  end

  @doc """
  Resolves `~` and `~/...` paths to the user home directory.

  Passes through absolute paths unchanged. Returns `nil` for `nil`,
  useful for optional path parameters like `working_dir`.

  **Note:** For path validation that rejects relative paths, use
  `validate_absolute_path/1` instead.
  """
  @spec expand_path(String.t() | nil) :: String.t() | nil
  def expand_path(nil), do: nil
  def expand_path("~"), do: System.user_home!()
  def expand_path("~/" <> rest), do: Path.join(System.user_home!(), rest)
  def expand_path(path), do: path

  @doc """
  Validates that a path is absolute or tilde-based, and expands `~` to the user home directory.

  Only accepts:
  - `nil` (for optional path parameters) → returns `{:ok, nil}`
  - `~` → expands to user home directory
  - `~/...` → expands to `$HOME/...`
  - `/...` → absolute path, returned as-is

  Returns `{:error, message}` for relative paths (e.g. `foo/bar`, `../file`).

  ## Examples

      iex> validate_absolute_path("/tmp/file")
      {:ok, "/tmp/file"}

      iex> validate_absolute_path("~/Documents")
      {:ok, "/Users/me/Documents"}

      iex> validate_absolute_path(nil)
      {:ok, nil}

      iex> validate_absolute_path("relative/path")
      {:error, "Relative paths are not supported: ..."}
  """
  @spec validate_absolute_path(String.t() | nil) :: {:ok, String.t() | nil} | {:error, String.t()}
  def validate_absolute_path(nil), do: {:ok, nil}
  def validate_absolute_path("~"), do: {:ok, System.user_home!()}
  def validate_absolute_path("~/" <> rest), do: {:ok, Path.join(System.user_home!(), rest)}
  def validate_absolute_path("/" <> _ = path), do: {:ok, path}

  def validate_absolute_path(path) when is_binary(path) do
    {:error,
     "Relative paths are not supported: '#{path}'. Use an absolute path (e.g. /path/to/file) or ~ shorthand (e.g. ~/path/to/file)."}
  end

  @doc """
  Returns a clean environment variable list suitable for spawning child processes.

  Filters out Elixir Release-related environment variables (RELEASE_*) that can
  interfere with child process execution, particularly when running mix commands
  or other Elixir tooling from within a release.

  Preserves all user environment variables like PATH, HOME, USER, etc.

  Returns a list of {key, value} tuples for use with Exile's `env` option.
  """
  @spec clean_env() :: [{String.t(), String.t()}]
  def clean_env do
    System.get_env()
    |> Enum.reject(fn {k, _} ->
      String.starts_with?(k, "RELEASE") or
        k in ["PROGNAME", "ROOTDIR", "BINDIR"]
    end)
    |> Enum.to_list()
  end

  @doc """
  Returns the configured shell executable.

  Looks up the shell from application config, defaulting to "sh" if not set.

  ## Examples

      iex> Helpers.get_shell()
      "sh"

  Can be configured in config/config.exs:

      config :exhub, :shell, "bash"
  """
  @spec get_shell() :: String.t()
  def get_shell do
    Application.get_env(:exhub, :shell, "sh")
  end

  @doc """
  Returns the full argument list for executing a command in the configured shell.

  Defaults to a login shell. Use `shell_command_args(command, login: false)` for
  simple commands that don't need login shell behavior.

  Handles common shells with appropriate flags:
  - sh, bash, zsh: Uses "-l -c" for login shell, "-c" for non-login
  - fish: Uses "-c" (fish doesn't support -l the same way)
  - Other shells: Uses "-c" as a safe default

  ## Examples

      iex> Helpers.shell_command_args("echo hello")
      ["sh", "-l", "-c", "echo hello"]

      iex> Helpers.shell_command_args("echo hello", login: false)
      ["sh", "-c", "echo hello"]
  """
  @spec shell_command_args(String.t(), keyword()) :: [String.t()]
  def shell_command_args(command, opts \\ []) do
    shell = get_shell()
    shell_name = Path.basename(shell)
    login? = Keyword.get(opts, :login, true)

    args =
      cond do
        not login? ->
          # Non-login shell: just use -c for all shells
          ["-c", command]

        shell_name in ["sh", "bash", "zsh", "dash", "ksh"] ->
          ["-l", "-c", command]

        shell_name == "fish" ->
          ["-c", command]

        true ->
          # Default to "-c" for unknown shells
          ["-c", command]
      end

    [shell | args]
  end

  @doc """
  Checks if a command likely needs a working directory.

  The inverse of `anchored?/1`: a command needs a working directory unless it
  names its own location. This is the deterministic fallback used when Smart
  Decide is disabled or unavailable, so it errs towards `true` (fail closed).
  """
  @spec needs_working_dir?(String.t()) :: boolean()
  def needs_working_dir?(command), do: not anchored?(command)

  @doc """
  Returns `true` when `command` names its own location, so the working
  directory is irrelevant:

    * it contains an absolute (`/…`) or `~`/`~/…` path token, anywhere;
    * it contains a `cd` command whose target is absolute or `~` (e.g.
      `cd /tmp`, `cd ~/src`), or a bare `cd` (which goes to `$HOME`), e.g.
      `cd`, `cd;`, `cd && ls`. A `cd` only counts at the start of the command
      or after a command separator (`;`, `&`, `|`, `(`, `)`, newline) — a
      trailing argument such as `cat cd` or `grep foo cd` does not anchor, nor
      does a `cd` merely printed (`git commit -m "cd fix"`).

  A `cd` into a **relative** directory is *not* anchored: `cd build && make`
  still depends on the working directory to resolve `build`, so it must supply
  one. Text inside single or double quotes is replaced by an empty token before
  scanning, so a quoted path (`echo "ls /tmp"`) anchors nothing and a quoted
  `cd` target (`cd "build"`) stays relative.

  ## Examples

      iex> anchored?("cd /tmp && ls")
      true

      iex> anchored?("cd build && make")
      false
  """
  @spec anchored?(String.t()) :: boolean()
  def anchored?(command) when is_binary(command) do
    stripped = strip_quoted(command)

    has_absolute_path =
      stripped
      |> String.split()
      |> Enum.any?(&absolute_word?/1)

    has_absolute_cd = Regex.match?(~r{(^|[;&|()\n])\s*cd\s+(~|/)\S*}, stripped)
    has_bare_cd = Regex.match?(~r{(^|[;&|()\n])\s*cd\s*($|[;&|()\n])}, stripped)

    has_absolute_path or has_absolute_cd or has_bare_cd
  end

  def anchored?(_command), do: false

  # Replace shell-quoted spans with an empty token before looking for tokens:
  # a value such as `git commit -m "cd fix"` must not be mistaken for a `cd`
  # command, and a quoted path must not anchor — while a quoted argument still
  # counts as a token, so `cd "build"` is not misread as a bare `cd`.
  defp strip_quoted(text) do
    text
    |> String.replace(~r/'[^']*'/, ~s(""))
    |> String.replace(~r/"[^"]*"/, ~s(""))
  end

  defp absolute_word?(word) do
    word == "~" or String.starts_with?(word, "/") or String.starts_with?(word, "~/")
  end

  @doc """
  Returns the current server working directory for documentation.
  """
  @spec current_pwd() :: String.t()
  def current_pwd do
    File.cwd!() || System.user_home!()
  end
end
