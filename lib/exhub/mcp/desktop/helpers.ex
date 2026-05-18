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

  @doc """
  Encode a map as TOON and add it as text content to a tool response.

  Falls back to JSON encoding if TOON encoding fails for any reason.
  """
  @spec toon_response(Response.t(), map()) :: Response.t()
  def toon_response(%Response{} = resp, data) when is_map(data) do
    encoded =
      try do
        Toon.encode!(data)
      rescue
        _ -> Jason.encode!(data)
      end

    Response.text(resp, encoded)
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

  Returns `false` if the command contains `cd`, `ls`, or any absolute path
  (starting with `/` or `~/`), since those commands already specify where
  to operate.
  """
  @spec needs_working_dir?(String.t()) :: boolean()
  def needs_working_dir?(command) do
    trimmed = String.trim(command)

    has_cd = String.starts_with?(trimmed, "cd ") or String.contains?(trimmed, " cd ")
    has_ls = String.starts_with?(trimmed, "ls ") or String.contains?(trimmed, " ls ")

    has_absolute_path =
      trimmed
      |> String.split()
      |> Enum.any?(fn word ->
        String.starts_with?(word, "/") or String.starts_with?(word, "~/")
      end)

    not (has_cd or has_ls or has_absolute_path)
  end

  @doc """
  Returns the current server working directory for documentation.
  """
  @spec current_pwd() :: String.t()
  def current_pwd do
    File.cwd!() || System.user_home!()
  end
end
