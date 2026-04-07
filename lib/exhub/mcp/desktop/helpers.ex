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

  Passes through absolute and relative paths unchanged. Returns `nil` for `nil`,
  useful for optional path parameters like `working_dir`.
  """
  @spec expand_path(String.t() | nil) :: String.t() | nil
  def expand_path(nil), do: nil
  def expand_path("~"), do: System.user_home!()
  def expand_path("~/" <> rest), do: Path.join(System.user_home!(), rest)
  def expand_path(path), do: path

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
end
