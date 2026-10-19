defmodule Exhub.MCP.Hub.EnvTemplate do
  @moduledoc """
  Expands `${VAR_NAME}` placeholders in MCP Hub server configuration values.

  Server configs are stored in `~/.config/exhub/mcp_servers.json` and get
  **persisted back to disk** whenever the hub config changes, so a secret must
  never be written there in plaintext. Instead, the config file may reference
  an environment variable with `${VAR_NAME}`:

      {
        "name": "github",
        "transport": "stdio",
        "command": "npx",
        "args": ["-y", "@modelcontextprotocol/server-github"],
        "env": {"GITHUB_PERSONAL_ACCESS_TOKEN": "${GITHUB_TOKEN}"}
      }

  Expansion happens in `Exhub.MCP.Hub.ServerConfig.to_anubis_client_opts/1`,
  i.e. only when an Anubis client is actually started — the `ServerConfig`
  structs (and therefore the on-disk file and the REST/status API) keep the raw
  placeholders. This keeps resolved secrets out of the config file and out of
  every config-representation path.

  ## Resolution order

  1. **Process environment** — `System.get_env/1`.
  2. **envless vault** — when the variable is not in the environment, it is
     read from the shared [envless](https://github.com/biliboss/envless) vault
     via `envless exec --env <env> -- sh -c 'printf %s "$VAR"'`. The secret
     never touches a shell, argv, the config file, or the ExHub process
     environment (so it is never inherited by `desktop`/stdio children), and it
     is never logged.

  A placeholder resolved by neither source is left literal and a warning naming
  the variable (never its value) is logged, so a misconfiguration is visible
  rather than silently sending an empty credential.

  ## Configuration

      config :exhub, Exhub.MCP.Hub.EnvTemplate,
        envless: true,          # false disables the vault fallback (env only)
        envless_env: "dev",     # envless --env (default "dev")
        envless_root: "/path",  # defaults to $ENVLESS_ROOT, else ~/GTD
        envless_path: "/path"   # override the envless binary (mainly for tests)
  """

  require Logger

  @placeholder ~r/\$\{[A-Za-z_][A-Za-z0-9_]*\}/
  @default_env "dev"

  @doc """
  Expands `${VAR_NAME}` placeholders in a string.

  Unset variables are resolved from the envless vault; a variable found in
  neither the environment nor the vault is left as the literal `${VAR_NAME}`
  (plus a warning). Strings without a `${` are returned unchanged.
  """
  @spec expand_string(String.t()) :: String.t()
  def expand_string(str) when is_binary(str) do
    if String.contains?(str, "${") do
      String.replace(str, @placeholder, &resolve/1)
    else
      str
    end
  end

  @doc """
  Recursively expands placeholders in a config value.

  Strings are expanded with `expand_string/1`; lists and map values are walked;
  anything else (atoms, numbers, `nil`) is returned unchanged.
  """
  @spec expand_value(term()) :: term()
  def expand_value(str) when is_binary(str), do: expand_string(str)
  def expand_value(list) when is_list(list), do: Enum.map(list, &expand_value/1)

  def expand_value(map) when is_map(map) do
    Map.new(map, fn {key, value} -> {key, expand_value(value)} end)
  end

  def expand_value(other), do: other

  # --- Placeholder resolution ---

  defp resolve("${" <> _ = match) do
    name = String.slice(match, 2..-2//1)

    case System.get_env(name) do
      nil -> resolve_from_vault(name)
      value -> value
    end
  end

  defp resolve_from_vault(name) do
    with true <- vault_enabled?(),
         binary when is_binary(binary) <- vault_binary(),
         {:ok, value} <- vault_get(binary, name) do
      value
    else
      _ ->
        Logger.warning(
          "[MCP Hub] ${#{name}} is not set in the environment or the envless vault; " <>
            "leaving placeholder unexpanded"
        )

        "${#{name}}"
    end
  end

  # Reads a single secret from the envless vault. `envless exec` injects the
  # secrets into the child's environment and the child prints just the requested
  # one; the value is captured from stdout and never logged.
  defp vault_get(binary, name) do
    script = ~s([ -n "$#{name}" ] && printf %s "$#{name}")
    args = ["exec", "--env", vault_env(), "--", "sh", "-c", script]

    try do
      case System.cmd(binary, args, env: [{"ENVLESS_ROOT", vault_root()}]) do
        {output, 0} -> {:ok, output}
        {_output, _code} -> {:error, :exit}
      end
    rescue
      _ -> {:error, :not_found}
    catch
      _, _ -> {:error, :not_found}
    end
  end

  # --- envless configuration ---

  defp config, do: Application.get_env(:exhub, __MODULE__, [])

  defp vault_enabled?, do: Keyword.get(config(), :envless, true)

  defp vault_env, do: Keyword.get(config(), :envless_env, @default_env)

  defp vault_root do
    System.get_env("ENVLESS_ROOT") ||
      Keyword.get(config(), :envless_root) ||
      Path.join(System.user_home!(), "GTD")
  end

  defp vault_binary do
    Keyword.get(config(), :envless_path) || System.find_executable("envless") || default_binary()
  end

  defp default_binary do
    path = Path.join([System.user_home!(), ".cargo", "bin", "envless"])
    if File.exists?(path), do: path, else: nil
  end
end
