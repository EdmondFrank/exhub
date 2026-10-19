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

  Values come from the process environment (`System.get_env/1`). To inject
  them from a secrets vault, start ExHub under `envless`:

      ENVLESS_ROOT=~/GTD envless exec -- bin/exhub start

  A placeholder whose variable is unset is left literal and a warning naming
  the variable is logged, so a misconfiguration is visible rather than silently
  sending an empty credential. The resolved value is never logged.
  """

  require Logger

  @placeholder ~r/\$\{[A-Za-z_][A-Za-z0-9_]*\}/

  @doc """
  Expands `${VAR_NAME}` placeholders in a string using environment variables.

  Unset variables are left as the literal `${VAR_NAME}` (plus a warning), and
  strings without a `${` are returned unchanged.
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

  defp resolve("${" <> _ = match) do
    name = String.slice(match, 2..-2//1)

    case System.get_env(name) do
      nil ->
        Logger.warning(
          "[MCP Hub] ${#{name}} referenced in mcp_servers config but not set; " <>
            "leaving placeholder unexpanded"
        )

        "${#{name}}"

      value ->
        value
    end
  end
end
