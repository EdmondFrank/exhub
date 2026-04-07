defmodule Exhub.MCP.Agent.Config do
  @moduledoc """
  Configuration loader for ACP agents.

  Loads agent definitions from `~/.config/exhub/agents.json` or the path
  specified by the `EXHUB_AGENTS_CONFIG` environment variable.

  The config file should be a JSON map of `agent_id => %{command: ..., args: [...], env: %{}}`.
  """

  @default_path "~/.config/exhub/agents.json"

  @doc """
  Returns the path to the agents config file.
  """
  def config_path do
    System.get_env("EXHUB_AGENTS_CONFIG") || Path.expand(@default_path)
  end

  @doc """
  Loads the agents configuration from disk.

  Returns `{:ok, map}` on success, or `{:ok, %{}}` if the file doesn't exist
  or is invalid JSON.
  """
  def load do
    path = config_path()

    case File.read(path) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, map} when is_map(map) -> {:ok, map}
          {:error, _} -> {:ok, %{}}
        end

      {:error, _} ->
        {:ok, %{}}
    end
  end

  @doc """
  Gets the command for a specific agent from the config.

  Returns `{:ok, [command | args]}` or `{:error, :not_found}`.
  """
  def get_agent_command(agent_id) do
    case load() do
      {:ok, config} ->
        case Map.get(config, agent_id) do
          nil ->
            {:error, :not_found}

          entry ->
            cmd = Map.get(entry, "command", agent_id)
            args = Map.get(entry, "args", [])
            {:ok, [cmd | args]}
        end

      _ ->
        {:error, :not_found}
    end
  end
end
