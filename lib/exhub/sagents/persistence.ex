defmodule Exhub.Sagents.Persistence do
  @moduledoc """
  Optional file-based persistence for sagents agent state.

  Implements `Sagents.AgentPersistence` behaviour. Saves/loads agent state
  to `~/.config/exhub/agent_hub/{agent_id}/state.json`.
  """

  @behaviour Sagents.AgentPersistence

  require Logger

  @base_dir "~/.config/exhub/agent_hub"

  @impl true
  def persist_state(_scope, state_data, context) do
    agent_id = Map.get(context, :agent_id)
    dir = agent_dir(agent_id)
    File.mkdir_p!(dir)

    path = Path.join(dir, "state.json")

    case Jason.encode(state_data, pretty: true) do
      {:ok, json} ->
        File.write!(path, json)
        Logger.debug("[Sagents.Persistence] Persisted state for #{agent_id}")
        :ok

      {:error, reason} ->
        Logger.error("[Sagents.Persistence] Failed to encode state: #{inspect(reason)}")
        {:error, reason}
    end
  rescue
    error ->
      Logger.error("[Sagents.Persistence] Persist failed: #{inspect(error)}")
      {:error, inspect(error)}
  end

  @impl true
  def load_state(_scope, context) do
    agent_id = Map.get(context, :agent_id)
    path = Path.join(agent_dir(agent_id), "state.json")

    case File.read(path) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, data} ->
            Logger.debug("[Sagents.Persistence] Loaded state for #{agent_id}")
            {:ok, data}

          {:error, reason} ->
            Logger.warning("[Sagents.Persistence] Failed to decode state: #{inspect(reason)}")
            {:error, :not_found}
        end

      {:error, :enoent} ->
        {:error, :not_found}

      {:error, reason} ->
        Logger.error("[Sagents.Persistence] Failed to read state: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp agent_dir(agent_id) do
    @base_dir
    |> Path.expand()
    |> Path.join(agent_id)
  end
end
