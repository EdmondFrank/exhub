defmodule Exhub.Sagents.Persistence do
  @moduledoc """
  `Sagents.AgentPersistence` backed by `Exhub.Sagents.Store` (SQLite).

  Sagents hands us its serialized state envelope
  (`%{"version" => 1, "state" => %{...}, "serialized_at" => ...}`); we store
  it as JSON in `agent_states`, keyed by conversation id. Because the
  conversation is the unit of persistence, one agent process per conversation
  restores exactly the messages it owned before it timed out.
  """

  @behaviour Sagents.AgentPersistence

  alias Exhub.Sagents.Store

  @impl true
  def persist_state(_scope, state_data, context) do
    case conversation_id(context) do
      nil ->
        # Unscoped write (no conversation configured) — nothing to key on.
        :ok

      id ->
        Store.save_state(id, state_data)
    end
  end

  @impl true
  def load_state(_scope, context) do
    case conversation_id(context) do
      nil -> {:error, :not_found}
      id -> Store.load_state(id)
    end
  end

  @impl true
  def set_interrupted(_scope, context, interrupted?) do
    case conversation_id(context) do
      nil ->
        :ok

      id ->
        status = if interrupted?, do: "interrupted", else: "active"

        case Store.update_conversation(id, %{status: status}) do
          {:ok, _conversation} -> :ok
          {:error, reason} -> {:error, reason}
        end
    end
  end

  defp conversation_id(%{conversation_id: id}) when is_binary(id), do: id
  defp conversation_id(_context), do: nil
end
