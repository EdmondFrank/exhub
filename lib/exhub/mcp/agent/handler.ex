defmodule Exhub.MCP.Agent.Handler do
  @moduledoc """
  Handler for ExMCP.ACP.Client that forwards events to the AgentStore.

  Implements the `ExMCP.ACP.Client.Handler` behaviour to receive session updates,
  permission requests, and file operations from ACP agents.
  """

  @behaviour ExMCP.ACP.Client.Handler

  def init(opts) do
    agent_id = Keyword.fetch!(opts, :agent_id)
    {:ok, %{agent_id: agent_id}}
  end

  def handle_session_update(session_id, update, state) do
    # Strip large fields from tool_call_update events
    update = slim_tool_call_update(update)

    event = %{type: "update", update: update, session_id: session_id, agent_id: state.agent_id}
    Exhub.MCP.Agent.Store.push_event(state.agent_id, session_id, event)
    {:ok, state}
  end

  defp slim_tool_call_update(update) when is_map(update) do
    if Map.get(update, "sessionUpdate") == "tool_call_update" do
      update
      |> strip_raw_output_fields()
    else
      update
    end
  end

  defp slim_tool_call_update(update), do: update

  defp strip_raw_output_fields(update) do
    case Map.get(update, "rawOutput") do
      raw_output when is_map(raw_output) ->
        stripped_raw_output =
          raw_output
          |> Map.delete("filediff")
          |> Map.delete("before")
          |> Map.delete("after")

        Map.put(update, "rawOutput", stripped_raw_output)

      _ ->
        update
    end
  end

  def handle_permission_request(session_id, tool_call, options, state) do
    # For operator mode: push event and wait for resolution via grant_permission tool
    from_ref = make_ref()

    Exhub.MCP.Agent.Store.set_pending_permission(
      state.agent_id,
      session_id,
      tool_call,
      options,
      {self(), from_ref}
    )

    # Push a permission_request event so the MCP client knows
    event = %{
      type: "permission_request",
      session_id: session_id,
      agent_id: state.agent_id,
      tool_call: tool_call,
      options: options
    }

    Exhub.MCP.Agent.Store.push_event(state.agent_id, session_id, event)

    # Wait for resolution (up to 5 minutes)
    receive do
      {^from_ref, outcome} -> {:ok, outcome, state}
    after
      300_000 -> {:ok, %{"cancelled" => true}, state}
    end
  end

  def handle_file_read(_session_id, path, _opts, state) do
    case File.read(path) do
      {:ok, content} -> {:ok, content, state}
      {:error, reason} -> {:error, "Cannot read #{path}: #{inspect(reason)}", state}
    end
  end

  def handle_file_write(_session_id, path, content, state) do
    case File.write(path, content) do
      :ok -> {:ok, state}
      {:error, reason} -> {:error, "Cannot write #{path}: #{inspect(reason)}", state}
    end
  end

  def terminate(_reason, _state), do: :ok
end
