defmodule Exhub.MCP.Tools.AgentBus.Helpers do
  @moduledoc """
  Shared plumbing for the agent-bus tools.

  Identity resolution order: the `as` parameter, then the
  `x-exhub-agent-id` header (injected by `/agent-bus/mcp?as=<id>`), so a
  subagent configured with its own bus URL does not have to repeat who it is
  on every call.
  """

  alias Anubis.Server.Response

  @doc "Resolves the calling agent id, or `nil` when the caller did not identify itself."
  @spec caller(map(), Anubis.Server.Frame.t()) :: String.t() | nil
  def caller(params, frame) do
    Map.get(params, :as) || Map.get(params, "as") || header(frame)
  end

  @spec require_caller(map(), Anubis.Server.Frame.t()) :: {:ok, String.t()} | {:error, String.t()}
  def require_caller(params, frame) do
    case caller(params, frame) do
      id when is_binary(id) and id != "" ->
        {:ok, id}

      _ ->
        {:error,
         "unknown caller: pass `as: \"<agent-id>\"`, or connect to /agent-bus/mcp?as=<agent-id>"}
    end
  end

  @spec ok(Anubis.Server.Frame.t(), map()) :: {:reply, Response.t(), Anubis.Server.Frame.t()}
  def ok(frame, data) do
    {:reply, Response.tool() |> Response.text(Jason.encode!(data)), frame}
  end

  @spec error(Anubis.Server.Frame.t(), term()) :: {:reply, Response.t(), Anubis.Server.Frame.t()}
  def error(frame, message) do
    {:reply, Response.tool() |> Response.error(to_string(message)), frame}
  end

  @doc "Turns a validation atom into a message an agent can act on."
  def error_message(:missing_id), do: "id is required"

  def error_message(:invalid_id),
    do: "invalid id: use lowercase letters, digits, '.', '_' or '-' (max 64)"

  def error_message(:missing_agent), do: "from/to is required"

  def error_message(:invalid_agent_id),
    do: "invalid agent id: use lowercase letters, digits, '.', '_' or '-' (max 64)"

  def error_message(:missing_room), do: "room is required"

  def error_message(:invalid_room_name),
    do: "invalid room name: use lowercase letters, digits, '.', '_' or '-' (max 64)"

  def error_message(:empty_body), do: "body must not be empty"

  def error_message({:body_too_large, limit}),
    do: "body exceeds #{limit} bytes; put large output in a file and reference the path"

  def error_message(:invalid_body), do: "body must be a string"

  def error_message({:invalid_kind, kind}),
    do: "invalid kind #{inspect(kind)}: use ask, tell, reply or handoff"

  def error_message({:invalid_status, status}),
    do:
      "invalid status #{inspect(status)}: use #{Enum.join(Exhub.Agent.Registry.statuses(), ", ")}"

  def error_message(:invalid_detail), do: "detail must be a string of at most 200 characters"
  def error_message(:invalid_name), do: "name must be a string"
  def error_message(:invalid_capabilities), do: "capabilities must be a list of strings"
  def error_message(:invalid_idempotency_key), do: "idempotency_key must be a string"

  def error_message(:idempotency_key_too_long),
    do: "idempotency_key must be at most 128 characters"

  def error_message(:idempotency_key_reused),
    do: "idempotency_key was already used with a different target or body"

  def error_message(:room_not_found), do: "room not found"
  def error_message(other), do: inspect(other)

  @doc "True when an ACP agent process is alive. Safe when the store is not running."
  def running?(agent_id) do
    case Exhub.MCP.Agent.Store.get(agent_id) do
      {:ok, entry} -> Process.alive?(entry.client)
      _ -> false
    end
  rescue
    _ -> false
  catch
    :exit, _ -> false
  end

  defp header(frame) do
    case frame do
      %{context: %{headers: headers}} when is_map(headers) ->
        headers["x-exhub-agent-id"] || headers["x-exhub-agent"]

      _ ->
        nil
    end
  end
end
