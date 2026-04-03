defmodule Exhub.MCP.Tools.TodoClearItems do
  @moduledoc """
  MCP Tool: clear_items

  Removes all todo items for a given tenant while keeping the tenant entry
  alive (with an empty list).
  """

  alias Exhub.MCP.TodoStore
  alias Anubis.Server.Response

  use Anubis.Server.Component, type: :tool

  def name, do: "clear_items"

  @impl true
  def description do
    """
    Remove all todo items from a task's list.

    Call this when a task is fully complete and you no longer need the list,
    or when you want to start over with a fresh plan (follow up with
    `set_items` to create a new one).

    The list entry itself is kept in the store (with an empty items array),
    so a subsequent `get_items` call will return an empty list rather than
    a "not found" response. The list will be fully removed after 2 hours of
    inactivity regardless.

    Parameters:
    - tenant_id: The same stable string used when the list was created with
      `set_items`. Must match exactly (case-sensitive).
    """
  end

  schema do
    field :tenant_id, {:required, :string}, description: "The same stable string used when the list was created with set_items (e.g. a conversation ID, username, or task slug). Must match exactly."
  end

  @impl true
  def execute(params, frame) do
    tenant_id = Map.get(params, :tenant_id)

    case TodoStore.clear_todos(tenant_id) do
      :ok ->
        resp =
          Response.tool()
          |> Response.structured(%{
            "success" => true,
            "tenant_id" => tenant_id,
            "message" => "All todo items cleared successfully."
          })

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to clear todos: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end
end
