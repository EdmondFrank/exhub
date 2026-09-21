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
    Remove all items from a task's list. Call it when the task is done, or before
    `set_items` to start a fresh plan.

    The entry stays as an empty list, so a later `get_items` returns an empty
    list rather than "not found"; the entry is purged after 2 hours idle.
    """
  end

  schema do
    field(:tenant_id, {:required, :string},
      description: "Stable task/conversation id; must match the value used with set_items."
    )
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
