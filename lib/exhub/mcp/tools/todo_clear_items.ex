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
    Remove all todo items for a tenant.

    Clears the entire todo list for the given tenant.  The tenant entry is
    kept in the store (with an empty list) so subsequent `get_items` calls
    still return a valid (empty) response.

    Parameters:
    - tenant_id: Unique identifier for the tenant / session.
    """
  end

  schema do
    field :tenant_id, {:required, :string}, description: "Unique identifier for the tenant or session."
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
