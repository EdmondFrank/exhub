defmodule Exhub.MCP.Tools.TodoUpdateItem do
  @moduledoc """
  MCP Tool: update_item_completion

  Updates the `completed` flag of a single todo item (identified by name)
  for a given tenant.
  """

  alias Exhub.MCP.TodoStore
  alias Anubis.Server.Response

  use Anubis.Server.Component, type: :tool

  def name, do: "update_item_completion"

  @impl true
  def description do
    """
    Update the completion status of a specific todo item for a tenant.

    Finds the item by its `name` and sets its `completed` flag to the
    provided value.  Returns the full updated list on success.

    Parameters:
    - tenant_id: Unique identifier for the tenant / session.
    - name: The name of the todo item to update.
    - completed: The new completion status (true or false).
    """
  end

  schema do
    field :tenant_id, {:required, :string}, description: "Unique identifier for the tenant or session."
    field :name, {:required, :string}, description: "The name of the todo item to update."
    field :completed, {:required, :boolean}, description: "The new completion status for the todo item."
  end

  @impl true
  def execute(params, frame) do
    tenant_id = Map.get(params, :tenant_id)
    item_name = Map.get(params, :name)
    completed = Map.get(params, :completed)

    case TodoStore.update_item(tenant_id, item_name, completed) do
      {:ok, entry} ->
        resp =
          Response.tool()
          |> Response.structured(%{
            "success" => true,
            "tenant_id" => tenant_id,
            "updated_item" => item_name,
            "completed" => completed,
            "items" => Enum.map(entry.items, &item_to_map/1),
            "count" => length(entry.items)
          })

        {:reply, resp, frame}

      {:error, :not_found} ->
        resp =
          Response.tool()
          |> Response.error("No todo list found for tenant '#{tenant_id}'. Create one first with set_items.")

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to update item: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end

  defp item_to_map(%{name: name, completed: completed}), do: %{"name" => name, "completed" => completed}
  defp item_to_map(item), do: item
end
