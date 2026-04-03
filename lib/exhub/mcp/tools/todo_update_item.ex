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
    Mark a single todo item as completed or not completed.

    Call this immediately after finishing a step to keep the list up to date.
    The item is looked up by its exact `name`, so the value must match what
    was passed to `set_items` character-for-character.

    Requires an existing list — call `set_items` first if none exists yet.
    Returns the full updated list so you can see the current state at a glance.

    Parameters:
    - tenant_id: The same stable string used when the list was created with
      `set_items`. Must match exactly (case-sensitive).
    - name: The exact name of the item to update, as it appears in the list.
    - completed: true to mark the item done, false to reopen it.
    """
  end

  schema do
    field :tenant_id, {:required, :string}, description: "The same stable string used when the list was created with set_items (e.g. a conversation ID, username, or task slug). Must match exactly."
    field :name, {:required, :string}, description: "The exact name of the todo item to update, character-for-character as it was given to set_items."
    field :completed, {:required, :boolean}, description: "true to mark the item done; false to reopen it."
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
