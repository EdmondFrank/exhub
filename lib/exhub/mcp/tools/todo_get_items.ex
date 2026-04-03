defmodule Exhub.MCP.Tools.TodoGetItems do
  @moduledoc """
  MCP Tool: get_items

  Returns the current todo list for a given tenant.
  """

  alias Exhub.MCP.TodoStore
  alias Anubis.Server.Response

  use Anubis.Server.Component, type: :tool

  def name, do: "get_items"

  @impl true
  def description do
    """
    Read the current todo list for a task.

    Call this to reload your plan and check progress — for example when
    resuming a task, before deciding what to do next, or to verify the
    current state of the list.

    Returns all items with their `completed` status, the `initial_user_prompt`
    that was saved when the list was created, and the time of the last update.
    If no list exists for the given `tenant_id` (not yet created, or expired
    after 2 hours of inactivity), an empty list is returned — not an error.

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

    case TodoStore.get_todos(tenant_id) do
      {:ok, entry} ->
        resp =
          Response.tool()
          |> Response.structured(%{
            "tenant_id" => tenant_id,
            "initial_user_prompt" => entry.initial_user_prompt,
            "items" => Enum.map(entry.items, &item_to_map/1),
            "count" => length(entry.items),
            "updated_at" => DateTime.to_iso8601(entry.updated_at)
          })

        {:reply, resp, frame}

      {:error, :not_found} ->
        resp =
          Response.tool()
          |> Response.structured(%{
            "tenant_id" => tenant_id,
            "initial_user_prompt" => "",
            "items" => [],
            "count" => 0,
            "updated_at" => nil
          })

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to get todos: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end

  defp item_to_map(%{name: name, completed: completed}), do: %{"name" => name, "completed" => completed}
  defp item_to_map(item), do: item
end
