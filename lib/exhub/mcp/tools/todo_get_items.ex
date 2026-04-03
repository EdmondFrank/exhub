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
    Retrieve the current todo list for a tenant.

    Returns all todo items together with the `initial_user_prompt` that was
    stored when the list was created.  Returns an empty list when no list
    exists for the given tenant.

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
