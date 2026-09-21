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
    Read the current todo list for a task: every item with its `completed`
    status, the saved `initial_user_prompt`, and the time of the last update.
    Use it to resume work or check progress before deciding what to do next.

    Returns an empty list (not an error) when no list exists for `tenant_id`.
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

  defp item_to_map(%{name: name, completed: completed}),
    do: %{"name" => name, "completed" => completed}

  defp item_to_map(item), do: item
end
