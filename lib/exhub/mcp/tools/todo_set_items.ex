defmodule Exhub.MCP.Tools.TodoSetItems do
  @moduledoc """
  MCP Tool: set_items

  Initialises or overwrites the todo list for a given tenant.
  Passing an empty `items` array effectively clears the list while
  preserving the `initial_user_prompt`.
  """

  alias Exhub.MCP.TodoStore
  alias Anubis.Server.Response

  use Anubis.Server.Component, type: :tool

  def name, do: "set_items"

  @impl true
  def description do
    """
    Initialise or overwrite the todo list for a tenant.

    Accepts an array of todo items (each with a `name` and optional `completed`
    flag) and an `initial_user_prompt` that describes the original task.
    Calling this tool replaces any existing list for the tenant.

    Parameters:
    - tenant_id: Unique identifier for the tenant / session.
    - items: Array of todo items [{name, completed?}].
    - initial_user_prompt: The original user request that triggered the task.
    """
  end

  schema do
    field :tenant_id, {:required, :string}, description: "Unique identifier for the tenant or session."

    embeds_many :items, description: "Array of todo items to set." do
      field :name, {:required, :string}, description: "The name of the todo item."
      field :completed, :boolean, description: "Whether the item is completed. Defaults to false.", default: false
    end

    field :initial_user_prompt, :string, description: "The original user prompt that initiated the task.", default: ""
  end

  @impl true
  def execute(params, frame) do
    tenant_id = Map.get(params, :tenant_id)
    items = Map.get(params, :items, [])
    prompt = Map.get(params, :initial_user_prompt, "")

    case TodoStore.set_todos(tenant_id, items, prompt) do
      :ok ->
        resp =
          Response.tool()
          |> Response.structured(%{
            "success" => true,
            "tenant_id" => tenant_id,
            "count" => length(items),
            "message" => "Todo list set successfully."
          })

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to set todos: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end
end
