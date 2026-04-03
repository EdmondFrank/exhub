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
    Create or replace the todo list for a task.

    Call this at the START of a multi-step task to record your full plan and
    the user's original request. Each item represents one step or sub-task.

    IMPORTANT: This call REPLACES any existing list for the given tenant_id.
    Do not call it mid-task unless you intend to rewrite the entire plan.
    To update a single item's status, use `update_item_completion` instead.

    After calling this tool, work through the items in order and mark each
    one complete with `update_item_completion` as you finish it.

    Parameters:
    - tenant_id: A short, stable string that identifies this task or
      conversation (e.g. "refactor-auth", a conversation ID, or a username).
      Use the SAME value for all todo tool calls within the same task.
    - items: The list of steps/tasks. Each item needs a `name` (required)
      and an optional `completed` flag (defaults to false).
    - initial_user_prompt: The user's original request, copied verbatim.
      This is stored for context and returned by `get_items`.
    """
  end

  schema do
    field :tenant_id, {:required, :string}, description: "A short, stable string scoping this list to a specific task or conversation (e.g. a conversation ID, username, or task slug like \"refactor-auth\"). Must stay the same across all todo tool calls for the same task."

    embeds_many :items, description: "The ordered list of steps or sub-tasks to complete." do
      field :name, {:required, :string}, description: "A clear, concise description of the step or task."
      field :completed, :boolean, description: "Whether this item is already done. Defaults to false.", default: false
    end

    field :initial_user_prompt, :string, description: "The user's original request, copied verbatim. Stored for context and returned by get_items.", default: ""
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
