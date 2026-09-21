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
    Create or replace the todo list for a task. Call this at the START of a
    multi-step task to record the full plan and the user's original request,
    then work through the items and mark them done with `update_item_completion`.

    Replaces any existing list for `tenant_id`, so do not call it mid-task
    unless you mean to rewrite the whole plan.

    - tenant_id: stable task/conversation id, reused by every todo call.
    - items: ordered steps, each with a `name` and optional `completed` (default false).
    - initial_user_prompt: the user's original request, copied verbatim.
    """
  end

  schema do
    field(:tenant_id, {:required, :string},
      description:
        "Stable task/conversation id (e.g. a conversation ID or task slug like \"refactor-auth\"); use the same value for every todo call."
    )

    embeds_many :items, description: "The ordered plan of steps or sub-tasks." do
      field(:name, {:required, :string}, description: "A clear, concise description of the step.")

      field(:completed, :boolean,
        description: "Whether this step is already done. Defaults to false.",
        default: false
      )
    end

    field(:initial_user_prompt, :string,
      description: "The user's original request, copied verbatim.",
      default: ""
    )
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
