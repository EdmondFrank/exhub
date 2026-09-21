defmodule Exhub.MCP.TodoServer do
  @moduledoc """
  MCP server exposing a small, multi-tenant todo list for tracking progress
  across a multi-step task.

  Workflow:
  - `set_items`              — create/replace the plan at the start of a task.
  - `update_item_completion` — mark one or more items done as you finish them.
  - `get_items`              — reload the plan (e.g. when resuming).
  - `clear_items`            — drop the list when the task is done.

  Every tool takes a `tenant_id`: a short, stable string scoping the list to one
  task/conversation (e.g. a conversation ID or a slug like "refactor-auth"). Use
  the same value for all calls that belong to the same task.

  Lists that have not been updated for more than 2 hours are purged
  automatically. Endpoint: `/todo/mcp`.
  """

  use Anubis.Server,
    name: "exhub-todo-server",
    version: "1.1.0",
    capabilities: [:tools]

  component(Exhub.MCP.Tools.TodoSetItems)
  component(Exhub.MCP.Tools.TodoGetItems)
  component(Exhub.MCP.Tools.TodoUpdateItem)
  component(Exhub.MCP.Tools.TodoClearItems)

  @impl true
  def init(client_info, frame) do
    _ = client_info
    {:ok, frame}
  end

  @impl true
  def handle_request(request, frame) do
    Exhub.MCP.ServerHelpers.handle_request_with_filtered_tools(__MODULE__, request, frame)
  end
end
