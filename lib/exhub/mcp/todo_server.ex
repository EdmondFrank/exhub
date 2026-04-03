defmodule Exhub.MCP.TodoServer do
  @moduledoc """
  MCP Server for tracking and managing todo/task lists across conversations.

  ## Purpose
  Use this server to keep a persistent, up-to-date task list while working
  through a multi-step user request. It lets you record your plan at the
  start, tick off items as you complete them, and always know what is left
  to do — even if the conversation is long or interrupted.

  ## Typical workflow
  1. **Start of task** — call `set_items` to record the full plan and the
     user's original request.
  2. **After each step** — call `update_item_completion` to mark the finished
     item as `completed: true`.
  3. **Resuming work** — call `get_items` to reload the current state before
     continuing.
  4. **Task finished** — call `clear_items` to clean up, or just let the list
     expire automatically after 2 hours of inactivity.

  ## About tenant_id
  Every tool requires a `tenant_id` that scopes the list to a specific
  conversation or task. Use a short, stable string that stays the same for
  the entire task — for example the conversation ID, the user's name, or a
  brief task slug such as "refactor-auth". Keep it consistent across all
  tool calls that belong to the same task.

  ## Tools
  - `set_items`              — Create or replace the todo list for a tenant
  - `get_items`              — Read the current todo list for a tenant
  - `update_item_completion` — Mark a single item as done or not done
  - `clear_items`            — Remove all items from a tenant's list

  Todo lists that have not been updated for more than 2 hours are
  automatically purged. The server is accessible at `/todo/mcp`.
  """

  use Anubis.Server,
    name: "exhub-todo-server",
    version: "1.0.1",
    capabilities: [:tools]

  component Exhub.MCP.Tools.TodoSetItems
  component Exhub.MCP.Tools.TodoGetItems
  component Exhub.MCP.Tools.TodoUpdateItem
  component Exhub.MCP.Tools.TodoClearItems

  @impl true
  def init(client_info, frame) do
    _ = client_info
    {:ok, frame}
  end
end
