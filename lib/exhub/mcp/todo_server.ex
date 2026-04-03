defmodule Exhub.MCP.TodoServer do
  @moduledoc """
  MCP Server for multi-tenant todo list management.

  Exposes four tools via the Model Context Protocol:
  1. set_items            — Initialise / overwrite a tenant's todo list
  2. get_items            — Retrieve a tenant's current todo list
  3. update_item_completion — Toggle the completed flag of a single item
  4. clear_items          — Remove all items from a tenant's todo list

  Todo lists that have not been updated for more than 2 hours are
  automatically purged by the backing `Exhub.MCP.TodoStore`.

  The server is accessible at the `/todo/mcp` HTTP endpoint.
  """

  use Anubis.Server,
    name: "exhub-todo-server",
    version: "1.0.0",
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
