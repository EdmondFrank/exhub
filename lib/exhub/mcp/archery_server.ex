defmodule Exhub.MCP.ArcheryServer do
  @moduledoc """
  MCP Server for Archery SQL audit platform integration.

  This server exposes Archery functionality as MCP tools.

  ## Active Tools
  1. get_instances - Get list of database instances
  2. get_databases - Get databases for a specific instance
  3. query_execute - Execute read-only SQL queries
  4. get_query_history - Get query execution history
  5. get_resource_groups - Get available resource groups
  6. get_group_instances - Get instances for a resource group

  ## Temporarily Disabled Tools
  The following tools are currently commented out pending further testing:
  - sql_check, sql_review, get_workflow_list, get_workflow_detail,
    check_sql, submit_workflow

  The server uses HTTP transport and can be accessed at the /archery/mcp endpoint.

  ## Configuration

  Configure via SecretVault:
  ```bash
  mix scr.insert dev archery_url "https://archery.company.com"
  mix scr.insert dev archery_username "your-username"
  mix scr.insert dev archery_password "your-password"
  ```
  """

  use Anubis.Server,
    name: "exhub-archery-server",
    version: "1.0.0",
    capabilities: [:tools]

  # Register the tool components
  component Exhub.MCP.Tools.ArcheryGetInstances
  component Exhub.MCP.Tools.ArcheryGetDatabases
  component Exhub.MCP.Tools.ArcheryQueryExecute
  # component Exhub.MCP.Tools.ArcherySqlCheck
  # component Exhub.MCP.Tools.ArcherySqlReview
  # component Exhub.MCP.Tools.ArcheryGetWorkflowList
  # component Exhub.MCP.Tools.ArcheryGetWorkflowDetail
  component Exhub.MCP.Tools.ArcheryGetQueryHistory
  component Exhub.MCP.Tools.ArcheryGetResourceGroups
  component Exhub.MCP.Tools.ArcheryGetGroupInstances
  # component Exhub.MCP.Tools.ArcheryCheckSql
  # component Exhub.MCP.Tools.ArcherySubmitWorkflow

  @impl true
  def init(client_info, frame) do
    # You can access client info to customize initialization
    # client_info contains: %{"name" => "client-name", "version" => "x.y.z"}
    _ = client_info

    {:ok, frame}
  end
end
