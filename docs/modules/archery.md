# Archery SQL Audit Integration

Exhub provides MCP-based integration with the [Archery](https://github.com/hhyo/Archery) SQL audit platform, enabling database operations through the Model Context Protocol.

## Overview

The Archery integration exposes Archery's functionality as MCP tools, allowing LLMs and other MCP clients to:

- Query database instances and their databases
- Execute read-only SQL queries
- Retrieve query execution history
- Access resource groups and their instances

## Configuration

Configure Archery connection via SecretVault:

```bash
mix scr.insert dev archery_url "https://archery.company.com"
mix scr.insert dev archery_username "your-username"
mix scr.insert dev archery_password "your-password"
```

## MCP Server

The Archery MCP server runs at `/archery/mcp` endpoint.

**Module:** `Exhub.MCP.ArcheryServer`

## Available Tools

### Active Tools

| Tool                  | Description                                          |
|-----------------------|------------------------------------------------------|
| `get_instances`       | Get list of database instances configured in Archery |
| `get_databases`       | Get databases for a specific instance                |
| `query_execute`       | Execute read-only SQL queries against databases      |
| `get_query_history`   | Get query execution history                          |
| `get_resource_groups` | Get available resource groups                        |
| `get_group_instances` | Get instances belonging to a resource group          |

### Disabled Tools

The following tools are temporarily disabled pending further testing:

- `sql_check` - SQL syntax checking
- `sql_review` - SQL review workflows
- `get_workflow_list` - List SQL workflow requests
- `get_workflow_detail` - Get workflow details
- `check_sql` - Advanced SQL checking
- `submit_workflow` - Submit SQL workflow for approval

## Usage Example

```elixir
# The server is automatically started with the Exhub application
# Access via MCP client at http://localhost:4000/archery/mcp
```

## Architecture

The integration consists of:

- **`Exhub.MCP.ArcheryServer`** - MCP server implementation
- **`Exhub.MCP.Archery.Client`** - HTTP client for Archery API
- **Tool modules** - Individual MCP tools (e.g., `ArcheryGetInstances`, `ArcheryQueryExecute`)

## Security Notes

- Only read-only queries are permitted through `query_execute`
- Authentication is handled via the Archery API with username/password
- CSRF tokens are automatically managed by the client

## See Also

- [Archery API](https://github.com/hhyo/Archery/wiki/api)
- [MCP Documentation](https://modelcontextprotocol.io/)
