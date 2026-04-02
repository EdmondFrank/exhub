# Time MCP Server

Exhub provides an MCP server for time-related functionality, enabling timezone-aware operations through the Model Context Protocol.

## Overview

The Time MCP server exposes time utilities as MCP tools, allowing LLMs and other MCP clients to:

- Get current time in any IANA timezone
- Convert time between different timezones

## MCP Server

The Time MCP server runs at `/time/mcp` endpoint.

**Module:** `Exhub.MCP.TimeServer`

## Available Tools

| Tool               | Description                                  |
|--------------------|----------------------------------------------|
| `get_current_time` | Get current time in a specific IANA timezone |
| `convert_time`     | Convert time between two IANA timezones      |

## Tool Details

### get_current_time

Returns the current time for a specified IANA timezone.

**Parameters:**
- `timezone` (string, required) - IANA timezone identifier (e.g., "America/New_York", "Asia/Shanghai")

**Returns:**
- Current datetime in the specified timezone

### convert_time

Converts a datetime from one timezone to another.

**Parameters:**
- `source_timezone` (string, required) - Source IANA timezone
- `time` (string, required) - Time to convert (ISO 8601 format or datetime string)
- `target_timezone` (string, required) - Target IANA timezone

**Returns:**
- Converted datetime in the target timezone

## Usage Example

```elixir
# The server is automatically started with the Exhub application
# Access via MCP client at http://localhost:4000/time/mcp

# Example: Get current time in Tokyo
# Tool: get_current_time
# Args: %{"timezone" => "Asia/Tokyo"}

# Example: Convert time from New York to London
# Tool: convert_time
# Args: %{
#   "source_timezone" => "America/New_York",
#   "time" => "2024-01-15 10:00:00",
#   "target_timezone" => "Europe/London"
# }
```

## Architecture

The Time MCP server consists of:

- **`Exhub.MCP.TimeServer`** - MCP server implementation
- **`Exhub.MCP.Tools.GetCurrentTime`** - Get current time tool
- **`Exhub.MCP.Tools.ConvertTime`** - Time conversion tool

## Supported Timezones

The server supports all valid IANA timezone identifiers, including:

- `UTC` - Coordinated Universal Time
- `America/New_York` - Eastern Time
- `America/Los_Angeles` - Pacific Time
- `Europe/London` - Greenwich Mean Time / British Summer Time
- `Europe/Paris` - Central European Time
- `Asia/Shanghai` - China Standard Time
- `Asia/Tokyo` - Japan Standard Time
- `Asia/Dubai` - Gulf Standard Time
- `Australia/Sydney` - Australian Eastern Time

For a complete list, see the [IANA Time Zone Database](https://www.iana.org/time-zones).

## See Also

- [IANA Time Zones](https://www.iana.org/time-zones)
- [MCP Documentation](https://modelcontextprotocol.io/)
