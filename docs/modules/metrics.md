# Performance Metrics

## Overview

Exhub tracks performance metrics for LLM proxy requests, MCP tool calls, and Hercules runs. Metrics are stored in a two-tier ETS-based store with JSON persistence, queryable via REST API and visualised on the dashboard.

## Architecture

```text
┌─────────────────────────────────────────────────────────────────────┐
│                        Call Sites                                   │
│  Exhub.Router (LLM proxy routes)                                    │
│  Exhub.MCP.Hub.ClientManager (tool calls)                           │
│  Exhub.MCP.Hub.Server (tool calls)                                  │
└──────────────────────┬──────────────────────────────────────────────┘
                       │ PerformanceTracker.record_*  (cast, fire-and-forget)
                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│  Exhub.Metrics.PerformanceTracker                                   │
│  • record_llm_proxy/3, record_mcp_tool_call/3, record_hercules_run/3│
│  • Guards: skips if PerformanceStore not running, never raises      │
└──────────────────────┬──────────────────────────────────────────────┘
                       │ GenServer.cast
                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│  Exhub.Metrics.PerformanceStore (GenServer)                         │
│  ┌────────────────────────────────────────────────────────────────┐ │
│  │ :perf_raw (ETS :bag, public)                                   │ │
│  │   Individual records — ring buffer, max 10,000                 │ │
│  │   Used for percentile calculations                             │ │
│  └────────────────────────────────────────────────────────────────┘ │
│  ┌────────────────────────────────────────────────────────────────┐ │
│  │ :perf_aggregate (ETS :set, protected)                          │ │
│  │   Aggregated stats keyed by {metric_type, entity, date}        │ │
│  │   Running totals: count, total_ms, min, max, error_count       │ │
│  └────────────────────────────────────────────────────────────────┘ │
│  Persists to ~/.config/exhub/performance_metrics.ndjson             │
└──────────────────────┬──────────────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│  Exhub.Metrics.PerformanceStats                                     │
│  • Aggregation: by model, tool, provider, day, type                 │
│  • Percentiles: p50, p95, p99 (from raw samples)                    │
│  • Summary: total requests, avg, p50/p95/p99, error rate            │
│  • Dashboard data: summary + trends + top models/tools + recent     │
└──────────────────────┬──────────────────────────────────────────────┘
                       │
              ┌────────┴─────────┐
              ▼                  ▼
   REST API endpoints     Dashboard UI
   (/api/v1/metrics/*)    (DashboardView)
```

## Modules

| Module                             | Role                                                                           |
|------------------------------------|--------------------------------------------------------------------------------|
| `Exhub.Metrics.PerformanceStore`   | GenServer owning ETS tables; records metrics, persists to disk, serves queries |
| `Exhub.Metrics.PerformanceTracker` | Convenience API for call sites; fire-and-forget casts with error suppression   |
| `Exhub.Metrics.PerformanceStats`   | Aggregation/query API; percentiles, summaries, trends, dashboard data          |

## Metric Types

| Type             | Entity         | Recorded Where                          | Description                        |
|------------------|----------------|-----------------------------------------|------------------------------------|
| `:llm_proxy`     | Model name     | `Exhub.Router` LLM proxy routes         | LLM API proxy request latency      |
| `:mcp_tool_call` | Tool name      | `Hub.ClientManager`, `Hub.Server`       | MCP tool call latency              |
| `:hercules_run`  | Run ID         | (future)                                | Hercules test run duration         |

## Storage

### Two-Tier ETS

| Table             | Type   | Access       | Purpose                                          |
|-------------------|--------|--------------|--------------------------------------------------|
| `:perf_raw`       | `:bag` | `:public`    | Individual metric records (ring buffer, max 10k) |
| `:perf_aggregate` | `:set` | `:protected` | Aggregated stats by `{type, entity, date}`       |

### Persistence

- Data file: `~/.config/exhub/performance_metrics.ndjson` (NDJSON format, one JSON object per line, null fields omitted)
- Migrated automatically from old `performance_metrics.json` on first write
- Periodic flush to disk (every 60s when dirty, plus on shutdown)
- Loaded on startup; `await_loaded/2` call available for synchronous wait

## REST API

All endpoints return JSON with `{success: true, data: ...}` on success or `{success: false, error: "..."}` on error.

### GET /api/v1/metrics/recent

Retrieve recent raw metric records.

| Parameter | Type    | Default | Description                                                          |
|-----------|---------|---------|----------------------------------------------------------------------|
| `type`    | string  | `all`   | Filter by metric type (`llm_proxy`, `mcp_tool_call`, `hercules_run`) |
| `model`   | string  | —       | Filter by model name (maps to entity for `:llm_proxy` metrics)       |
| `limit`   | integer | `100`   | Maximum number of records to return                                  |

**Response:**
```json
{
  "success": true,
  "data": [
    {
      "metric_type": "llm_proxy",
      "entity": "gpt-4",
      "duration_ms": 350,
      "status": "success",
      "provider": "openai",
      "timestamp": "2026-07-31T12:00:00Z"
    }
  ],
  "count": 1
}
```

### GET /api/v1/metrics/stats

Retrieve aggregated statistics grouped by a dimension.

| Parameter    | Type   | Default | Description                                                    |
|--------------|--------|---------|----------------------------------------------------------------|
| `group_by`   | string | `model` | Group dimension: `model`, `tool`, `provider`, `day`, `type`    |
| `start_date` | string | —       | ISO 8601 date filter (inclusive)                               |
| `end_date`   | string | —       | ISO 8601 date filter (inclusive)                               |
| `model`      | string | —       | Filter by model name (maps to entity for `:llm_proxy` metrics) |

**Response:**
```json
{
  "success": true,
  "data": [
    {
      "entity": "gpt-4",
      "request_count": 150,
      "avg_ms": 320,
      "min_ms": 100,
      "max_ms": 2000,
      "total_ms": 48000,
      "error_count": 3,
      "error_rate": 2.0
    }
  ],
  "group_by": "model"
}
```

### GET /api/v1/metrics/summary

Retrieve overall summary statistics.

| Parameter    | Type   | Default | Description                                                    |
|--------------|--------|---------|----------------------------------------------------------------|
| `start_date` | string | —       | ISO 8601 date filter (inclusive)                               |
| `end_date`   | string | —       | ISO 8601 date filter (inclusive)                               |
| `model`      | string | —       | Filter by model name (maps to entity for `:llm_proxy` metrics) |

**Response:**
```json
{
  "success": true,
  "data": {
    "total_requests": 1000,
    "avg_duration_ms": 320,
    "p50": 250,
    "p95": 750,
    "p99": 1500,
    "error_count": 20,
    "error_rate": 2.0,
    "total_duration_ms": 320000
  }
}
```

### GET /api/v1/metrics/percentiles

Retrieve percentile statistics (p50, p95, p99) for a specific metric type and optional entity.

| Parameter    | Type   | Default | Description                                                |
|--------------|--------|---------|------------------------------------------------------------|
| `type`       | string | `all`   | Metric type (`llm_proxy`, `mcp_tool_call`, `hercules_run`) |
| `entity`     | string | —       | Filter by entity name (model, tool, or run ID)             |
| `model`      | string | —       | Alias for `entity` (filter by model name)                  |
| `start_date` | string | —       | ISO 8601 date filter (inclusive)                           |
| `end_date`   | string | —       | ISO 8601 date filter (inclusive)                           |

### GET /api/v1/metrics/dashboard

Retrieve combined dashboard data in a single call.

| Parameter    | Type    | Default | Description                                                    |
|--------------|---------|---------|----------------------------------------------------------------|
| `days`       | integer | —       | Number of days for trend data (default: 30)                    |
| `start_date` | string  | —       | ISO 8601 date filter (overrides `days`)                        |
| `end_date`   | string  | —       | ISO 8601 date filter (overrides `days`)                        |
| `model`      | string  | —       | Filter by model name (maps to entity for `:llm_proxy` metrics) |

**Response includes:**
- `summary` — overall statistics (total, avg, p50/p95/p99, error rate)
- `trends` — daily breakdown
- `top_models` — top 5 models by total duration
- `top_tools` — top 5 tools by total duration
- `by_type` — breakdown by metric type
- `recent_metrics` — 50 most recent raw records

## Dashboard UI

The web dashboard (`Exhub.Router.DashboardView`) includes a "⚡ Performance Metrics" section with:

- **Stat cards**: Total Metrics, Avg Duration, P50/P95/P99 Latency, Error Rate
- **Top Tools by Duration table**: Tool name, call count, avg/p50/p95/p99, error count, error %
- **Recent Metrics table**: Timestamp, type, entity, duration, status badge
- **Auto-refresh**: Every 60 seconds via `loadPerformanceMetrics()`
- **Status badges**: Green (success), red (error), amber (timeout)

## Supervision

`Exhub.Metrics.PerformanceStore` is registered in the `Exhub.Application` supervision tree:

```elixir
# Performance Metrics Tracking
{Exhub.Metrics.PerformanceStore, name: Exhub.Metrics.PerformanceStore},
```

## Instrumentation Points

### LLM Proxy Routes (`Exhub.Router`)

All LLM proxy routes record `:llm_proxy` metrics after forwarding the upstream request:

| Route Pattern               | Provider          | Model                            |
|-----------------------------|-------------------|----------------------------------|
| `/openai/v1/*path` (GET)    | `openai`          | `openai`                         |
| `/openai/v1/*path` (POST)   | `openai`          | from request body or `openai`    |
| `/anthropic/v1/*path`       | `anthropic`       | from request body or `anthropic` |
| `/burncloud/v1/*path`       | `burncloud`       | `burncloud`                      |
| `/bailiancloud/v1/*path`    | `bailiancloud`    | `bailiancloud`                   |
| `/baidu-anthropic/v1/*path` | `baidu-anthropic` | `baidu-anthropic`                |
| `/google/v1/*path`          | `google`          | `google`                         |
| `/cohere/v1/*path`          | `cohere`          | `cohere`                         |
| `/samba/v1/*path`           | `samba`           | `samba`                          |

### MCP Hub Tool Calls

- `Exhub.MCP.Hub.ClientManager` — records `:mcp_tool_call` metrics in three paths:
  1. Async tool call success (`Task` result received)
  2. Async tool call crash (`Task` DOWN with non-normal reason)
  3. Synchronous tool call (built-in server direct call)
- `Exhub.MCP.Hub.Server` — records `:mcp_tool_call` metrics in the `do_handle_tool_call` handler

Tool name format: `"{server_name}__{tool_name}"` (e.g., `"desktop__execute_command"`).
