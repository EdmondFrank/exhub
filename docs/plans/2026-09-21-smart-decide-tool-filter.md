# Smart Decide tool-relevance filter for MCP Hub retrieval

**Goal:** cut the number of tool definitions handed to the model by using the
Smart Decide (System One) decision model to judge whether each TF-IDF candidate
from `retrieve_tools` is actually relevant to the query.

**Status:** implemented.

## Problem

`retrieve_tools` returned the top-N results of an in-memory TF-IDF search
(`Exhub.MCP.Hub.ToolSearch`) regardless of how relevant they really were. TF-IDF
is cheap but coarse: it matches on shared tokens, so a query like "read file"
also surfaces tools that merely mention "file". Every returned tool definition
costs input tokens on the next model turn.

## Approach

Two-stage retrieval: cheap recall (TF-IDF) then sharp precision (Smart Decide).

```text
query -> ToolSearch.search(index, candidate_limit)   # wide TF-IDF pool
      -> ToolRelevance.filter(query, candidates)     # one noul question per tool
      -> Enum.take(limit)                            # final result
```

### One tool per request

The model has a ~2k-token context (prompts above 2048 tokens are rejected), so a
single tool description is all that fits comfortably. Each candidate therefore
becomes its own request:

```text
state:        Tool: <server>__<name>
              Server: <server>
              Description: <description>      # truncated to state_char_limit
instructions: Decide whether the tool described in the state is useful for
              this task: "<query>" ...          # query truncated to query_char_limit
```

Requests run concurrently with `Task.async_stream/3`
(`max_concurrency: 8`, per-item `timeout`, `on_timeout: :kill_task`,
`ordered: true`), so judging 30 candidates is roughly 4 round-trips.

### Fault tolerance

- **Fail-open:** a per-tool failure (timeout, API error, decode error, raise) is
  treated as *relevant* so recall is never silently reduced; the count is
  reported in the filter stats.
- **Skip:** a blank query or empty candidate list skips the pass entirely.
- **Empty result is allowed:** if every candidate is judged irrelevant the tool
  returns no tools - that is the signal the query has no good match.

## Changes

| File | Change |
|------|--------|
| `lib/exhub/mcp/tools/smart_decide.ex` | New public `decide/3` programmatic API; HTTP path refactored into `request/5` + `decode_response/3`; `execute/2` delegates to it. |
| `lib/exhub/mcp/hub/tool_relevance.ex` | New module: `filter/3`, `relevant?/2`, `config/0`, `enabled?/0`. |
| `lib/exhub/mcp/tools/hub/retrieve_tools.ex` | Two-stage pipeline, `filter` param, widened candidate pool, `filtered`/`candidates` in the response. |
| `config/config.exs` | `config :exhub, Exhub.MCP.Hub.ToolRelevance` defaults. |
| `test/exhub/mcp/hub/tool_relevance_test.exs` | Unit tests with an injected decider (no network). |
| `test/exhub/mcp/tools/smart_decide_test.exs` | `decide/3` validation tests. |

## Configuration

```elixir
config :exhub, Exhub.MCP.Hub.ToolRelevance,
  enabled: true,
  candidate_limit: 30,
  max_concurrency: 8,
  threshold: 0.5,
  timeout: 30_000,
  state_char_limit: 1500,
  query_char_limit: 800
```

In-code defaults in `ToolRelevance` apply for missing keys, so a hot-reloaded
release picks up the feature without a runtime `Application.put_env/3`.

## Trade-offs

- **Cost/latency:** each filtered `retrieve_tools` call issues about
  `candidate_limit` System One requests. Tune `candidate_limit`/`max_concurrency`,
  or set `filter: false` per call, when latency matters more than token savings.
- **Scope:** only the MCP `retrieve_tools` tool is filtered; the HTTP search
  endpoint and `ClientManager.search_tools/2` remain pure TF-IDF.
- **Fail-open vs fail-closed:** fail-open keeps irrelevant tools on error but
  never drops a relevant one - judged the safer default for discovery.

## Verification

- `mix test --no-start test/exhub/mcp/hub/tool_relevance_test.exs test/exhub/mcp/tools/smart_decide_test.exs`
- `mix compile --force --warnings-as-errors`
- `MIX_ENV=prod mix release --overwrite` + `Exhub.HotReload.reload/0`
- Live: `retrieve_tools` via the `mcp-hub` server, with and without `filter`.