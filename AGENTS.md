# AGENTS.md

Guidance for AI agents working on this repository.

## Running tests

### The reliable way (unit tests)

Most ExUnit tests in this repo do **not** need the ExHub application booted.
Run them with `--no-start` to skip app boot:

```sh
mix test --no-start test/exhub/router/                              # a directory
mix test --no-start test/exhub/proxy_plug_stream_test.exs           # a file
mix test --no-start test/exhub/router/ test/exhub/mcp/encoding_test.exs
```

Prefer targeted file/directory runs. **Avoid plain `mix test --no-start`
(full suite)**: most tests pass, but a graceful-restart path schedules
`:init.stop/0` (3s) which halts the VM before the final summary prints, so the
run never reports a clean "Finished in …" line.

### Why not plain `mix test`?

Plain `mix test` boots the ExHub application first. In the current local
environment that boot **crashes**, so plain `mix test` exits with an error
before reliably running tests:

- `Application exhub exited: ... failed to start child:
  {:ranch_listener_sup, Exhub.Router.HTTP}` — the HTTP listener fails to start
  (typically a port conflict with a running dev instance).
- An external `skillport` MCP subprocess (Python/FastMCP) crashes with a
  `BrokenPipeError` and pollutes the output.

This is pre-existing local environment noise, unrelated to code changes. If your
change causes a test failure, reproduce it with `mix test --no-start <that
file>` rather than reasoning from a plain `mix test` error.

### Caveats

- Tests that exercise app-supervised servers/endpoints (e.g. desktop tool tests,
  restart/hot-reload, brain integration) may require the app booted and can fail
  in this environment for the same reasons — not because of your change.
  `graceful_restart_test.exs` itself is pure; the `:init.stop/0` comes from
  elsewhere during a full run.
- `test/test_helper.exs` configures `ExUnit.configure(exclude: [:e2e_ssh])` —
  SSH e2e tests are excluded by default.
- `config/test.exs` sets `config :exhub, :port, 0` (random port) to avoid
  conflicts with a running dev instance.
- Expect output noise: `[TokenUsage] TokenUsageStore not available, skipping
  tracking` warnings, `The function passed as a handler with ID
  {:swarm_ex, ...}` info lines, and the skillport MCP banner. All normal.

## Compile & style checks

```sh
mix compile --force --warnings-as-errors   # project compiles warning-clean
mix format <changed files>                 # format only what you touched
```

Note: `mix format --check-formatted` (repo-wide) currently fails on one
pre-existing unformatted file: `lib/exhub/mcp/brain/search/policy.ex`. Don't
chase it; format only the files you changed.

## Operating the running prod release (zero-downtime deploy)

A **prod release is usually already running** on port 9069 (started by Emacs
via `exhub-start-elixir`; binary at `_build/prod/rel/exhub/bin/exhub`). Its
stdout/stderr is captured in the Emacs `*exhub*` buffer.

> **⚠️ Never restart or stop the running VM** — no `exhub_restart` tool calls,
> no `bin/exhub stop`, no killing the beam process. All local AI tooling LLM
> API traffic is forwarded through ExHub's upstream proxy (`proxy_plug.ex`);
> a restart cuts off in-flight model requests, including your own session's.

### Build a new prod release

```sh
MIX_ENV=prod mix release --overwrite
# Output: _build/prod/rel/exhub   (verify: bin/exhub version → "exhub 0.1.0")
```

`runtime.exs` is evaluated at VM boot (SecretVault), not at build time, so
assembling the release needs no `SECRET_VAULT_PASSWORD`.

### Apply code changes without downtime

`Exhub.HotReload.reload/0` scans the running release's ebin dir
(`_build/prod/rel/exhub/lib/exhub-<vsn>/ebin`) and soft-purges + reloads every
`:exhub` module. In-flight requests finish on old code; new requests use new
code immediately.

1. Edit code, then `MIX_ENV=prod mix release --overwrite` (fresh beams land in
   the release ebin).
2. Trigger reload (either works):
   - MCP hub upstream `exhub` → tool `exhub_hot_reload` (no params), or
   - RPC: `_build/prod/rel/exhub/bin/exhub rpc "Exhub.HotReload.reload_and_summarize()"`
3. Rotate secrets without downtime: tool `exhub_reload_keys`, or
   `bin/exhub rpc "Exhub.Router.Config.reload_from_scr()"`.
4. Verify with `exhub_get_status` / `exhub_get_version` or
   `curl localhost:9069`.

### Self-management MCP tools (upstream server `exhub`, route `/exhub/mcp`)

| Tool | Safe? | Purpose |
|------|-------|---------|
| `exhub_hot_reload` | ✅ zero-downtime | Reload compiled BEAM modules |
| `exhub_reload_keys` | ✅ zero-downtime | Re-read API keys from SecretVault |
| `exhub_get_status` / `exhub_get_version` | ✅ read-only | VM stats / versions |
| `exhub_restart` (soft/hard) | ⛔ do not use | Restarts the VM; kills proxied LLM traffic |

### Observing service logs via Emacs

The `*exhub*` buffer holds the backend process output (the service log,
including ANSI escapes). Through the MCP hub upstream `emacs`:

```json
// emacs_read_buffer
{"buffer_name": "*exhub*", "start_line": 1, "end_line": 50}
```

Related buffers: `*exhub-reload*` and `*exhub-release*` (build output),
`*Messages*`. Use `emacs_list_buffers` to discover them.

## Test layout

- `test/exhub/` — module-level unit tests mirroring `lib/exhub/` structure
  (e.g. `test/exhub/router/` ↔ `lib/exhub/router/`, plus MCP tool tests under
  `test/exhub/mcp/`).
- `test/exhub/*_test.exs` at the top level for cross-module behavior
  (e.g. `proxy_plug_stream_test.exs`, `graceful_restart_test.exs`).
- New tests typically use `use ExUnit.Case, async: true` and avoid booting the
  app; prefer pure functions over side-effectful setup.