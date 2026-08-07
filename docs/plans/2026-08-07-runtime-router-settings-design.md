# Runtime Router Settings Design

## Goal

Allow outbound router headers to be configured at runtime in JSON, without recompiling `config.exs` or restarting Exhub. The initial use case is adding `X-package-id: 8848` for DeepSeek models.

## Configuration file

The path is selected in this order:

1. `EXHUB_ROUTER_CONFIG`, when set.
2. `~/.config/exhub/router.json`.

Example:

```json
{
  "headers": [
    {
      "models": ["deepseek-*"],
      "providers": ["openai"],
      "headers": {
        "X-package-id": "8848"
      }
    }
  ]
}
```

Model and provider matching are case-insensitive. Model patterns use glob semantics, initially supporting `*` for model families. A missing file means no configured custom headers. Invalid JSON or invalid rule shapes are logged and ignored rather than taking down the router.

## Runtime behavior

`Exhub.Router.Config.get_auth_headers/2` remains the single entry point for proxy authorization and custom outbound headers. It will:

1. Build the existing provider/model-specific authentication headers.
2. Load the JSON settings at runtime using a small TTL cache or file-metadata-aware cache.
3. Select rules matching the provider and model.
4. Merge configured headers over the base list, with configured values taking precedence for the same case-insensitive header name.

Only headers explicitly present in the router settings file are emitted. Incoming client headers are not copied into the upstream request. Authentication headers remain controlled by Exhub and cannot be overridden by JSON settings.

The existing built-in DeepSeek package-header behavior will be migrated into the JSON example/default configuration during implementation, avoiding a second model-specific header mechanism in code.

## Reload and observability

Because settings are read at runtime, editing the file takes effect without recompilation. The existing `/system/reload` endpoint may continue to hot-reload BEAM modules but is not required for JSON settings. The implementation should expose a small explicit reload/cache-invalidation function for tests and operational tooling, and log the selected path plus parse/validation failures without logging secret header values.

## Security and failure handling

- Restrict configuration to a local file path selected by environment or the user-home default.
- Do not evaluate JSON values as code.
- Do not support request-header interpolation initially.
- Do not log header values.
- Treat missing files as an empty configuration.
- Treat malformed files/rules as configuration errors with safe fallback to base authentication headers.
- Never allow configured headers to replace authorization headers.

## Testing

Add unit tests covering:

- Exact and wildcard model matching.
- Provider filtering.
- Case-insensitive matching and header-name replacement.
- Missing and malformed files.
- Runtime reload after changing the file.
- Protection of authorization headers.
- DeepSeek configuration producing `X-package-id: 8848` while unrelated models do not.

Use temporary files and environment isolation so tests do not read the developer's real router settings.

## Alternatives considered

- **Compile-time `config.exs` settings:** simple, but requires recompilation and is unsuitable for operational header changes.
- **A dedicated GenServer with explicit reload only:** provides predictable caching, but file edits would not apply until an operation is invoked; a small cache with explicit invalidation provides the same control while keeping request behavior simple.
- **Per-request client header passthrough:** rejected because it risks forwarding untrusted or sensitive headers upstream.
