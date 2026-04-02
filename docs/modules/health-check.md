# exhub-health-check

The `exhub-health-check` module provides URL monitoring with scheduled health checks and webhook notifications.

## Setup

The health check feature is built into the Exhub application. No additional Emacs configuration is required.

## Configuration

Configure health checks in `config/config.exs`:

```elixir
config :exhub, Exhub.HealthCheck,
  targets: [
    [name: "Example API", url: "https://api.example.com/health"],
    [name: "Main Site", url: "https://example.com", expected_status: 200, timeout: 30_000]
  ],
  webhook_url: "https://open.feishu.cn/open-apis/bot/v2/hook/your-webhook-token",
  webhook_provider: :feishu,  # or :default for generic webhooks
  jobs: [
    {"*/5 * * * *", {Exhub.HealthCheck, :run_health_checks, []}}
  ]
```

### Configuration Options

- `targets`: List of targets to monitor, each with:
  - `name`: Display name for the target
  - `url`: URL to check
  - `expected_status`: Expected HTTP status code (default: 200)
  - `timeout`: Request timeout in milliseconds (default: 30_000)
  - `method`: HTTP method (`:get` or `:head`, default: `:get`)
- `webhook_url`: URL to send notifications on failures
- `webhook_provider`: `:feishu` for Feishu/Lark or `:default` for generic JSON webhooks
- `jobs`: Quantum scheduler jobs (cron syntax)

## Webhook Providers

### Feishu/Lark

For Feishu bot webhooks, use `webhook_provider: :feishu`. The message format follows Feishu's text message specification.

### Generic Webhooks

For generic webhooks, use `webhook_provider: :default`. The payload includes:

```json
{
  "event": "health_check_failure",
  "timestamp": "2026-02-10T10:00:00Z",
  "failures": [
    {"name": "Example API", "url": "https://api.example.com/health", "reason": "timeout"}
  ]
}
```
