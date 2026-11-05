# LLM Server (`llm`)

MCP server that runs LangChain chat turns against the models configured in
`Exhub.Llm.LlmConfigServer`. It is also the way Lua code-mode snippets reach an
LLM: because the `lua` server bridges every visible built-in tool, `llm.chat`
is callable from a snippet with no sandbox changes.

- **Endpoint**: `/llm/mcp` (streamable HTTP)
- **Built-in name**: `llm` (reachable via `Exhub.MCP.Hub.BuiltInRegistry` without an HTTP hop)
- **Server**: `Exhub.MCP.LlmServer`
- **Tools**: `models`, `chat`, `chat_json`

## Layers

| Module | Role |
|--------|------|
| `Exhub.Llm.Client` | Canonical LangChain facade: model resolution, message mapping, model building, chain execution, guardrails |
| `Exhub.MCP.LlmServer` | Anubis server registering the three tools |
| `Exhub.MCP.Tools.Llm.Models` | `models` — configured model list |
| `Exhub.MCP.Tools.Llm.Chat` | `chat` — one chat turn |
| `Exhub.MCP.Tools.Llm.ChatJson` | `chat_json` — schema-constrained turn |

`Exhub.Llm.Client` reuses the same building blocks as `Exhub.Llm.Chain`:
`Exhub.TLSCompat.langchain_req_config/1` (TrustAsia-style TLS chains) and
`Exhub.Router.TokenPool` (token-pool key selection).

## Model names

`model` accepts:

- a full configured name — `openai/deepseek-v4.1-flash`
- a bare model — `kimi-k2.6` (tried as-is, then as `openai/<name>`, then by
  suffix against every configured name)
- a `Exhub.Router.ModelVariant` suffix — `openai/qwen3.8-flash--no-think`
  resolves to the base model's configuration
- omitted/empty — the current default (`LlmConfigServer.get_default_llm_name/0`)

## Tools

### `models`

No parameters. Returns the configured names, provider and bare model, plus the
current default, whether calls are enabled, and the allow-list. API keys and
base URLs are never returned.

### `chat`

| Parameter | Type | Description |
|-----------|------|-------------|
| `model` | string | Optional model name (see above) |
| `messages` | array | `[{role, content}]`, roles `system`/`user`/`assistant` |
| `prompt` | string | Single user message, when `messages` is absent |
| `system` | string | System prompt, prepended when no system message is present |
| `temperature` | number | Sampling temperature |
| `max_tokens` | integer | Output cap (default `:default_max_tokens`) |
| `timeout_ms` | integer | Receive timeout, clamped to `:max_timeout_ms` |

`content` is a string or a list of content parts:

```json
{"type": "text", "text": "describe this"}
{"type": "image_url", "url": "https://example.com/a.png", "detail": "low"}
{"type": "image", "data": "<base64>", "media": "image/png"}
```

Returns `{content, model, usage}`; `usage` is `{input, output, total}` when the
provider reports it.

### `chat_json`

Same parameters plus `schema` (a JSON Schema object or its JSON string) and
`strict` (default `true`). Returns `{content, json, model, usage}`; `json` is
`nil` when the reply is not valid JSON (the raw `content` is still returned).

OpenAI-compatible and Google models use native structured output. Anthropic and
Mistral have no such fields in their LangChain struct, so the schema is appended
to the user message as an instruction and the reply is parsed.

## Lua usage

```lua
-- discover
local models = llm.models()
local name = models.models[1].name

-- one turn
local out = llm.chat({model = name, system = "Be brief.", prompt = "What is ExHub?"})
return out.content

-- fan out across models in one round trip
local r = parallel({
  {server = "llm", tool = "chat", args = {prompt = "tldr", model = "kimi-k2.6"}},
  {server = "llm", tool = "chat", args = {prompt = "tldr", model = "qwen3.5-122b-a10b"}}
})
return {a = r[1].content, b = r[2].content}

-- structured output
local json = llm.chat_json({
  prompt = "Extract the title",
  schema = {type = "object", properties = {title = {type = "string"}}, required = {"title"}}
})
return json.json.title
```

## Limits

- **No streaming** — an MCP tool result is single-shot; only the final text is
  returned.
- **No model-side tool calling** — LangChain tools are Elixir closures and
  cannot cross the Lua sandbox. A snippet calls the real MCP tools directly
  instead.
- **Cost** — each call is a billable LLM request. Lua loops are bounded by the
  code-mode instruction and wall-clock budgets, and `parallel` by
  `max_concurrency` (8).

## Configuration

```elixir
config :exhub, :llm_tool,
  enabled: true,
  # nil = every configured model; a list (full or bare names) is an allow-list
  # enforced by `Exhub.Llm.Client.resolve/1`.
  allowed_models: nil,
  default_max_tokens: 4096,
  default_timeout_ms: 180_000,
  max_timeout_ms: 600_000
```

The server is also listed in `Exhub.MCP.Hub.BuiltInRegistry` (name `llm`) and
mounted in `Exhub.Router` at `/llm/mcp`.

## Testing

```sh
MIX_ENV=test mix test --no-start test/exhub/llm/client_test.exs test/exhub/mcp/tools/llm_test.exs
```

The tests cover resolution (full/bare/variant/allowed-list), message and content
part mapping, the TLS `req_config` on the built structs, timeout clamping, and
the tools' validation paths. No test makes a network call.