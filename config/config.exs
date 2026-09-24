import Config

config :elixir, :time_zone_database, Tzdata.TimeZoneDatabase

config :exhub, :shell, "zsh"

# Gitee AI (moark) token pool: two billing-mode access tokens and the
# context-token threshold that selects between them (< threshold →
# token-based, >= threshold → request-based). Override in runtime.exs.
config :exhub,
  giteeai_token_api_key: "",
  giteeai_request_api_key: "",
  giteeai_pool_threshold: 20_000

# Dedicated LLM for the exhub-translate module. Set to a name from the `llms`
# config map (e.g. "codestral/codestral-latest"); nil (default) → the default
# LLM model. Per-call override via opts[:llm] takes precedence.
# Override in runtime.exs or via the EXHUB_TRANSLATE_LLM env var.
config :exhub, :translate_llm, "openai/hy-mt2-30b-a3b"

# Obsidian vault path for the Brain MCP server.
# Override in runtime.exs or environment-specific config.
config :exhub, :obsidian_vault_path, "~/GTD/PKB"

# `probe` binary used by the Desktop `search_files` tool's semantic mode.
# Different probe builds rank results differently and run at noticeably
# different speeds — the npm-bundled build (~/.bun/bin/probe) is ~2.5x slower
# and returns ~2x the output tokens of the native build — so pin the binary
# instead of relying on PATH resolution order. Set to nil to fall back to the
# first `probe` on the system PATH.
config :exhub, :probe_binary, "/usr/local/bin/probe"

# Brain vault search ranking defaults. Tunable per-call via brain_search_vault
# `fusion`/`weights`/`min_score` params, which are merged over these defaults.
config :exhub, :brain_ranking, %{
  "fusion" => "weighted_sum",
  "weights" => %{
    "bm25" => 0.4,
    "title_match" => 0.25,
    "tag_match" => 0.15,
    "freshness" => 0.1,
    "link_authority" => 0.1,
    "semantic" => 0.3
  },
  "min_score" => 0.0
}

# Brain vault search policies. A policy bundles retrieval + ranking
# hyper-parameters; the default ("auto") picks a policy from query heuristics.
#   - "default_policy": "auto" | any built-in/custom policy name
#   - "semantic_autodetect": allow conversational queries to auto-enable vector
#     search (requires a configured embedding provider under :brain_rag)
#   - "policies": custom/overridden policies, deep-merged over built-ins of the
#     same name (built-ins: balanced, keyword, semantic, recency, filename)
config :exhub, :brain_search, %{
  "default_policy" => "auto",
  "semantic_autodetect" => true,
  "policies" => %{}
}

# Brain RAG (semantic/vector search) configuration.
# Provider is "openai" (default) or "gitee_ai" (moark endpoint).
# - For "openai", the API key comes from :exhub -> :openai_api_key.
# - For "gitee_ai", the API key comes from :exhub -> :giteeai_api_key.
#
# Model: Qwen3-Embedding-4B (1024-dim) — recommended for this vault because
# ~48% of notes contain Chinese and Qwen3-Embedding is natively bilingual
# (中英双语) with a 32k token context window. Free to use on moark.
config :exhub, :brain_rag, %{
  "provider" => "gitee_ai",
  "embedding_model" => "Qwen3-Embedding-4B",
  "api_base" => "https://api.moark.com/v1",
  "dim" => 1024,
  # index_path defaults to ~/.config/exhub/brain_index.db if unset
  "batch_size" => 16,
  "max_chars" => 2000,
  "min_chars" => 32
}

config :exhub, :proxy_providers, ["openrouter"]

config :exhub, :secret_vault,
  default: [
    password: System.get_env("SECRET_VAULT_PASSWORD", "")
  ]

# secrets_dir: "priv/secrets"

# Brain index refresh — daily incremental rebuild of the vector index
# (Runs at 3:00 AM by default; change the cron expression as needed.)
config :exhub, Exhub.BrainIndexRefresh,
  jobs: [
    daily: [schedule: "0 3 * * *", task: {Exhub.BrainIndexRefresh, :run_refresh, []}]
  ]

# Brain vault search: Smart Decide (System One) relevance filter applied after
# the ranked candidate pool in `brain_search_vault`.
#   - `enabled`: master switch (per-call override via `brain_search_vault.filter`)
#   - `candidate_limit`: ranked pool judged when filtering (>= the policy's `top_n`)
#   - `max_concurrency`: concurrent System One requests (one note per request)
#   - `threshold`: minimum `noul` probability to keep a note
#   - `state_char_limit`/`query_char_limit`: truncation to fit the ~8k-token
#     input budget (~3 chars/token for code, ~4.3 for prose)
#   - `fallback`: return the ranked pool when nothing is judged relevant
# In-code defaults in `Exhub.MCP.Brain.Search.Relevance` apply for any missing key.
config :exhub, Exhub.MCP.Brain.Search.Relevance,
  enabled: true,
  candidate_limit: 20,
  max_concurrency: 20,
  threshold: 0.5,
  timeout: 30_000,
  state_char_limit: 18000,
  query_char_limit: 3200,
  fallback: true

# MCP Hub tool retrieval: Smart Decide (System One) relevance filter applied
# after the TF-IDF candidate search in `retrieve_tools`.
#   - `enabled`: master switch (per-call override via `retrieve_tools.filter`)
#   - `candidate_limit`: TF-IDF pool judged when filtering (>= the tool's `limit`)
#   - `max_concurrency`: concurrent System One requests (one tool per request)
#   - `threshold`: minimum `noul` probability to keep a tool
#   - `state_char_limit`/`query_char_limit`: truncation to fit the ~8k-token
#     input budget (~3 chars/token for code, ~4.3 for prose)
#   - `exclude_servers`: servers never offered as candidates (the hub's own
#     search tools); `smart-decide` is left in so it stays discoverable
#   - `fallback`: return the TF-IDF pool when nothing is judged relevant
# In-code defaults in `Exhub.MCP.Hub.ToolRelevance` apply for any missing key.
config :exhub, Exhub.MCP.Hub.ToolRelevance,
  enabled: true,
  candidate_limit: 30,
  max_concurrency: 20,
  threshold: 0.5,
  timeout: 30_000,
  state_char_limit: 18000,
  query_char_limit: 3200,
  exclude_servers: ["mcp-hub"],
  fallback: true

# Web tools: Smart Decide (System One) relevance filter applied to the web
# search result pages returned by `web_search`.
#   - `enabled`: master switch (per-call override via `web_search.filter`)
#   - `candidate_limit`: API pool judged when filtering (>= the tool's `count`)
#   - `max_concurrency`: concurrent System One requests (one result per request)
#   - `threshold`: minimum `noul` probability to keep a result. Measured
#     judgments are strongly bimodal (relevant >= 0.95, irrelevant <= 0.27),
#     so 0.7 sits inside that gap and discards the instruction's "unsure -> yes"
#     borderline band without dropping on-topic results.
#   - `state_char_limit`/`query_char_limit`: truncation to fit the ~8k-token
#     input budget (~3 chars/token for code, ~4.3 for prose)
#   - `fallback`: return the raw search results when nothing is judged relevant
# In-code defaults in `Exhub.MCP.WebTools.Relevance` apply for any missing key.
config :exhub, Exhub.MCP.WebTools.Relevance,
  enabled: true,
  candidate_limit: 20,
  max_concurrency: 20,
  threshold: 0.7,
  timeout: 30_000,
  state_char_limit: 18000,
  query_char_limit: 3200,
  fallback: true

# Desktop `search_files`: Smart Decide (System One) relevance filter applied to
# the probe semantic-search code blocks. The judged task is the call's
# `purpose` (falling back to `query`).
#   - `enabled`: master switch (per-call override via `search_files.filter`)
#   - `candidate_limit`: probe result pool judged when filtering (>= `max_results`)
#   - `max_concurrency`: concurrent System One requests (one code block per request)
#   - `threshold`: minimum `noul` probability to keep a code block
#   - `state_char_limit`: max characters of code sent as `state` when judging
#   - `max_judgeable_chars`: a candidate whose state exceeds this is kept unjudged
#     rather than judged on truncated code
#   - `query_char_limit`: max characters of the task embedded in the question
#   - `fallback`: return the ranked pool when nothing is judged relevant
# In-code defaults in `Exhub.MCP.Desktop.Search.Relevance` apply for any missing key.
config :exhub, Exhub.MCP.Desktop.Search.Relevance,
  enabled: true,
  candidate_limit: 20,
  max_concurrency: 20,
  threshold: 0.5,
  timeout: 30_000,
  state_char_limit: 18000,
  max_judgeable_chars: 24000,
  query_char_limit: 3200,
  fallback: true

# Desktop shell tools (`execute_command`, `start_process`): decide whether a
# command requires a `working_dir` before running it in the server's cwd.
# Commands that specify their own location (absolute/~ paths, or `cd`) are
# resolved locally; every other command is judged by a Smart Decide (System One)
# `noul` question, with the deterministic `Exhub.MCP.Desktop.Helpers` heuristic
# as the failure fallback (fail closed).
#   - `enabled`: master switch (falls back to the pure heuristic when false)
#   - `threshold`: minimum `noul` probability to require a `working_dir`
#   - `timeout`: per-request timeout in milliseconds
#   - `cache_ttl_ms`: how long a verdict is cached per command string
#   - `cache_limit`: clear the cache when it grows past this many entries
# In-code defaults in `Exhub.MCP.Desktop.WorkingDir` apply for any missing key.
config :exhub, Exhub.MCP.Desktop.WorkingDir,
  enabled: true,
  threshold: 0.5,
  timeout: 30_000,
  cache_ttl_ms: 60_000,
  cache_limit: 2000

import_config "#{config_env()}.exs"
