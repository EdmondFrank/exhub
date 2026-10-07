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

# Code mode (`code_mode` MCP tool on the hub): evaluate a Lua 5.3 snippet that
# calls visible hub tools as functions, in a resource-bounded sandbox. See
# `Exhub.MCP.Hub.CodeMode`. Set `enabled: false` to turn the tool into an error.
config :exhub, :code_mode,
  enabled: true,
  # Match the Hub server's `request_timeout` (600s) so the sandbox times out
  # gracefully before the transport hard-kills the request.
  timeout_ms: 600_000,
  max_instructions: 5_000_000,
  max_call_depth: 200,
  max_heap_size: 268_435_456,
  max_string_bytes: 8_388_608,
  # When a result exceeds `max_output_chars`, the full output is written to a
  # temp file and its path returned alongside the truncated prefix, so the
  # caller can read it back later. Set `spill_truncated: false` to disable, or
  # `spill_dir` to choose the directory (nil → `System.tmp_dir!()`).
  max_output_chars: 24_000,
  spill_truncated: true,
  spill_dir: nil,
  max_concurrency: 8,
  raise_on_tool_error: true,
  exclude_servers: ["mcp-hub"]

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

# Memory layer — a Beacon-style, review-gated memory loop built on the Brain
# vault (notes under `vault_folder`) and Smart Decide (System One).
#   - `vault_folder`/`skill_folder`: where memories and promoted skills live,
#     relative to the Obsidian vault.
#   - `kinds`/`statuses`: allowed lifecycle values.
#   - `evaluator`: System One question gate (never approves anything itself).
#   - `recall`: default recall limit and Smart Decide relevance pass.
config :exhub, :memory,
  vault_folder: "memory",
  skill_folder: "memory/skills",
  kinds: ~w(workflow correction debugging_pattern gotcha convention),
  statuses: ~w(candidate approved rejected superseded),
  evaluator: [
    enabled: true,
    model: "Intern-Decision-4B",
    task_success_min: 0.50,
    mean_min: 0.60,
    state_char_limit: 16_000
  ],
  recall: [
    limit: 5,
    filter: true
  ]

# Toonflow — native AI short-drama pipeline (novel → script → storyboard →
# image → video → export). Projects live under `root_dir` (default
# ~/.config/exhub/toonflow). See docs/modules/toonflow.md and
# docs/plans/2026-09-30-toonflow-design.md.
config :exhub, :toonflow, %{
  "root_dir" => nil,
  "agents" => %{
    "script" => "deepseek-v4.1-flash",
    "director" => "deepseek-v4.1-flash",
    "qa" => "deepseek-v4.1-flash"
  },
  "media" => %{
    "image_model" => "qwen-image-2.0",
    "video_model" => "MiniMax-H3",
    "tts_model" => "CosyVoice2",
    "tts_voice" => "alloy"
  },
  "memory" => %{
    "enabled" => true,
    "index_path" => nil,
    "embedding_model" => "text-embedding-3-small",
    "dim" => 1536
  },
  "assembly" => %{
    "ffmpeg_path" => "ffmpeg",
    "subtitles" => true
  },
  "ui" => %{
    "enabled" => true,
    "tick_ms" => 5000,
    # Mutating REST endpoints (create project / run pipeline) are loopback-only
    # by default — the app is also reachable over the VPN.
    "require_local" => true
  }
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
#   - `state_char_limit`/`query_char_limit`: truncation to fit the model's
#     input budget (~3 chars/token for code, ~4.3 for prose)
#   - `model`: System One model id (default Intern-Decision-4B; e.g.
#     laya-multilingual, APUS-OpenJev-v1-9B)
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
#   - `state_char_limit`/`query_char_limit`: truncation to fit the model's
#     input budget (~3 chars/token for code, ~4.3 for prose)
#   - `exclude_servers`: servers never offered as candidates (the hub's own
#     search tools); `smart-decide` is left in so it stays discoverable
#   - `model`: System One model id (default Intern-Decision-4B; e.g.
#     laya-multilingual, APUS-OpenJev-v1-9B)
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

# Browser Agent — Jev-style loop over kuri (observation/interaction) and
# Smart Decide (operation + target policy).
#   - `backend`: `:http` drives the ExHub-managed kuri daemon, whose HTTP API
#     keeps accessibility refs server-side so CLICK/TYPE_TEXT resolve; `:cli`
#     shells out to `kuri-agent`, which assigns refs per process and therefore
#     cannot act on a ref printed by an earlier snapshot.
#   - `endpoint`/`model`: the OpenAI-compatible chat model used to write
#     TYPE_TEXT field values; uses the shared :giteeai_api_key
# In-code defaults in `Exhub.BrowserAgent.TextHelper` apply for any missing key.
config :exhub, Exhub.BrowserAgent, backend: :http

config :exhub, Exhub.BrowserAgent.TextHelper,
  endpoint: "https://api.moark.com/v1/chat/completions",
  model: "deepseek-v4.1-flash"

# Web tools: Smart Decide (System One) relevance filter applied to the web
# search result pages returned by `web_search`.
#   - `enabled`: master switch (per-call override via `web_search.filter`)
#   - `candidate_limit`: API pool judged when filtering (>= the tool's `count`)
#   - `max_concurrency`: concurrent System One requests (one result per request)
#   - `threshold`: minimum `noul` probability to keep a result. Measured
#     judgments are strongly bimodal (relevant >= 0.95, irrelevant <= 0.27),
#     so 0.7 sits inside that gap and discards the instruction's "unsure -> yes"
#     borderline band without dropping on-topic results.
#   - `state_char_limit`/`query_char_limit`: truncation to fit the model's
#     input budget (~3 chars/token for code, ~4.3 for prose)
#   - `model`: System One model id (default Intern-Decision-4B; e.g. APUS-OpenJev-v1-9B for 128K)
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
#   - `model`: System One model id (default Intern-Decision-4B; e.g. APUS-OpenJev-v1-9B for 128K)
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

# Network counterpart of the working-dir gate: after a failed outbound command,
# `Exhub.MCP.Desktop.ProxyEnv` decides whether to retry it with HTTP(S) proxy
# variables in the child's environment only (never persisted).
#   - `enabled`: master switch (off ⇒ the tool never retries or touches env)
#   - `mode`: `:on_fail` (default; judge only after a proxy-shaped failure) or
#     `:pre` (also pre-inject for `start_process`, which cannot observe output)
#   - `proxy_url`: the candidate proxy; when unset, an exported HTTPS_PROXY is
#     used, then `fallback_proxies` (a live local proxy such as Clash)
#   - `fallback_proxies`: loopback candidates, each TCP-probed before use
#   - `no_proxy`: extra always-direct domains, merged with existing NO_PROXY
#   - `min_confidence`: below this `needs_proxy` probability the model is
#     treated as abstaining and nothing is injected
#   - `max_leak_risk`: `leak_risk` score (0–3) above which injection is refused
#   - `probe_timeout_ms`: TCP reachability probe budget per candidate
# In-code defaults in `Exhub.MCP.Desktop.ProxyEnv` apply for any missing key.
config :exhub, Exhub.MCP.Desktop.ProxyEnv,
  enabled: true,
  mode: :on_fail,
  proxy_url: nil,
  # Mechanical bypass only: a proxy env with no loopback exemption breaks local
  # sockets (Docker, dev servers, health checks). Domain-specific bypass is NOT
  # configured here — the operator's proxy inventory is passed to the model as
  # advisory notes below, because a `no_proxy` list is reference information,
  # not a policy the judge has to obey.
  no_proxy: [],
  # Default premise in every question: mainland-China egress, overseas blocked
  # directly, a local proxy is the intended path. Measured: without it a genuine
  # `curl https://www.google.com` timeout scored needs_proxy 0.033; with it, 0.992.
  # The sentence lives in `@mainland_premise` (proxy_env.ex) so it stays the
  # default; set `network_premise:` here to reword it, or to nil on a host with
  # unrestricted egress.
  # network_premise: nil,
  # knowledge-base/ci-proxy-usage-guide.md + preferences/network-proxy.md, as
  # facts the model may weigh (HTTPS_PROXY/HTTP_PROXY are set to the chosen
  # candidate anyway).
  network_notes:
    "Clash listens on 127.0.0.1:7890 (personal overseas egress); the CI box has an " <>
      "HTTPS CONNECT proxy at hj.runjs.cn:31443 (port 31443, not 443). Historically a " <>
      "proxy broke TLS to *.runjs.cn and api.moark.com/ai.gitee.com were reachable " <>
      "directly, so those were previously bypassed — treat that as history, judge from " <>
      "the measured probe.",
  target_probe: true,
  target_probe_timeout_ms: 1_500,
  min_confidence: 0.6,
  max_leak_risk: 2.0,
  timeout: 30_000,
  probe_timeout_ms: 50,
  cache_ttl_ms: 600_000,
  cache_limit: 2000

import_config "#{config_env()}.exs"
