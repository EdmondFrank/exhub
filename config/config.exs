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

# Obsidian vault path for the Brain MCP server.
# Override in runtime.exs or environment-specific config.
config :exhub, :obsidian_vault_path, "~/GTD/PKB"

# Brain vault search ranking defaults. Tunable per-call via brain_search_vault
# `fusion`/`weights`/`min_score` params, which are merged over these defaults.
config :exhub, :brain_ranking,
  %{
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
config :exhub, :brain_search,
  %{
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
config :exhub, :brain_rag,
  %{
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

import_config "#{config_env()}.exs"
