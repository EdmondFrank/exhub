import Config

config :elixir, :time_zone_database, Tzdata.TimeZoneDatabase

config :exhub, :shell, "zsh"

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
      "link_authority" => 0.1
    },
    "min_score" => 0.0
  }

config :exhub, :proxy_providers, ["openrouter"]

config :exhub, :secret_vault,
  default: [
    password: System.get_env("SECRET_VAULT_PASSWORD", "")
  ]

# secrets_dir: "priv/secrets"

import_config "#{config_env()}.exs"
