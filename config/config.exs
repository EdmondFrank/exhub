import Config

config :elixir, :time_zone_database, Tzdata.TimeZoneDatabase

config :exhub, :shell, "zsh"

# Obsidian vault path for the Brain MCP server.
# Override in runtime.exs or environment-specific config.
config :exhub, :obsidian_vault_path, "~/GTD/PKB"

config :exhub, :secret_vault,
  default: [
    password: System.get_env("SECRET_VAULT_PASSWORD", "")
  ]
  # secrets_dir: "priv/secrets"
