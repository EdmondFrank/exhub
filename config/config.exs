import Config

config :elixir, :time_zone_database, Tzdata.TimeZoneDatabase

config :exhub, :shell, "zsh"

config :exhub, :secret_vault,
  default: [
    password: System.get_env("SECRET_VAULT_PASSWORD", "")
  ]
  # secrets_dir: "priv/secrets"
