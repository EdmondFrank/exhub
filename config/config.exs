import Config

config :exhub, :secret_vault,
  default: [
    password: System.get_env("SECRET_VAULT_PASSWORD", "")
  ]
  # secrets_dir: "priv/secrets"
