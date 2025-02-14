import Config


config :exhub,
  llm: %{
    api_base: "https://api.mistral.ai/v1",
    api_key: "api_key",
    model: "mistral/codestral-latest"
  }
