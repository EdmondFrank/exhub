import Config


config :exhub,
  llm: %{
    api_base: "https://api.siliconflow.cn/v1"
    model: "Qwen/Qwen2.5-Coder-32B-Instruct",
  }

config :langchain,
  openai_key: "Your api key"
