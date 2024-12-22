import Config


config :exhub,
  llm: %{
    model: "Qwen/Qwen2.5-7B-Instruct",
    api_base: "https://api.siliconflow.cn/v1"
  }

config :langchain,
  openai_key: "Your api key"
