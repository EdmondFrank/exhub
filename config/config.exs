import Config


config :exhub,
  llms: %{
    "qwen2.5-72b-instruct" => %{
      api_base: "https://ai.gitee.com/v1",
      api_key: "your token",
      model: "openai/Qwen2.5-72B-Instruct",
    },
    "openai/Qwen/Qwen2.5-Coder-32B-Instruct" => %{
      api_base: "https://api.siliconflow.cn/v1",
      api_key: "your token",
      model: "openai/Qwen/Qwen2.5-Coder-32B-Instruct",
    },
    "openai/Qwen/Qwen2.5-32B-Instruct" => %{
      api_base: "https://api.siliconflow.cn/v1",
      api_key: "your token",
      model: "openai/Qwen/Qwen2.5-32B-Instruct",
    },
    "codestral/codestral-latest" => %{
      api_base: "https://codestral.mistral.ai/v1",
      api_key: "your token",
      model: "mistral/codestral-latest",
    },
    "mistral/mistral-large-latest" => %{
      api_base: "https://api.mistral.ai/v1",
      api_key: "your token",
      model: "mistral/mistral-large-latest",
    },
    "groq/llama-3.3-70b-versatile" => %{
      api_base: "http://127.0.0.1:9069/groq/v1",
      api_key: "your token",
      model: "openai/llama-3.3-70b-versatile",
    },
    "gemini/gemini-2.0-flash" => %{
      api_base: "http://127.0.0.1:9069/google/v1",
      api_key: "your token",
      model: "google/gemini-2.0-flash",
    },
    "command-r-plus" => %{
      api_base: "http://127.0.0.1:9069/cohere/v1",
      api_key: "your token",
      model: "openai/command-r-plus",
    }
  },
  proxy: "http://127.0.0.1:7890",
  gitee_cat: %{
    endpoint: "https://api.gitee.com/",
    auth: %{cookie: "your cookies"}
  }
