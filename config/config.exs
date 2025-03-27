import Config


config :exhub,
  llms: %{
    "openai/Qwen2.5-72B-Instruct" => %{
      api_base: "https://ai.gitee.com/v1",
      api_key: "your token",
      model: "openai/Qwen2.5-72B-Instruct",
    },
    "openai/DeepSeek-V3" => %{
      api_base: "https://ai.gitee.com/v1",
      api_key: "your token",
      model: "openai/DeepSeek-V3",
    },
    "openai/QwQ-32B" => %{
      api_base: "http://localhost:9069/samba/v1",
      api_key: "your token",
      model: "openai/QwQ-32B"
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
    "anthropic/claude-3-5-sonnet-latest" => %{
      api_base: "http://127.0.0.1:9069/anthropic/v1",
      api_key: "your token",
      model: "anthropic/claude-3-5-sonnet-latest",
    },
    "mistral/mistral-small-latest" => %{
      api_base: "https://api.mistral.ai/v1",
      api_key: "your token",
      model: "mistral/mistral-small-latest",
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
