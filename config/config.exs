import Config

# Store common values in variables instead of module attributes
gitee_api_base = "https://ai.gitee.com/v1"
gitee_api_key = "your token"

# Define LLM configurations using the DRY approach
llms_config = %{
  "openai/Qwen2.5-72B-Instruct" => %{
    api_base: gitee_api_base,
    api_key: gitee_api_key,
    model: "openai/Qwen2.5-72B-Instruct"
  },
  "openai/Qwen3-235B-A22B" => %{
    api_base: gitee_api_base,
    api_key: gitee_api_key,
    model: "openai/Qwen3-235B-A22B"
  },
  "openai/minimax-m2" => %{
    api_base: gitee_api_base,
    api_key: gitee_api_key,
    model: "openai/minimax-m2"
  },
  "openai/qwen3-next-80b-a3b-instruct" => %{
    api_base: gitee_api_base,
    api_key: gitee_api_key,
    model: "openai/qwen3-next-80b-a3b-instruct"
  },
  "openai/qwen3-235b-a22b-instruct-2507" => %{
    api_base: gitee_api_base,
    api_key: gitee_api_key,
    model: "openai/qwen3-235b-a22b-instruct-2507"
  },
  "openai/qwen3-coder-480b-a35b-instruct" => %{
    api_base: gitee_api_base,
    api_key: gitee_api_key,
    model: "openai/qwen3-coder-480b-a35b-instruct"
  },
  "openai/kimi-k2-instruct" => %{
    api_base: gitee_api_base,
    api_key: gitee_api_key,
    model: "openai/kimi-k2-instruct"
  },
  "openai/cursor/gpt-4o-mini" => %{
    api_base: "http://127.0.0.1:9069/openai/v1",
    api_key: "your token",
    model: "openai/cursor/gpt-4o-mini"
  },
  "openai/gpt-4o-mini" => %{
    api_base: "http://localhost:4444/v1",
    api_key: "edmondfrank",
    model: "openai/gpt-4o-mini"
  },
  "openai/deepseek-v3_1-terminus" => %{
    api_base: gitee_api_base,
    api_key: gitee_api_key,
    model: "openai/deepseek-v3_1-terminus"
  },
  "openai/deepseek-v3.2-exp" => %{
    api_base: gitee_api_base,
    api_key: gitee_api_key,
    model: "openai/deepseek-v3.2-exp"
  },
  "openai/glm-4.6" => %{
    api_base: gitee_api_base,
    api_key: gitee_api_key,
    model: "openai/glm-4.6"
  },
  "openai/DeepSeek-V3" => %{
    api_base: gitee_api_base,
    api_key: gitee_api_key,
    model: "openai/DeepSeek-V3"
  },
  "openai/QwQ-32B" => %{
    api_base: "http://localhost:9069/samba/v1",
    api_key: "your token",
    model: "openai/QwQ-32B"
  },
  "openai/gemini-2.5-pro" => %{
    api_base: "http://127.0.0.1:9069/openai/v1",
    api_key: "your token",
    model: "openai/gemini-2.5-pro"
  },
  "openai/Qwen/Qwen2.5-Coder-32B-Instruct" => %{
    api_base: "https://api.siliconflow.cn/v1",
    api_key: "your token",
    model: "openai/Qwen/Qwen2.5-Coder-32B-Instruct"
  },
  "openai/Qwen/Qwen2.5-32B-Instruct" => %{
    api_base: "https://api.siliconflow.cn/v1",
    api_key: "your token",
    model: "openai/Qwen/Qwen2.5-32B-Instruct"
  },
  "codestral/codestral-latest" => %{
    api_base: "https://codestral.mistral.ai/v1",
    api_key: "your token",
    model: "mistral/codestral-latest"
  },
  "anthropic/claude-3-5-sonnet-latest" => %{
    api_base: "http://127.0.0.1:9069/anthropic/v1",
    api_key: "your token",
    model: "anthropic/claude-3-5-sonnet-latest"
  },
  "mistral/mistral-small-latest" => %{
    api_base: "https://api.mistral.ai/v1",
    api_key: "your token",
    model: "mistral/mistral-small-latest"
  },
  "mistral/mistral-large-latest" => %{
    api_base: "https://api.mistral.ai/v1",
    api_key: "your token",
    model: "mistral/mistral-large-latest"
  },
  "groq/llama-3.3-70b-versatile" => %{
    api_base: "http://127.0.0.1:9069/groq/v1",
    api_key: "your token",
    model: "openai/llama-3.3-70b-versatile"
  },
  "gemini/gemini-2.0-flash" => %{
    api_base: "http://127.0.0.1:9069/google/v1",
    api_key: "your token",
    model: "google/gemini-2.0-flash"
  },
  "command-r-plus" => %{
    api_base: "http://127.0.0.1:9069/cohere/v1",
    api_key: "your token",
    model: "openai/command-r-plus"
  },
  "command-a-03-2025" => %{
    api_base: "http://127.0.0.1:9069/cohere/v1",
    api_key: "your token",
    model: "openai/command-a-03-2025"
  }
}

config :exhub,
  gemini_api_base: "http://localhost:8765/v1",
  giteeai_api_key: gitee_api_key,
  openai_api_key: "your token",
  llms: llms_config,
  proxy: "http://127.0.0.1:7890",
  gitee_cat: %{
    endpoint: "https://api.gitee.com/",
    auth: %{cookie: "your cookies"}
  }
