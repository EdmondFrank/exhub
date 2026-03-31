import Config

# Only load secrets at runtime if SecretVault is configured and secrets exist.
# This allows `mix scr.create` and `mix scr.insert` to run before any secrets
# have been created, falling back to compile-time defaults from config.exs.
case SecretVault.Config.fetch_from_current_env(:exhub) do
  {:ok, vault_config} ->
    fetch_secret = fn name ->
      case SecretVault.fetch(vault_config, name) do
        {:ok, value} -> value |> String.trim()
        _ -> "your token"
      end
    end

    giteeai_api_key = fetch_secret.("gitee_api_key")
    openai_api_key = fetch_secret.("openai_api_key")
    gitee_cookie = fetch_secret.("gitee_cookie")

    gitee_api_base = "https://ai.gitee.com/v1"

    # Helper to build a gitee LLM config entry
    gitee_llm = fn model ->
      %{
        api_base: gitee_api_base,
        api_key: giteeai_api_key,
        model: model
      }
    end

    llms_config = %{
      "openai/Qwen2.5-72B-Instruct" => gitee_llm.("openai/Qwen2.5-72B-Instruct"),
      "openai/Qwen3-235B-A22B" => gitee_llm.("openai/Qwen3-235B-A22B"),
      "openai/minimax-m2" => gitee_llm.("openai/minimax-m2"),
      "openai/qwen3-coder-next" => gitee_llm.("openai/qwen3-coder-next"),
      "openai/qwen3-next-80b-a3b-instruct" => gitee_llm.("openai/qwen3-next-80b-a3b-instruct"),
      "openai/qwen3-next-80b-a3b-thinking" => gitee_llm.("openai/qwen3-next-80b-a3b-thinking"),
      "openai/qwen3-235b-a22b-instruct-2507" => gitee_llm.("openai/qwen3-235b-a22b-instruct-2507"),
      "openai/qwen3-coder-480b-a35b-instruct" => gitee_llm.("openai/qwen3-coder-480b-a35b-instruct"),
      "openai/kimi-k2.5" => gitee_llm.("openai/kimi-k2.5"),
      "openai/kimi-k2-instruct" => gitee_llm.("openai/kimi-k2-instruct"),
      "openai/deepseek-v3_1-terminus" => gitee_llm.("openai/deepseek-v3_1-terminus"),
      "openai/deepseek-v3.2" => gitee_llm.("openai/deepseek-v3.2"),
      "openai/deepseek-v3.2-exp" => gitee_llm.("openai/deepseek-v3.2-exp"),
      "openai/glm-4.6" => gitee_llm.("openai/glm-4.6"),
      "openai/DeepSeek-V3" => gitee_llm.("openai/DeepSeek-V3"),
      "openai/QwQ-32B" => %{
        api_base: "http://localhost:9069/samba/v1",
        api_key: fetch_secret.("samba_api_key"),
        model: "openai/QwQ-32B"
      },
      "openai/Qwen/Qwen2.5-Coder-32B-Instruct" => %{
        api_base: "https://api.siliconflow.cn/v1",
        api_key: fetch_secret.("siliconflow_api_key"),
        model: "openai/Qwen/Qwen2.5-Coder-32B-Instruct"
      },
      "openai/Qwen/Qwen2.5-32B-Instruct" => %{
        api_base: "https://api.siliconflow.cn/v1",
        api_key: fetch_secret.("siliconflow_api_key"),
        model: "openai/Qwen/Qwen2.5-32B-Instruct"
      },
      "codestral/codestral-latest" => %{
        api_base: "https://codestral.mistral.ai/v1",
        api_key: fetch_secret.("codestral_api_key"),
        model: "mistral/codestral-latest"
      },
      "anthropic/claude-3-5-sonnet-latest" => %{
        api_base: "http://127.0.0.1:9069/anthropic/v1",
        api_key: fetch_secret.("anthropic_api_key"),
        model: "anthropic/claude-3-5-sonnet-latest"
      },
      "mistral/mistral-small-latest" => %{
        api_base: "https://api.mistral.ai/v1",
        api_key: fetch_secret.("mistral_api_key"),
        model: "mistral/mistral-small-latest"
      },
      "mistral/mistral-large-latest" => %{
        api_base: "https://api.mistral.ai/v1",
        api_key: fetch_secret.("mistral_api_key"),
        model: "mistral/mistral-large-latest"
      },
      "groq/llama-3.3-70b-versatile" => %{
        api_base: "http://127.0.0.1:9069/groq/v1",
        api_key: fetch_secret.("groq_api_key"),
        model: "openai/llama-3.3-70b-versatile"
      },
      "openai/gemini-2.5-pro" => %{
        api_base: "http://127.0.0.1:9069/google/v1",
        api_key: fetch_secret.("gemini_api_key"),
        model: "openai/gemini-2.5-pro"
      },
      "gemini/gemini-2.0-flash" => %{
        api_base: "http://127.0.0.1:9069/google/v1",
        api_key: fetch_secret.("gemini_api_key"),
        model: "google/gemini-2.0-flash"
      },
      "command-r-plus" => %{
        api_base: "http://127.0.0.1:9069/cohere/v1",
        api_key: fetch_secret.("cohere_api_key"),
        model: "openai/command-r-plus"
      },
      "command-a-03-2025" => %{
        api_base: "http://127.0.0.1:9069/cohere/v1",
        api_key: fetch_secret.("cohere_api_key"),
        model: "openai/command-a-03-2025"
      }
    }

    config :exhub,
      giteeai_api_key: giteeai_api_key,
      openai_api_key: openai_api_key,
      burncloud_api_key: fetch_secret.("burncloud_api_key"),
      minimax_api_key: fetch_secret.("minimax_api_key"),
      anthropic_api_key: fetch_secret.("anthropic_api_key"),
      openrouter_api_key: fetch_secret.("openrouter_api_key"),
      llms: llms_config,
      proxy: "http://127.0.0.1:7890",
      default_timeout: 300_000,
      mcp_idle_timeout: 300_000,
      mcp_keepalive_interval: 60_000,
      gitee_cat: %{
        endpoint: "https://api.gitee.com/",
        auth: %{cookie: gitee_cookie}
      }

  {:error, reason} ->
    # Secrets not yet configured (e.g. during `mix scr.create` / `mix scr.insert`).
    # Fall back to compile-time defaults from config.exs — no runtime config applied.
    IO.puts(
      "[runtime.exs] SecretVault not available (#{inspect(reason)}), " <>
        "skipping runtime secrets. Using compile-time defaults."
    )
end
