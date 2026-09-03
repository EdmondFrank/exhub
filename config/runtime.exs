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
    giteeai_token_api_key = fetch_secret.("giteeai_token_api_key")
    giteeai_request_api_key = fetch_secret.("giteeai_request_api_key")
    openai_api_key = fetch_secret.("openai_api_key")
    bai_api_key = fetch_secret.("bai_api_key")
    amd_api_key = fetch_secret.("amd_api_key")
    gitee_cookie = fetch_secret.("gitee_cookie")

    # Build llms config from Exhub.LLMModels (single source of truth)
    llms_config =
      Exhub.LLMModels.build_llms_config(%{
        giteeai_api_key: giteeai_api_key,
        samba_api_key: fetch_secret.("samba_api_key"),
        siliconflow_api_key: fetch_secret.("siliconflow_api_key"),
        codestral_api_key: fetch_secret.("codestral_api_key"),
        anthropic_api_key: fetch_secret.("anthropic_api_key"),
        mistral_api_key: fetch_secret.("mistral_api_key"),
        groq_api_key: fetch_secret.("groq_api_key"),
        gemini_api_key: fetch_secret.("gemini_api_key"),
        cohere_api_key: fetch_secret.("cohere_api_key")
      })

    config :exhub,
      giteeai_api_key: giteeai_api_key,
      # Token pool for Gitee AI billing modes: token-based vs request-based
      # pools, selected per request from the estimated context token count.
      giteeai_token_api_key: giteeai_token_api_key,
      giteeai_request_api_key: giteeai_request_api_key,
      giteeai_pool_threshold: 20_000,
      openai_api_key: openai_api_key,
      burncloud_api_key: fetch_secret.("burncloud_api_key"),
      burncloud_gemini_api_key: fetch_secret.("burncloud_gemini_api_key"),
      bailiancloud_api_key: fetch_secret.("bailiancloud_api_key"),
      minimax_api_key: fetch_secret.("minimax_api_key"),
      mimo_api_key: fetch_secret.("mimo_api_key"),
      kiro_api_key: fetch_secret.("kiro_api_key"),
      anthropic_api_key: fetch_secret.("anthropic_api_key"),
      baidu_anthropic_api_key: fetch_secret.("baidu_anthropic_api_key"),
      openrouter_api_key: fetch_secret.("openrouter_api_key"),
      runinfra_api_key: fetch_secret.("runinfra_api_key"),
      orcarouter_api_key: fetch_secret.("orcarouter_api_key"),
      bai_api_key: bai_api_key,
      amd_api_key: amd_api_key,
      llms: llms_config,
      # Optional dedicated model for the exhub-translate module (an `llms`
      # key, e.g. "codestral/codestral-latest"). Falls back to the default
      # LLM when unset. See config/config.exs -> :translate_llm.
      translate_llm:
        System.get_env("EXHUB_TRANSLATE_LLM") || Application.get_env(:exhub, :translate_llm),
      proxy: "http://127.0.0.1:7890",
      default_timeout: 300_000,
      mcp_idle_timeout: 300_000,
      mcp_keepalive_interval: 60_000,
      gitee_cat: %{
        endpoint: "https://api.gitee.com/",
        auth: %{cookie: gitee_cookie}
      },
      # Archery SQL audit platform configuration
      archery_url: fetch_secret.("archery_url"),
      archery_username: fetch_secret.("archery_username"),
      archery_password: fetch_secret.("archery_password")

  {:error, reason} ->
    # Secrets not yet configured (e.g. during `mix scr.create` / `mix scr.insert`).
    # Fall back to compile-time defaults from config.exs — no runtime config applied.
    IO.puts(
      "[runtime.exs] SecretVault not available (#{inspect(reason)}), " <>
        "skipping runtime secrets. Using compile-time defaults."
    )
end
