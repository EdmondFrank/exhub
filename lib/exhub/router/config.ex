defmodule Exhub.Router.Config do
  @moduledoc """
  Centralized configuration for model routing and API mappings.

  This module provides compile-time and runtime configuration for:
  - Model to provider URL mappings
  - Model to API key mappings
  - Provider-specific settings (proxy usage, headers, etc.)
  """

  require Logger

  @typedoc "Model identifier string"
  @type model :: String.t()

  @typedoc "Provider URL string"
  @type provider_url :: String.t()

  @typedoc "API key string"
  @type api_key :: String.t()

  @typedoc "Proxy configuration"
  @type proxy_config :: String.t() | false

  # Default upstream URL when no specific mapping exists
  @default_upstream "https://pinova.ai/v1"

  # Provider base URLs
  @provider_urls %{
    giteeai: "https://api.moark.com/v1",
    kimi: "https://api.kimi.com/coding/v1",
    minimaxi: "https://api.minimaxi.com/v1",
    mimo: "https://token-plan-sgp.xiaomimimo.com/v1",
    openrouter: "https://openrouter.ai/api/v1",
    local: "http://localhost:8765/v1",
    openai: @default_upstream,
    infini: "https://cloud.infini-ai.com/maas/v1",
    kiro: "http://localhost:8000/v1",
    nvidia: "https://integrate.api.nvidia.com/v1",
    baidu_anthropic: "http://211.23.3.236:27545/v1"
  }

  # Model to provider mappings
  # Most GiteeAI models share the same endpoint
  @giteeai_models [
    "step3",
    "glm-4_5",
    "glm-4_5v",
    "glm-4.6",
    "glm-4.7",
    "glm-5",
    "glm-5.1",
    "glm-5.2",
    "glm-5-turbo",
    "hy-mt2-30b-a3b",
    "deepseek-v4-flash",
    "deepseek-v4-pro",
    "deepseek-v3",
    "deepseek-r1",
    "deepseek-v3_1",
    "deepseek-v3_1-terminus",
    "deepseek-v3.2",
    "deepseek-v3.2-exp",
    "gpt-oss-120b",
    "internvl3-78b",
    "kimi-k2.5",
    "kimi-k2.6",
    "kimi-k2.7-code",
    "kimi-k2-instruct",
    "kimi-k2-thinking",
    "qwen3.6-max",
    "qwen3.6-plus",
    "qwen3.5-9b",
    "qwen3.5-27b-pro",
    "qwen3.5-27b",
    "qwen3.5-35b-a3b",
    "qwen3.5-122b-a10b",
    "qwen3-235b-a22b",
    "qwen3-235b-a22b-instruct-2507",
    "qwen3-next-80b-a3b-instruct",
    "qwen3-next-80b-a3b-thinking",
    "qwen3-coder-next",
    "qwen3-coder-30b-a3b-instruct",
    "qwen3-coder-480b-a35b-instruct",
    "qwen3-30b-a3b-instruct-2507",
    "qwen3.5-27b-claude-4.6-opus-reasoning-distilled",
    "minimax-m2",
    "minimax-m2.1",
    "minimax-m2.5",
    "minimax-m2.7",
    "minimax-m2-preview",
    "mimo-v2.5-pro",
    "mimo-v2.5"
  ]

  @minimax_models ["minimax-m2.7", "minimax-m2-preview"]

  # MiMo AI models (backup — currently routed via Gitee AI, see @giteeai_models)
  @mimo_models ["mimo-v2.5-pro", "mimo-v2.5"]

  # Infini AI models (with inf- prefix for distinction)
  @infini_models [
    "inf-glm-5.1",
    "inf-glm-5.2",
    "inf-kimi-k2.5",
    "inf-kimi-k2.7-code",
    "inf-minimax-m2.7",
    "inf-deepseek-v3.2"
  ]

  # OpenRouter models
  @openrouter_models [
    "tngtech/deepseek-r1t2-chimera:free",
    "minimax/minimax-m2:free",
    "openrouter/polaris-alpha",
    "nvidia/nemotron-3-ultra-550b-a55b:free"
  ]

  # Kiro Gateway models (local Claude proxy)
  @kiro_models [
    "auto-kiro",
    "claude-3.7-sonnet",
    "claude-haiku-4.5",
    "claude-opus-4.5",
    "claude-sonnet-4",
    "claude-sonnet-4.5"
  ]

  # NVIDIA API models
  @nvidia_models [
    "nvidia/nemotron-3-ultra-550b-a55b"
  ]

  # Mapping from prefixed model names to actual API model names
  @infini_model_mapping %{
    "inf-glm-5.1" => "glm-5.1",
    "inf-glm-5.2" => "glm-5.2",
    "inf-kimi-k2.5" => "kimi-k2.5",
    "inf-kimi-k2.7-code" => "kimi-k2.7-code",
    "inf-minimax-m2.7" => "minimax-m2.7",
    "inf-deepseek-v3.2" => "deepseek-v3.2"
  }

  @doc """
  Returns the target URL for a given model.

  ## Examples

      iex> Exhub.Router.Config.get_model_target("deepseek-v3")
      "https://api.moark.com/v1"

      iex> Exhub.Router.Config.get_model_target("unknown-model")
      "https://pinova.ai/v1"
  """
  @spec get_model_target(model() | nil) :: provider_url()
  def get_model_target(nil), do: @default_upstream

  def get_model_target(model) when is_binary(model) do
    cond do
      model in @giteeai_models and model not in @minimax_models ->
        @provider_urls.giteeai

      model == "kimi-for-coding" ->
        @provider_urls.kimi

      model in @minimax_models ->
        @provider_urls.minimaxi

      model in @mimo_models ->
        @provider_urls.mimo

      model in @openrouter_models ->
        @provider_urls.openrouter

      model in ["gemini-2.5-pro", "gemini-2.5-flash", "gemini-3.5-flash"] ->
        get_burncloud_target()

      model in @infini_models ->
        @provider_urls.infini

      model in @kiro_models ->
        @provider_urls.kiro

      model in @nvidia_models ->
        @provider_urls.nvidia

      true ->
        Logger.debug("No specific target for model #{model}, using default")
        @default_upstream
    end
  end

  @doc """
  Returns the API key for a given model.

  ## Examples

      iex> Exhub.Router.Config.get_model_api_key("deepseek-v3")
      # Returns :exhub giteeai_api_key config value

      iex> Exhub.Router.Config.get_model_api_key("unknown-model")
      # Returns :exhub openai_api_key config value
  """
  @spec get_model_api_key(model() | nil) :: api_key()
  def get_model_api_key(nil) do
    Application.get_env(:exhub, :openai_api_key, "")
  end

  def get_model_api_key(model) when is_binary(model) do
    cond do
      model in @giteeai_models and model not in @minimax_models ->
        Application.get_env(:exhub, :giteeai_api_key, "")

      model == "kimi-for-coding" ->
        Application.get_env(:exhub, :kimi_api_key, "")

      model in @minimax_models ->
        Application.get_env(:exhub, :minimax_api_key, "")

      model in @mimo_models ->
        Application.get_env(:exhub, :mimo_api_key, "")

      model in @openrouter_models ->
        Application.get_env(:exhub, :openrouter_api_key, "")

      model in ["gemini-2.5-pro", "gemini-2.5-flash", "gemini-3.5-flash"] ->
        Application.get_env(:exhub, :burncloud_gemini_api_key, "")

      model in @infini_models ->
        Application.get_env(:exhub, :infini_api_key, "")

      model in @kiro_models ->
        Application.get_env(:exhub, :kiro_api_key, "")

      model in @nvidia_models ->
        Application.get_env(:exhub, :nvidia_api_key, "")

      true ->
        Application.get_env(:exhub, :openai_api_key, "")
    end
  end

  @doc """
  Returns whether proxy should be used for a given model.
  Used primarily for Anthropic API routing.

  ## Examples

      iex> Exhub.Router.Config.use_proxy_for_model?("minimax-m2.1")
      false
  """
  @spec use_proxy_for_model?(model()) :: boolean()
  def use_proxy_for_model?(model) when is_binary(model) do
    model in @openrouter_models or
      model in @nvidia_models or
      model in ["minimax-m2.1", "minimax-m2-preview"]
  end

  @doc """
  Returns the authorization header for a given model and provider type.
  Includes model-specific custom headers (e.g., X-Package for kimi-k2.5).

  ## Examples

      iex> Exhub.Router.Config.get_auth_headers("deepseek-v3", :openai)
      [{"authorization", "Bearer <giteeai_api_key>"}]

      iex> Exhub.Router.Config.get_auth_headers("minimax-m2.1", :anthropic)
      [{"x-api-key", "<minimax_api_key>"}]
  """
  @spec get_auth_headers(model(), :openai | :anthropic) :: [{String.t(), String.t()}]
  def get_auth_headers(model, :openai) do
    token = get_model_api_key(model)
    [{"authorization", "Bearer #{token}"}]
  end

  def get_auth_headers(model, :anthropic) do
    token = get_model_api_key(model)
    [{"x-api-key", token}]
  end

  @doc """
  Returns the target URL for Anthropic API requests.

  ## Examples

      iex> Exhub.Router.Config.get_anthropic_target("minimax-m2.1")
      "https://api.minimaxi.com/anthropic/v1"
  """
  @spec get_anthropic_target(model() | nil) :: provider_url()
  def get_anthropic_target(nil), do: @default_upstream

  def get_anthropic_target(model) when is_binary(model) do
    case model do
      m when m in ["minimax-m2.1", "minimax-m2-preview"] ->
        "https://api.minimaxi.com/anthropic/v1"

      _ ->
        @default_upstream
    end
  end

  @doc """
  Returns the target URL for BurnCloud API requests.
  """
  @spec get_burncloud_target() :: provider_url()
  def get_burncloud_target do
    Application.get_env(:exhub, :burncloud_endpoint, "https://csp.burncloud.com/v1")
  end

  @doc """
  Returns the target URL for BailianCloud (Aliyun MaaS) API requests.
  """
  @spec get_bailiancloud_target() :: provider_url()
  def get_bailiancloud_target do
    Application.get_env(:exhub, :bailiancloud_endpoint,
      "https://token-plan.cn-beijing.maas.aliyuncs.com/compatible-mode/v1"
    )
  end

  @doc """
  Returns the target URL for Baidu Anthropic (Anthropic-compatible) API requests.
  """
  @spec get_baidu_anthropic_target() :: provider_url()
  def get_baidu_anthropic_target do
    Application.get_env(:exhub, :baidu_anthropic_endpoint,
      "http://211.23.3.236:27545/v1"
    )
  end

  @doc """
  Returns the default upstream URL.
  """
  @spec default_upstream() :: provider_url()
  def default_upstream, do: @default_upstream

  @doc """
  Returns all configured provider base URLs.
  """
  @spec provider_urls() :: %{atom() => provider_url()}
  def provider_urls, do: @provider_urls

  @doc """
  Normalizes a model name by stripping provider prefixes.
  For Infini models (inf-*), returns the actual model name used by the API.

  ## Examples

      iex> Exhub.Router.Config.normalize_model_name("inf-deepseek-v3.2")
      "deepseek-v3.2"

      iex> Exhub.Router.Config.normalize_model_name("deepseek-v3")
      "deepseek-v3"
  """
  @spec normalize_model_name(model()) :: model()
  def normalize_model_name(model) when is_binary(model) do
    case Map.get(@infini_model_mapping, model) do
      nil -> model
      actual_name -> actual_name
    end
  end

  @doc """
  Returns the proxy configuration from application environment.
  """
  @spec get_proxy() :: String.t()
  def get_proxy do
    Application.get_env(:exhub, :proxy, "")
  end

  @doc """
  Returns the default timeout for connections.
  """
  @spec get_timeout() :: pos_integer()
  def get_timeout do
    Application.get_env(:exhub, :default_timeout, 1_800_000)
  end

  # Models that require reasoning_content to be present in assistant tool-call
  # messages when thinking is enabled (Moonshot AI / Xiaomi MiMo requirement).
  @kimi_reasoning_models ["kimi-k2.5", "kimi-k2.6", "kimi-k2.7-code", "inf-kimi-k2.5", "inf-kimi-k2.7-code", "mimo-v2.5-pro", "mimo-v2.5"]

  @doc """
  Transforms request body for model-specific requirements.
  For kimi-k2.5/kimi-k2.6/inf-kimi-k2.5/mimo-v2.5-pro/mimo-v2.5, injects a placeholder `reasoning_content`
  into assistant messages that have tool_calls but are missing the field.
  This prevents the Moonshot API error:
  "thinking is enabled but reasoning_content is missing in assistant tool call message"

  ## Examples

      iex> Exhub.Router.Config.transform_request_body(%{"messages" => [%{"role" => "assistant", "tool_calls" => [%{}]}]}, "kimi-k2.5")
      %{"messages" => [%{"role" => "assistant", "tool_calls" => [%{}], "reasoning_content" => "."}]}

      iex> Exhub.Router.Config.transform_request_body(%{"messages" => [%{"role" => "assistant", "tool_calls" => [%{}]}]}, "kimi-k2.6")
      %{"messages" => [%{"role" => "assistant", "tool_calls" => [%{}], "reasoning_content" => "."}]}

      iex> Exhub.Router.Config.transform_request_body(%{"model" => "test"}, "deepseek-v3")
      %{"model" => "test"}
  """
  @spec transform_request_body(map(), model()) :: map()
  def transform_request_body(body, model) when is_map(body) and is_binary(model) do
    if model in @kimi_reasoning_models do
      transform_kimi_reasoning_body(body)
    else
      body
    end
  end

  @doc """
  Reload all API keys from SecretVault at runtime.

  Re-reads secrets from the configured SecretVault and updates the
  application environment. This allows key rotation without restarting
  the Exhub application.

  ## Returns
  - `:ok` on success
  - `{:error, reason}` if SecretVault is not available

  ## Examples

      iex> Exhub.Router.Config.reload_from_scr()
      :ok
  """
  @spec reload_from_scr() :: :ok | {:error, term()}
  def reload_from_scr do
    case SecretVault.Config.fetch_from_current_env(:exhub) do
      {:ok, vault_config} ->
        update_from_vault(vault_config)
        Logger.info("[Router.Config] Reloaded API keys from SecretVault")
        :ok

      {:error, reason} ->
        Logger.error("[Router.Config] Failed to access SecretVault: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp update_from_vault(vault_config) do
    fetch_secret = fn name ->
      case SecretVault.fetch(vault_config, name) do
        {:ok, value} -> String.trim(value)
        _ -> ""
      end
    end

    # Update all API keys in application environment
    Application.put_env(:exhub, :giteeai_api_key, fetch_secret.("gitee_api_key"))
    Application.put_env(:exhub, :openai_api_key, fetch_secret.("openai_api_key"))
    Application.put_env(:exhub, :burncloud_api_key, fetch_secret.("burncloud_api_key"))
    Application.put_env(:exhub, :burncloud_gemini_api_key, fetch_secret.("burncloud_gemini_api_key"))
    Application.put_env(:exhub, :bailiancloud_api_key, fetch_secret.("bailiancloud_api_key"))
    Application.put_env(:exhub, :infini_api_key, fetch_secret.("infini_api_key"))
    Application.put_env(:exhub, :minimax_api_key, fetch_secret.("minimax_api_key"))
    Application.put_env(:exhub, :mimo_api_key, fetch_secret.("mimo_api_key"))
    Application.put_env(:exhub, :kiro_api_key, fetch_secret.("kiro_api_key"))
    Application.put_env(:exhub, :anthropic_api_key, fetch_secret.("anthropic_api_key"))
    Application.put_env(:exhub, :openrouter_api_key, fetch_secret.("openrouter_api_key"))
    Application.put_env(:exhub, :nvidia_api_key, fetch_secret.("nvidia_api_key"))
    Application.put_env(:exhub, :baidu_anthropic_api_key, fetch_secret.("baidu_anthropic_api_key"))
  end

  defp transform_kimi_reasoning_body(body) do
    messages = Map.get(body, "messages")

    if is_list(messages) do
      transformed_messages =
        Enum.map(messages, fn msg ->
          if is_map(msg) and
               Map.get(msg, "role") == "assistant" and
               is_list(Map.get(msg, "tool_calls")) and
               length(Map.get(msg, "tool_calls")) > 0 and
               is_nil(Map.get(msg, "reasoning_content")) do
            tool_calls = Map.get(msg, "tool_calls")
            cached = Exhub.Router.ReasoningCache.get_for_tool_calls(tool_calls)
            Map.put(msg, "reasoning_content", cached || ".")
          else
            msg
          end
        end)

      Map.put(body, "messages", transformed_messages)
    else
      body
    end
  end
end
