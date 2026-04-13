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
    giteeai: "https://ai.gitee.com/v1",
    kimi: "https://api.kimi.com/coding/v1",
    minimaxi: "https://api.minimaxi.com/v1",
    openrouter: "https://openrouter.ai/api/v1",
    local: "http://localhost:8765/v1",
    openai: @default_upstream
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
    "glm-5-turbo",
    "deepseek-v3",
    "deepseek-r1",
    "deepseek-v3_1",
    "deepseek-v3_1-terminus",
    "deepseek-v3.2",
    "deepseek-v3.2-exp",
    "gpt-oss-120b",
    "internvl3-78b",
    "kimi-k2.5",
    "kimi-k2-instruct",
    "kimi-k2-thinking",
    "qwen3.5-27b",
    "qwen3.5-35b-a3b",
    "qwen3-235b-a22b",
    "qwen3-235b-a22b-instruct-2507",
    "qwen3-next-80b-a3b-instruct",
    "qwen3-next-80b-a3b-thinking",
    "qwen3-coder-next",
    "qwen3-coder-480b-a35b-instruct",
    "minimax-m2",
    "minimax-m2.1",
    "minimax-m2.5",
    "minimax-m2.7",
    "minimax-m2-preview"
  ]

  @minimax_models ["minimax-m2.7", "minimax-m2-preview"]

  @doc """
  Returns the target URL for a given model.

  ## Examples

      iex> Exhub.Router.Config.get_model_target("deepseek-v3")
      "https://ai.gitee.com/v1"

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

      model in [
        "tngtech/deepseek-r1t2-chimera:free",
        "minimax/minimax-m2:free",
        "openrouter/polaris-alpha"
      ] ->
        @provider_urls.openrouter

      model in ["gemini-2.5-pro", "gemini-2.5-flash"] ->
        get_burncloud_target()

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

      model in [
        "tngtech/deepseek-r1t2-chimera:free",
        "minimax/minimax-m2:free",
        "openrouter/polaris-alpha"
      ] ->
        Application.get_env(:exhub, :openrouter_api_key, "")

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
    # Currently only specific models bypass proxy
    model in ["minimax-m2.1", "minimax-m2-preview"]
  end

  @doc """
  Returns the authorization header for a given model and provider type.
  Includes model-specific custom headers (e.g., X-Package for kimi-k2.5).

  ## Examples

      iex> Exhub.Router.Config.get_auth_headers("deepseek-v3", :openai)
      [{"authorization", "Bearer <giteeai_api_key>"}]

      iex> Exhub.Router.Config.get_auth_headers("kimi-k2.5", :openai)
      [{"authorization", "Bearer <giteeai_api_key>"}, {"x-package", "6609"}]

      iex> Exhub.Router.Config.get_auth_headers("minimax-m2.1", :anthropic)
      [{"x-api-key", "<minimax_api_key>"}]
  """
  @spec get_auth_headers(model(), :openai | :anthropic) :: [{String.t(), String.t()}]
  def get_auth_headers(model, :openai) do
    token = get_model_api_key(model)
    headers = [{"authorization", "Bearer #{token}"}]

    if model == "kimi-k2.5" do
      headers ++ [{"X-Package", 6609}]
    else
      headers
    end
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

  @doc """
  Transforms request body for model-specific requirements.
  For kimi-k2.5, adds reasoning_content to assistant messages with tool_calls.

  ## Examples

      iex> Exhub.Router.Config.transform_request_body(%{"messages" => [%{"role" => "assistant", "tool_calls" => [%{}]}]}, "kimi-k2.5")
      %{"messages" => [%{"role" => "assistant", "tool_calls" => [%{}], "reasoning_content" => "."}]}

      iex> Exhub.Router.Config.transform_request_body(%{"model" => "test"}, "deepseek-v3")
      %{"model" => "test"}
  """
  @spec transform_request_body(map(), model()) :: map()
  def transform_request_body(body, model) when is_map(body) and is_binary(model) do
    case model do
      "kimi-k2.5" -> transform_kimi_k2_5_body(body)
      _ -> body
    end
  end

  defp transform_kimi_k2_5_body(body) do
    messages = Map.get(body, "messages")

    if is_list(messages) do
      transformed_messages =
        Enum.map(messages, fn msg ->
          if is_map(msg) and
               Map.get(msg, "role") == "assistant" and
               is_list(Map.get(msg, "tool_calls")) and
               length(Map.get(msg, "tool_calls")) > 0 and
               is_nil(Map.get(msg, "reasoning_content")) do
            Map.put(msg, "reasoning_content", ".")
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
