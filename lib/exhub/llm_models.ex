defmodule Exhub.LLMModels do
  @moduledoc """
  Shared LLM model definitions and provider mappings.

  This module serves as the single source of truth for:
  - Provider base URLs
  - Model to provider mappings
  - Model normalization rules
  - Provider-specific model lists
  - Models requiring reasoning_content injection

  Both `Exhub.Router.Config` and `Exhub.Llm.LlmConfigServer`
  (via runtime.exs) derive their model data from here.

  ## Deprecated Models

  The following models have been removed from `build_llms_config/1` and are
  no longer supported. Users should migrate to the models listed in
  `giteeai_models/0`:

  - `openai/QwQ-32B` (Samba) — use `deepseek-v3` or `deepseek-r1` instead
  - `openai/Qwen/Qwen2.5-Coder-32B-Instruct` (SiliconFlow) — use `qwen3-coder-30b-a3b-instruct` instead
  - `openai/Qwen/Qwen2.5-32B-Instruct` (SiliconFlow) — use `qwen3.5-27b` instead
  - `anthropic/claude-3-5-sonnet-latest` — use `claude-3.7-sonnet` (Kiro) instead
  - `groq/llama-3.3-70b-versatile` — use `qwen3.5-122b-a10b` instead
  - `openai/gemini-2.5-pro` / `gemini/gemini-2.0-flash` — use models from `giteeai_models/0` instead
  """

  # ============================================================================
  # Provider Definitions
  # ============================================================================

  @providers %{
    giteeai: "https://api.moark.com/v1",
    kimi: "https://api.kimi.com/coding/v1",
    minimaxi: "https://api.minimaxi.com/v1",
    mimo: "https://token-plan-sgp.xiaomimimo.com/v1",
    openrouter: "https://openrouter.ai/api/v1",
    local: "http://localhost:8765/v1",
    infini: "https://cloud.infini-ai.com/maas/v1",
    kiro: "http://localhost:8000/v1",
    nvidia: "https://integrate.api.nvidia.com/v1",
    baidu_anthropic: "http://211.23.3.236:27545/v1",
    runinfra: "https://api.runinfra.ai/v1",
    orcarouter: "https://api.orcarouter.ai/v1"
  }

  @default_upstream "https://api.moark.com/v1"

  # ============================================================================
  # Model Lists per Provider
  # ============================================================================

  # Most GiteeAI models share the same endpoint
  @giteeai_models [
    "step3",
    "step-3.7-flash",
    "glm-4_5",
    "glm-4_5v",
    "glm-4.6",
    "glm-4.7",
    "glm-4.7-flash",
    "glm-4.5-air",
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
    "kimi-k3",
    "qwen3.6-max",
    "qwen3.6-plus",
    "qwen3.6-flash",
    "qwen3.6-27b",
    "qwen3.6-35b-a3b",
    "qwen3.5-9b",
    "qwen3.5-27b",
    "qwen3.5-35b-a3b",
    "qwen3.5-122b-a10b",
    "qwen3.5-flash",
    "qwen3.5-plus",
    "qwen3-235b-a22b",
    "qwen3-235b-a22b-instruct-2507",
    "qwen3-next-80b-a3b-instruct",
    "qwen3-next-80b-a3b-thinking",
    "qwen3-coder-next",
    "qwen3-coder-flash",
    "qwen3-coder-30b-a3b-instruct",
    "qwen3-coder-480b-a35b-instruct",
    "qwen3-30b-a3b-instruct-2507",
    "qwen3.5-27b-claude-4.6-opus-reasoning-distilled",
    "qwen3.7-plus",
    "qwen3.7-max",
    "minimax-m2",
    "minimax-m2.1",
    "minimax-m2.5",
    "minimax-m2.7",
    "minimax-m2-preview",
    "minimax-m3",
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
    "nvidia/nemotron-3-ultra-550b-a55b:free",
    "tencent/hy3:free"
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

  # RunInfra AI models
  @runinfra_models [
    "qwen3-8-27b"
  ]

  # OrcaRouter AI models (OpenAI-compatible aggregator)
  @orcarouter_models [
    "tencent/hy3-free",
    "deepseek/deepseek-v4-flash-free",
    "qwen/qwen3.8-27b-free"
  ]

  # ============================================================================
  # Model Normalization Mappings
  # ============================================================================

  @infini_model_mapping %{
    "inf-glm-5.1" => "glm-5.1",
    "inf-glm-5.2" => "glm-5.2",
    "inf-kimi-k2.5" => "kimi-k2.5",
    "inf-kimi-k2.7-code" => "kimi-k2.7-code",
    "inf-minimax-m2.7" => "minimax-m2.7",
    "inf-deepseek-v3.2" => "deepseek-v3.2"
  }

  # ============================================================================
  # Models Requiring reasoning_content Injection
  # ============================================================================

  @kimi_reasoning_models [
    "kimi-k2.5",
    "kimi-k2.6",
    "kimi-k2.7-code",
    "inf-kimi-k2.5",
    "inf-kimi-k2.7-code",
    "mimo-v2.5-pro",
    "mimo-v2.5"
  ]

  # ============================================================================
  # Public API
  # ============================================================================

  @doc """
  Returns all configured provider base URLs.
  """
  @spec providers() :: %{atom() => String.t()}
  def providers, do: @providers

  @doc """
  Returns the default upstream URL.
  """
  @spec default_upstream() :: String.t()
  def default_upstream, do: @default_upstream

  @doc """
  Returns the list of GiteeAI models.
  """
  @spec giteeai_models() :: [String.t()]
  def giteeai_models, do: @giteeai_models

  @doc """
  Returns the list of Minimax models.
  """
  @spec minimax_models() :: [String.t()]
  def minimax_models, do: @minimax_models

  @doc """
  Returns the list of MiMo models.
  """
  @spec mimo_models() :: [String.t()]
  def mimo_models, do: @mimo_models

  @doc """
  Returns the list of Infini models.
  """
  @spec infini_models() :: [String.t()]
  def infini_models, do: @infini_models

  @doc """
  Returns the list of OpenRouter models.
  """
  @spec openrouter_models() :: [String.t()]
  def openrouter_models, do: @openrouter_models

  @doc """
  Returns the list of Kiro models.
  """
  @spec kiro_models() :: [String.t()]
  def kiro_models, do: @kiro_models

  @doc """
  Returns the list of NVIDIA models.
  """
  @spec nvidia_models() :: [String.t()]
  def nvidia_models, do: @nvidia_models

  @doc """
  Returns the list of RunInfra models.
  """
  @spec runinfra_models() :: [String.t()]
  def runinfra_models, do: @runinfra_models

  @doc """
  Returns the list of OrcaRouter models.
  """
  @spec orcarouter_models() :: [String.t()]
  def orcarouter_models, do: @orcarouter_models

  @doc """
  Returns the Infini model mapping (prefixed → actual).
  """
  @spec infini_model_mapping() :: %{String.t() => String.t()}
  def infini_model_mapping, do: @infini_model_mapping

  @doc """
  Returns the list of models requiring reasoning_content injection.
  """
  @spec kimi_reasoning_models() :: [String.t()]
  def kimi_reasoning_models, do: @kimi_reasoning_models

  @doc """
  Returns the provider URL for a given provider atom.

  ## Examples

      iex> Exhub.LLMModels.provider_url(:giteeai)
      "https://api.moark.com/v1"

      iex> Exhub.LLMModels.provider_url(:unknown)
      nil
  """
  @spec provider_url(atom()) :: String.t() | nil
  def provider_url(provider) when is_atom(provider) do
    Map.get(@providers, provider)
  end

  @doc """
  Checks if a model requires reasoning_content injection.

  ## Examples

      iex> Exhub.LLMModels.reasoning_model?("kimi-k2.5")
      true

      iex> Exhub.LLMModels.reasoning_model?("deepseek-v3")
      false
  """
  @spec reasoning_model?(String.t()) :: boolean()
  def reasoning_model?(model) when is_binary(model) do
    model in @kimi_reasoning_models
  end

  @doc """
  Normalizes a model name by stripping provider prefixes.
  For Infini models (inf-*), returns the actual model name used by the API.

  ## Examples

      iex> Exhub.LLMModels.normalize_model_name("inf-deepseek-v3.2")
      "deepseek-v3.2"

      iex> Exhub.LLMModels.normalize_model_name("deepseek-v3")
      "deepseek-v3"
  """
  @spec normalize_model_name(String.t()) :: String.t()
  def normalize_model_name(model) when is_binary(model) do
    case Map.get(@infini_model_mapping, model) do
      nil -> model
      actual_name -> actual_name
    end
  end

  @doc """
  Returns the provider atom for a given model name.

  ## Examples

      iex> Exhub.LLMModels.model_provider("deepseek-v3")
      :giteeai

      iex> Exhub.LLMModels.model_provider("minimax-m2.7")
      :minimaxi
  """
  @spec model_provider(String.t()) :: atom() | nil
  def model_provider(model) when is_binary(model) do
    cond do
      model in @giteeai_models and model not in @minimax_models ->
        :giteeai

      model == "kimi-for-coding" ->
        :kimi

      model in @minimax_models ->
        :minimaxi

      model in @mimo_models ->
        :mimo

      model in @openrouter_models ->
        :openrouter

      model in @infini_models ->
        :infini

      model in @kiro_models ->
        :kiro

      model in @nvidia_models ->
        :nvidia

      model in @runinfra_models ->
        :runinfra

      model in @orcarouter_models ->
        :orcarouter

      true ->
        nil
    end
  end

  @doc """
  Returns the target URL for a given model.

  ## Examples

      iex> Exhub.LLMModels.get_model_target("deepseek-v3")
      "https://api.moark.com/v1"

      iex> Exhub.LLMModels.get_model_target("unknown-model")
      "https://api.moark.com/v1"
  """
  @spec get_model_target(String.t() | nil) :: String.t()
  def get_model_target(nil), do: @default_upstream

  def get_model_target(model) when is_binary(model) do
    case model_provider(model) do
      nil -> @default_upstream
      provider -> provider_url(provider) || @default_upstream
    end
  end

  @doc """
  Returns all model lists combined.
  """
  @spec all_models() :: [String.t()]
  def all_models do
    @giteeai_models ++
      @minimax_models ++
      @mimo_models ++
      @infini_models ++
      @openrouter_models ++
      @kiro_models ++
      @nvidia_models ++
      @runinfra_models ++
      @orcarouter_models
  end

  # ============================================================================
  # LLMs Config Builder
  # ============================================================================

  @doc """
  Builds the llms config map from API keys.

  This function generates the configuration that `Exhub.Llm.LlmConfigServer`
  reads from `Application.get_env(:exhub, :llms)`. By centralizing the build
  logic here, both `runtime.exs` and `LlmConfigServer` share the same source
  of truth for model definitions.

  ## Supported Models

  Only models from `giteeai_models/0` and the 5 special entries (Codestral,
  Mistral, Cohere) are included. Deprecated models (SiliconFlow, Samba, Groq,
  Anthropic, Gemini) have been removed — see module doc for migration guide.

  ## Options

    * `:giteeai_api_key` - GiteeAI API key
    * `:codestral_api_key` - Codestral API key
    * `:mistral_api_key` - Mistral API key
    * `:cohere_api_key` - Cohere API key

  ## Examples

      iex> Exhub.LLMModels.build_llms_config(%{giteeai_api_key: "test"})
      %{"openai/step3" => %{api_base: "https://api.moark.com/v1", api_key: "test", model: "openai/step3"}, ...}
  """
  @spec build_llms_config(map()) :: map()
  def build_llms_config(api_keys) when is_map(api_keys) do
    gitee_api_base = "https://api.moark.com/v1"

    # Build GiteeAI models (most models share the same endpoint)
    gitee_entries =
      for model <- @giteeai_models do
        full_name = "openai/#{model}"

        {full_name,
         %{
           api_base: gitee_api_base,
           api_key: api_keys[:giteeai_api_key] || "",
           model: full_name
         }}
      end

    # Special models with different providers / endpoints
    special_entries = %{
      "codestral/codestral-latest" => %{
        api_base: "https://codestral.mistral.ai/v1",
        api_key: api_keys[:codestral_api_key] || "",
        model: "mistral/codestral-latest"
      },
      "mistral/mistral-small-latest" => %{
        api_base: "https://api.mistral.ai/v1",
        api_key: api_keys[:mistral_api_key] || "",
        model: "mistral/mistral-small-latest"
      },
      "mistral/mistral-large-latest" => %{
        api_base: "https://api.mistral.ai/v1",
        api_key: api_keys[:mistral_api_key] || "",
        model: "mistral/mistral-large-latest"
      },
      "command-r-plus" => %{
        api_base: "http://127.0.0.1:9069/cohere/v1",
        api_key: api_keys[:cohere_api_key] || "",
        model: "openai/command-r-plus"
      },
      "command-a-03-2025" => %{
        api_base: "http://127.0.0.1:9069/cohere/v1",
        api_key: api_keys[:cohere_api_key] || "",
        model: "openai/command-a-03-2025"
      }
    }

    Map.new(gitee_entries)
    |> Map.merge(special_entries)
  end

  @doc """
  Returns the default LLM name for LangChain configuration.

  This is used by `Exhub.Llm.LlmConfigServer` as the fallback default.
  """
  @spec default_llm_name() :: String.t()
  def default_llm_name, do: "openai/deepseek-v4-flash"

  @doc """
  Checks if a model name is known (exists in any model list).

  ## Examples

      iex> Exhub.LLMModels.known_model?("deepseek-v3")
      true

      iex> Exhub.LLMModels.known_model?("unknown-model")
      false
  """
  @spec known_model?(String.t()) :: boolean()
  def known_model?(model) when is_binary(model) do
    model in all_models()
  end
end
