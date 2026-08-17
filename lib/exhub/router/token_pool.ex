defmodule Exhub.Router.TokenPool do
  @moduledoc """
  Token pool selection for Gitee AI (moark) upstream billing modes.

  The moark Serverless API bills in two modes:

    * token-based — pay per token, deducted from a token resource package
    * request-based — flat price per call, regardless of context size

  Each mode is charged via a dedicated access token (API key). This module
  decides which pool (and therefore which `Authorization` token) a request
  should use based on the estimated context token count:

    * context tokens < `:giteeai_pool_threshold` (default 20_000) →
      `:token_based` (cheaper than the flat per-request price at break-even)
    * context tokens >= threshold → `:request_based` (flat fee caps the cost)

  ## Configuration

    * `:exhub -> :giteeai_token_api_key` — access token for the token-based pool
    * `:exhub -> :giteeai_request_api_key` — access token for the request-based pool
    * `:exhub -> :giteeai_pool_threshold` — context-token threshold (default 20_000)

  When neither pool key is configured the pool is disabled and requests keep
  using the regular `:giteeai_api_key`, preserving existing behavior.
  """

  require Logger

  alias Exhub.Router.Helpers

  @typedoc "Billing mode selected for a request"
  @type mode :: :token_based | :request_based

  @default_threshold 20_000

  @default_key_key :giteeai_api_key
  @token_mode_key :giteeai_token_api_key
  @request_mode_key :giteeai_request_api_key

  @doc """
  Context-token threshold above which the request-based pool is preferred.
  """
  @spec threshold() :: pos_integer()
  def threshold do
    Application.get_env(:exhub, :giteeai_pool_threshold, @default_threshold)
  end

  @doc """
  Whether the token pool is enabled, i.e. at least one pool mode key is set.

  ## Examples

      iex> Exhub.Router.TokenPool.enabled?()
      true
  """
  @spec enabled?() :: boolean()
  def enabled? do
    token_key() != "" or request_key() != ""
  end

  @doc """
  Selects the billing mode for a given estimated context token count.

  Counts below the threshold use the token-based pool (best economic
  efficiency for small contexts); counts at or above it use the
  request-based pool (flat fee caps the cost).

  ## Examples

      iex> Exhub.Router.TokenPool.select_mode(5000)
      :token_based

      iex> Exhub.Router.TokenPool.select_mode(25_000)
      :request_based
  """
  @spec select_mode(integer() | nil) :: mode()
  def select_mode(token_count) when is_integer(token_count) do
    if token_count < threshold(), do: :token_based, else: :request_based
  end

  def select_mode(_), do: :token_based

  @doc """
  Returns the API key for a billing mode, falling back to the default
  `:giteeai_api_key` when the mode key is not configured.
  """
  @spec api_key(mode()) :: String.t()
  def api_key(mode) do
    key =
      case mode do
        :token_based -> token_key()
        :request_based -> request_key()
      end

    fallback(key)
  end

  @doc """
  Resolves the `Authorization` token for a Gitee AI upstream request.

  When the pool is enabled and `model` is a GiteeAI model, estimates the
  context tokens from the request body (messages + system + tools), selects
  the billing mode and returns the matching pool key. `fallback` is returned
  in every other case (pool disabled, non-GiteeAI model, empty body),
  preserving existing routing behavior.

  ## Options

    * `:fallback` — token used when the pool does not apply (default:
      values of `:giteeai_api_key`)
  """
  @spec resolve_token(String.t() | nil, map(), Keyword.t()) :: String.t()
  def resolve_token(model, body_params, opts \\ []) do
    fallback = Keyword.get(opts, :fallback, default_key())

    if enabled?() and giteeai_model?(model) do
      token_count = estimate_tokens(body_params)
      mode = select_mode(token_count)
      token = api_key(mode)

      Logger.info(
        "[TokenPool] #{inspect(model)} — estimated #{token_count} context tokens, " <>
          "using #{mode} pool (#{token != fallback})"
      )

      token
    else
      fallback
    end
  end

  @doc """
  Estimates context tokens from an OpenAI-compatible request body map.

  Delegates to `Exhub.Router.Helpers.estimate_input_tokens/3` and accounts
  for `messages`, `system` and `tools` fields. Returns 0 for non-map bodies.

  ## Examples

      iex> Exhub.Router.TokenPool.estimate_tokens(%{"messages" => [%{"content" => "Hello"}]})
      1
  """
  @spec estimate_tokens(any()) :: non_neg_integer()
  def estimate_tokens(body) when is_map(body) do
    messages = Map.get(body, "messages", [])
    system = Map.get(body, "system")
    tools = Map.get(body, "tools", [])
    Helpers.estimate_input_tokens(messages, system, tools)
  end

  def estimate_tokens(_), do: 0

  @doc """
  Estimates context tokens from a LangChain message list.

  Accepts `%LangChain.Message{}` structs (binary `content` or
  `%LangChain.Message.ContentPart{}` lists) as well as plain maps with a
  `content` / `"content"` key. Returns 0 for non-list inputs.

  ## Examples

      iex> Exhub.Router.TokenPool.estimate_langchain_tokens([
      ...>   LangChain.Message.new_user!("Hello")
      ...> ])
      1
  """
  @spec estimate_langchain_tokens(any()) :: non_neg_integer()
  def estimate_langchain_tokens(messages) when is_list(messages) do
    body =
      Enum.map(messages, fn
        %LangChain.Message{content: content} -> %{"content" => flatten_langchain_content(content)}
        %{content: content} when not is_nil(content) -> %{"content" => flatten_langchain_content(content)}
        %{"content" => content} when not is_nil(content) -> %{"content" => flatten_langchain_content(content)}
        _ -> %{"content" => ""}
      end)

    estimate_tokens(%{"messages" => body})
  end

  def estimate_langchain_tokens(_), do: 0

  @doc """
  Resolves the token-pool `Authorization` key for a LangChain request.

  Same policy as `resolve_token/3` but for chains: the context size is
  estimated from the LangChain `messages` that will be sent, so the pool
  can be picked right before execution. Returns `fallback` (or `nil`) when
  the pool does not apply — e.g. pool disabled or a non-GiteeAI model —
  so callers can keep their existing key untouched.
  """
  @spec resolve_langchain_key(String.t() | nil, list(), Keyword.t()) :: String.t() | nil
  def resolve_langchain_key(model, messages, opts \\ []) do
    fallback = Keyword.get(opts, :fallback)

    if enabled?() and giteeai_model?(bare_model(model)) do
      token_count = estimate_langchain_tokens(messages)
      mode = select_mode(token_count)

      case api_key(mode) do
        "" ->
          fallback

        token ->
          Logger.info(
            "[TokenPool] #{inspect(model)} (LangChain) — estimated #{token_count} " <>
              "context tokens, using #{mode} pool"
          )

          token
      end
    else
      fallback
    end
  end

  defp giteeai_model?(model) when is_binary(model) do
    model in Exhub.LLMModels.giteeai_models() and model not in Exhub.LLMModels.minimax_models()
  end

  defp giteeai_model?(_), do: false

  # Strips the provider prefix (e.g. "openai/deepseek-v3") used by LLM config
  # model names so GiteeAI membership checks match the bare model name.
  defp bare_model(model) when is_binary(model), do: model |> String.split("/") |> List.last()
  defp bare_model(_), do: nil

  defp flatten_langchain_content(content) when is_binary(content), do: content

  defp flatten_langchain_content(content) when is_list(content) do
    content
    |> Enum.map(fn
      %LangChain.Message.ContentPart{content: c} when is_binary(c) -> c
      %{content: c} when is_binary(c) -> c
      %{"content" => c} when is_binary(c) -> c
      _ -> ""
    end)
    |> Enum.join("\n")
  end

  defp flatten_langchain_content(content), do: inspect(content)

  defp token_key, do: Application.get_env(:exhub, @token_mode_key, "")
  defp request_key, do: Application.get_env(:exhub, @request_mode_key, "")
  defp default_key, do: Application.get_env(:exhub, @default_key_key, "")

  defp fallback(""), do: default_key()
  defp fallback(key), do: key
end