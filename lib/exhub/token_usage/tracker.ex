defmodule Exhub.TokenUsage.Tracker do
  @moduledoc """
  Helper module for tracking token usage across the application.

  Provides functions to extract token usage from various LLM provider responses
  and record them to the TokenUsageStore.
  """

  alias Exhub.TokenUsage.TokenUsageStore
  require Logger

  @doc """
  Extract token usage from an OpenAI-compatible response and record it.

  ## Parameters
    - response_body: The response body from the API (JSON string or map)
    - model: The model name used for the request
    - provider: The provider name (e.g., "openai", "gitee", "deepseek")
    - request_body: Optional request body for estimating input tokens when not in response

  ## Examples
      iex> Tracker.track_openai_usage('{"usage":{"prompt_tokens":100,"completion_tokens":50}}', "gpt-4", "openai")
      :ok
  """
  @spec track_openai_usage(String.t() | map(), String.t(), String.t(), String.t() | nil) :: :ok
  def track_openai_usage(response_body, model, provider, request_body \\ nil)

  def track_openai_usage(response_body, model, provider, request_body)
      when is_binary(response_body) do
    case Jason.decode(response_body) do
      {:ok, decoded} ->
        track_openai_usage(decoded, model, provider, request_body)

      {:error, _} ->
        track_estimate_only(model, provider, response_body, request_body)
    end
  end

  def track_openai_usage(response, model, provider, request_body) when is_map(response) do
    input_tokens =
      deep_get(response, ["usage", "prompt_tokens"]) ||
        deep_get(response, ["usage", "input_tokens"]) ||
        0

    output_tokens =
      deep_get(response, ["usage", "completion_tokens"]) ||
        deep_get(response, ["usage", "output_tokens"]) ||
        0

    # If response doesn't have input tokens but we have request body, estimate from it
    input_tokens =
      if input_tokens == 0 and request_body do
        estimate_tokens_from_text(request_body)
      else
        input_tokens
      end

    if input_tokens > 0 or output_tokens > 0 do
      record_usage(model, provider, input_tokens, output_tokens)
    else
      track_estimate_only(model, provider, Jason.encode!(response), request_body)
    end
  end

  @doc """
  Extract token usage from an Anthropic response and record it.

  ## Parameters
    - response_body: The response body from the API (JSON string or map)
    - model: The model name used for the request
    - provider: The provider name (e.g., "anthropic")
    - request_body: Optional request body for estimating input tokens

  ## Examples
      iex> Tracker.track_anthropic_usage('{"usage":{"input_tokens":100,"output_tokens":50}}', "claude-3", "anthropic")
      :ok
  """
  @spec track_anthropic_usage(String.t() | map(), String.t(), String.t(), String.t() | nil) :: :ok
  def track_anthropic_usage(response_body, model, provider, request_body \\ nil)

  def track_anthropic_usage(response_body, model, provider, request_body)
      when is_binary(response_body) do
    case Jason.decode(response_body) do
      {:ok, decoded} ->
        track_anthropic_usage(decoded, model, provider, request_body)

      {:error, _} ->
        track_estimate_only(model, provider, response_body, request_body)
    end
  end

  def track_anthropic_usage(response, model, provider, request_body) when is_map(response) do
    input_tokens = deep_get(response, ["usage", "input_tokens"]) || 0
    output_tokens = deep_get(response, ["usage", "output_tokens"]) || 0

    # If response doesn't have input tokens but we have request body, estimate from it
    input_tokens =
      if input_tokens == 0 and request_body do
        estimate_tokens_from_text(request_body)
      else
        input_tokens
      end

    if input_tokens > 0 or output_tokens > 0 do
      record_usage(model, provider, input_tokens, output_tokens)
    else
      track_estimate_only(model, provider, Jason.encode!(response), request_body)
    end
  end

  @doc """
  Track usage with estimated token counts based on response size.

  ## Parameters
    - model: The model name
    - provider: The provider name
    - response_body: The response body (used for estimating output size)
    - request_body: Optional request body (used for estimating input size)

  ## Examples
      iex> Tracker.track_estimate_only("gpt-4", "openai", "This is the response text")
      :ok
  """
  @spec track_estimate_only(String.t(), String.t(), String.t(), String.t() | nil) :: :ok
  def track_estimate_only(model, provider, response_body, request_body \\ nil) do
    output_estimate = estimate_tokens_from_text(response_body)
    input_estimate = if request_body, do: estimate_tokens_from_text(request_body), else: 0

    record_usage(model, provider, input_estimate, output_estimate)
  end

  @doc """
  Record streaming token usage.

  For streaming responses where we accumulate tokens over multiple chunks.

  ## Parameters
    - model: The model name
    - provider: The provider name
    - input_tokens: Total input tokens
    - output_tokens: Total output tokens
    - metadata: Additional metadata (request_id, timestamp, etc.)

  ## Examples
      iex> Tracker.record_streaming_usage("gpt-4", "openai", 100, 50, %{request_id: "req-123"})
      :ok
  """
  @spec record_streaming_usage(
          String.t(),
          String.t(),
          non_neg_integer(),
          non_neg_integer(),
          map()
        ) :: :ok
  def record_streaming_usage(model, provider, input_tokens, output_tokens, metadata \\ %{}) do
    record_usage(model, provider, input_tokens, output_tokens, metadata)
  end

  # Private functions

  defp record_usage(model, provider, input_tokens, output_tokens, metadata \\ %{}) do
    if Process.whereis(Exhub.TokenUsage.TokenUsageStore) do
      TokenUsageStore.record_usage(model, provider, input_tokens, output_tokens, metadata)

      Logger.debug(
        "[TokenUsage] Recorded: model=#{model}, provider=#{provider}, " <>
          "input=#{input_tokens}, output=#{output_tokens}"
      )
    else
      Logger.warning("[TokenUsage] TokenUsageStore not available, skipping tracking")
    end

    :ok
  rescue
    e ->
      Logger.error("[TokenUsage] Failed to record usage: #{inspect(e)}")
      :ok
  end

  def estimate_tokens_from_text(text) when is_binary(text) do
    # Rough estimation: ~4 characters per token
    div(String.length(text), 4)
  end

  def estimate_tokens_from_text(_), do: 0

  defp deep_get(map, keys) when is_map(map) do
    deep_get_nested(map, keys)
  end

  defp deep_get_nested(nil, _), do: nil
  defp deep_get_nested(map, []), do: map

  defp deep_get_nested(map, [key | rest]) when is_map(map) do
    deep_get_nested(Map.get(map, key), rest)
  end

  defp deep_get_nested(_, _), do: nil
end
