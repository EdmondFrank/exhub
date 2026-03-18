defmodule Exhub.Router.Helpers do
  @moduledoc """
  Helper functions for the Router module.

  Provides utility functions for:
  - Request/response parsing
  - Token estimation
  - Parameter handling
  - Common transformations
  """

  require Logger

  @typedoc "Connection struct from Plug"
  @type conn :: Plug.Conn.t()

  @typedoc "Model name extracted from request"
  @type model :: String.t() | nil

  @doc """
  Extracts the model name from a connection's body parameters.

  Checks content-type header and parses JSON body to find the model field.

  ## Examples

      iex> Exhub.Router.Helpers.extract_model(conn_with_json_body)
      "deepseek-v3"

      iex> Exhub.Router.Helpers.extract_model(conn_without_body)
      nil
  """
  @spec extract_model(conn()) :: model()
  def extract_model(conn) do
    case Plug.Conn.get_req_header(conn, "content-type") do
      ["application/json" <> _] ->
        case conn.body_params do
          %{"model" => model} when is_binary(model) -> model
          _ -> nil
        end

      _ ->
        nil
    end
  end

  @doc """
  Parses an integer value from various input types.

  ## Examples

      iex> Exhub.Router.Helpers.parse_int("42", 10)
      42

      iex> Exhub.Router.Helpers.parse_int(nil, 10)
      10

      iex> Exhub.Router.Helpers.parse_int("invalid", 10)
      10
  """
  @spec parse_int(any(), integer()) :: integer()
  def parse_int(nil, default), do: default

  def parse_int(value, default) when is_binary(value) do
    case Integer.parse(value) do
      {int, _} -> int
      :error -> default
    end
  end

  def parse_int(value, _default) when is_integer(value), do: value
  def parse_int(_, default), do: default

  @doc """
  Conditionally puts a value into a map if the value is not nil.

  ## Examples

      iex> Exhub.Router.Helpers.maybe_put(%{}, :key, "value")
      %{key: "value"}

      iex> Exhub.Router.Helpers.maybe_put(%{}, :key, nil)
      %{}
  """
  @spec maybe_put(map(), any(), any()) :: map()
  def maybe_put(map, _key, nil), do: map
  def maybe_put(map, key, value), do: Map.put(map, key, value)

  @doc """
  Estimates input tokens for a request based on messages, system prompt, and tools.

  Uses a rough heuristic of 4 characters per token for estimation.

  ## Examples

      iex> Exhub.Router.Helpers.estimate_input_tokens(
      ...>   [%{"content" => "Hello"}],
      ...>   "System prompt",
      ...>   [%{"name" => "tool1"}]
      ...> )
      8
  """
  @spec estimate_input_tokens(list(map()), any(), list(map())) :: non_neg_integer()
  def estimate_input_tokens(messages, system, tools) do
    # Ensure messages and tools are lists (not nil/null)
    messages = if is_list(messages), do: messages, else: []
    tools = if is_list(tools), do: tools, else: []

    # Rough estimation: count characters and divide by 4
    message_tokens =
      Enum.reduce(messages, 0, fn msg, acc ->
        content = Map.get(msg, "content", "")
        content_str = if is_binary(content), do: content, else: inspect(content)
        acc + div(String.length(content_str), 4)
      end)

    system_tokens =
      if system do
        system_str = if is_binary(system), do: system, else: inspect(system)
        div(String.length(system_str), 4)
      else
        0
      end

    tool_tokens =
      Enum.reduce(tools, 0, fn tool, acc ->
        acc + div(String.length(Map.get(tool, "name", "")), 4) +
          div(String.length(inspect(Map.get(tool, "input_schema", ""))), 4)
      end)

    message_tokens + system_tokens + tool_tokens
  end

  @doc """
  Estimates output tokens from a response string.

  ## Examples

      iex> Exhub.Router.Helpers.estimate_output_tokens("Hello world")
      2
  """
  @spec estimate_output_tokens(String.t()) :: non_neg_integer()
  def estimate_output_tokens(response) when is_binary(response) do
    div(String.length(response), 4)
  end

  def estimate_output_tokens(_), do: 0

  @doc """
  Sends an SSE (Server-Sent Events) event to the connection.

  ## Examples

      iex> Exhub.Router.Helpers.send_sse_event(conn, "message_start", %{"type" => "message_start"})
      # Returns updated conn
  """
  @spec send_sse_event(conn(), String.t(), map()) :: conn()
  def send_sse_event(conn, event_type, data) do
    {:ok, conn} = Plug.Conn.chunk(conn, "event: #{event_type}\ndata: #{Jason.encode!(data)}\n\n")
    conn
  end

  @doc """
  Tracks token usage asynchronously.

  Spawns a process to record usage without blocking the response.

  ## Examples

      iex> Exhub.Router.Helpers.track_usage("gpt-4", "openai", 100, 50, "req_123")
      # Returns :ok, spawns tracking process
  """
  @spec track_usage(String.t(), String.t(), non_neg_integer(), non_neg_integer(), String.t()) ::
          :ok
  def track_usage(model, provider, input_tokens, output_tokens, request_id) do
    spawn(fn ->
      Exhub.TokenUsage.TokenUsageStore.record_usage(
        model,
        provider,
        input_tokens,
        output_tokens,
        %{request_id: request_id, timestamp: DateTime.utc_now()}
      )
    end)

    :ok
  end

  @doc """
  Returns standardized error response based on error type.

  ## Examples

      iex> Exhub.Router.Helpers.get_error_message("timeout error")
      "Request timed out. Please try again with a shorter request or reduce max_tokens."
  """
  @spec get_error_message(any()) :: String.t()
  def get_error_message(error) do
    error_str = to_string(error)

    cond do
      String.contains?(error_str, "timeout") ->
        "Request timed out. Please try again with a shorter request or reduce max_tokens."

      String.contains?(error_str, "rate_limit") ->
        "Rate limit exceeded. Please wait and try again."

      String.contains?(error_str, "authentication") or String.contains?(error_str, "api_key") ->
        "Authentication failed. Please check your API key configuration."

      true ->
        "LLM execution failed: #{error_str}"
    end
  end
end
