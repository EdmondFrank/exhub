defmodule Exhub.MCP.Brain.RAG.Embedder do
  @moduledoc """
  HTTP client for text embeddings used by the Brain RAG pipeline.

  Calls an OpenAI-compatible `/embeddings` endpoint. The provider is
  configurable via `:exhub -> :brain_rag`:

      config :exhub, :brain_rag,
        %{
          "provider" => "openai",          # "openai" | "gitee_ai"
          "embedding_model" => "text-embedding-3-small",
          "dim" => 1536,
          "api_base" => "https://api.openai.com/v1"
        }

  For `"gitee_ai"` the API key comes from `:exhub -> :giteeai_api_key` and
  defaults to the moark endpoint. For `"openai"` the key comes from
  `:exhub -> :openai_api_key`.

  All functions return `{:ok, embedding}` / `{:ok, embeddings}` on success
  and `{:error, reason}` on failure. Callers are expected to degrade
  gracefully (e.g. fall back to keyword-only search).
  """

  require Logger

  @default_model "text-embedding-3-small"
  @default_dim 1536
  @default_openai_base "https://api.openai.com/v1"
  @default_gitee_base "https://api.moark.com/v1"
  @http_timeout_ms 60_000

  @doc "Encode a single text into a vector."
  @spec encode(String.t()) :: {:ok, [float()]} | {:error, String.t()}
  def encode(text) when is_binary(text) do
    case encode_batch([text]) do
      {:ok, [embedding | _]} -> {:ok, embedding}
      {:ok, []} -> {:error, "no embeddings returned"}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc "Encode a batch of texts into vectors, preserving order."
  @spec encode_batch([String.t()]) :: {:ok, [[float()]]} | {:error, String.t()}
  def encode_batch(texts) when is_list(texts) do
    texts = Enum.reject(texts, &(not is_binary(&1) or String.trim(&1) == ""))

    if texts == [] do
      {:ok, []}
    else
      with {:ok, %{provider: provider, model: model, api_base: api_base, api_key: api_key}} <-
             config(),
           {:ok, body} <- do_request(provider, model, api_base, api_key, texts) do
        parse_response(body)
      end
    end
  end

  @doc "Return the configured embedding dimension."
  @spec dimension() :: pos_integer()
  def dimension do
    config()
    |> case do
      {:ok, %{dim: dim}} -> dim
      _ -> @default_dim
    end
  end

  # ── config ───────────────────────────────────────────────────────────

  defp config do
    cfg = Application.get_env(:exhub, :brain_rag, %{}) |> Map.new()

    provider = cfg["provider"] || "openai"
    model = cfg["embedding_model"] || @default_model
    dim = cfg["dim"] || dim_for(model)

    case provider do
      "gitee_ai" ->
        api_key = Application.get_env(:exhub, :giteeai_api_key, "")
        api_base = cfg["api_base"] || @default_gitee_base
        {:ok, %{provider: provider, model: model, dim: dim, api_base: api_base, api_key: api_key}}

      _ ->
        api_key = Application.get_env(:exhub, :openai_api_key, "")
        api_base = cfg["api_base"] || @default_openai_base
        {:ok, %{provider: provider, model: model, dim: dim, api_base: api_base, api_key: api_key}}
    end
  end

  defp dim_for("text-embedding-3-small"), do: 1536
  defp dim_for("text-embedding-3-large"), do: 3072
  defp dim_for("text-embedding-ada-002"), do: 1536
  defp dim_for("Qwen3-Embedding-4B"), do: 1024
  defp dim_for(_), do: @default_dim

  # ── request ──────────────────────────────────────────────────────────

  defp do_request(provider, model, api_base, api_key, texts) do
    if api_key == "" do
      {:error, "Embedding API key not configured for provider #{provider}"}
    else
      url = "#{api_base}/embeddings"
      body = %{model: model, input: texts}

      headers = [
        {"Content-Type", "application/json"},
        {"Authorization", "Bearer #{api_key}"}
      ]

      case HTTPoison.post(url, Jason.encode!(body), headers,
             recv_timeout: @http_timeout_ms,
             timeout: @http_timeout_ms
           ) do
        {:ok, %HTTPoison.Response{status_code: 200, body: resp_body}} ->
          {:ok, resp_body}

        {:ok, %HTTPoison.Response{status_code: status, body: resp_body}} ->
          {:error, "Embedding API error (HTTP #{status}): #{resp_body}"}

        {:error, %HTTPoison.Error{reason: reason}} ->
          {:error, "Embedding request failed: #{inspect(reason)}"}
      end
    end
  end

  defp parse_response(body) do
    case Jason.decode(body) do
      {:ok, %{"data" => data}} ->
        embeddings =
          data
          |> Enum.sort_by(&Map.get(&1, "index", 0))
          |> Enum.map(&Map.get(&1, "embedding", []))

        {:ok, embeddings}

      {:ok, %{"error" => %{"message" => msg}}} ->
        {:error, msg}

      {:ok, other} ->
        {:error, "Unexpected embedding response: #{inspect(other)}"}

      {:error, reason} ->
        {:error, "Failed to decode embedding response: #{inspect(reason)}"}
    end
  end
end