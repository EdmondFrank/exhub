defmodule Exhub.MCP.Hub.ToolSearch do
  @moduledoc """
  In-memory TF-IDF search for MCP tools.
  """

  require Logger

  @doc """
  Builds an inverted index from a list of tools.
  Each tool should have: server, name, description keys.
  Returns {docs, inverted_index} tuple.
  """
  def build_index(tools) when is_list(tools) do
    docs = Enum.map(tools, &to_doc/1)
    {docs, build_inverted_index(docs)}
  end

  def build_index(_), do: {%{}, %{}}

  @doc """
  Searches the index for tools matching the query.
  Returns a list of scored results sorted by relevance.
  """
  def search(index_tuple, query, opts \\ [])

  def search({docs, index}, query, opts) when is_binary(query) do
    limit = Keyword.get(opts, :limit, 5)
    tokens = tokenize(query)

    if tokens == [] do
      []
    else
      scored = score_docs(index, tokens)

      # Sort by score descending
      sorted = Enum.sort_by(scored, fn {_id, score} -> score end, :desc)

      # Take top results and map back to full documents
      sorted
      |> Enum.take(limit)
      |> Enum.map(fn {doc_id, _score} ->
        Enum.find(docs, %{}, &(&1["id"] == doc_id))
      end)
      |> Enum.reject(&(&1 == %{}))
    end
  end

  def search(_, _, _), do: []

  # --- Private ---

  defp to_doc(tool) do
    server = Map.get(tool, "server", "unknown")
    name = Map.get(tool, "name", "")
    description = Map.get(tool, "description", "")
    full_name = "#{server}__#{name}"

    text = "#{name} #{description}"
    tokens = tokenize(text)

    %{
      "id" => full_name,
      "server" => server,
      "name" => name,
      "full_name" => full_name,
      "description" => description,
      "input_schema" => Map.get(tool, "inputSchema", %{}),
      "tokens" => tokens,
      "token_freqs" => token_frequencies(tokens)
    }
  end

  defp tokenize(text) when is_binary(text) do
    text
    |> String.downcase()
    |> String.replace(~r/[^\w\s]/u, " ")
    |> String.split()
    |> Enum.reject(&(&1 in stop_words()))
  end

  defp tokenize(_), do: []

  defp stop_words do
    ~w(the a an is are was were be been have has had do does did will would could should)
  end

  defp token_frequencies(tokens) do
    Enum.frequencies(tokens)
  end

  defp build_inverted_index(docs) do
    docs
    |> Enum.reduce(%{}, fn doc, acc ->
      Enum.reduce(doc["tokens"], acc, fn token, idx_acc ->
        Map.update(idx_acc, token, [doc["id"]], &[doc["id"] | &1])
      end)
    end)
    |> Map.new(fn {token, doc_ids} ->
      {token, Enum.frequencies(doc_ids)}
    end)
  end

  defp score_docs(index, query_tokens) do
    query_tokens
    |> Enum.reduce(%{}, fn token, acc ->
      case Map.get(index, token) do
        nil ->
          acc

        doc_freqs ->
          Enum.reduce(doc_freqs, acc, fn {doc_id, freq}, scores_acc ->
            score = :math.log(1 + freq)
            Map.update(scores_acc, doc_id, score, &(&1 + score))
          end)
      end
    end)
    |> Enum.map(fn {doc_id, score} -> {doc_id, score} end)
  end
end
