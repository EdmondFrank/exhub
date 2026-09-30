defmodule Exhub.MCP.Tools.Memory.Helpers do
  @moduledoc "Shared response and argument helpers for the Memory MCP tools."

  alias Anubis.Server.Response

  @spec text(term(), String.t()) :: {:reply, Response.t(), term()}
  def text(frame, text), do: {:reply, Response.tool() |> Response.text(text), frame}

  @spec json(term(), term()) :: {:reply, Response.t(), term()}
  def json(frame, data), do: {:reply, Response.tool() |> Response.json(data), frame}

  @spec error(term(), term()) :: {:reply, Response.t(), term()}
  def error(frame, reason),
    do: {:reply, Response.tool() |> Response.error(to_string(reason)), frame}

  @doc "Normalize a comma-separated string or list into a list of strings."
  @spec str_list(term()) :: [String.t()]
  def str_list(nil), do: []

  def str_list(value) when is_list(value), do: Enum.map(value, &to_string/1)

  def str_list(value) when is_binary(value) do
    value
    |> String.split(",")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
  end

  def str_list(_), do: []

  @doc "First non-empty line of `text`, trimmed and capped."
  @spec default_title(String.t(), non_neg_integer()) :: String.t()
  def default_title(text, max \\ 80) do
    text
    |> to_string()
    |> String.split("\n")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> List.first()
    |> case do
      nil -> "Memory"
      line -> String.slice(line, 0, max)
    end
  end
end
