defmodule Exhub.Toonflow.Json do
  @moduledoc """
  Lenient JSON extraction for LLM responses.

  Models routinely wrap JSON in Markdown fences or surrounding prose, and the
  pipeline is bilingual — so unlike `Jason.decode/1`, this tolerates both. The
  slice step uses `:binary.part/3` (byte offsets) rather than `String.slice/2`
  (grapheme offsets), because Chinese text makes the two diverge.

  Shared by the Toonflow extraction modules (`Exhub.Toonflow.Assets`,
  `Exhub.Toonflow.Storyboard`).
  """

  @doc "Decode `raw`, tolerating Markdown fences and surrounding prose."
  @spec decode(String.t()) :: {:ok, term()} | {:error, term()}
  def decode(raw) when is_binary(raw) do
    cleaned = strip_fences(raw)

    [cleaned, slice_json(cleaned)]
    |> Enum.reject(&is_nil/1)
    |> Enum.find_value({:error, :invalid_json}, fn candidate ->
      case Jason.decode(candidate) do
        {:ok, value} -> {:ok, value}
        {:error, _} -> nil
      end
    end)
  end

  def decode(_), do: {:error, :invalid_payload}

  @doc """
  Find a list under `key` in a decoded value.

  Tolerates a bare top-level list, and one extra wrapping object
  (`%{"shots" => %{"shots" => [...]}}`).
  """
  @spec list(term(), String.t()) :: {:ok, [term()]} | {:error, term()}
  def list(%{} = map, key) do
    cond do
      is_list(map[key]) -> {:ok, map[key]}
      is_map(map[key]) and is_list(map[key][key]) -> {:ok, map[key][key]}
      true -> {:error, {:missing, key}}
    end
  end

  def list(value, _key) when is_list(value), do: {:ok, value}
  def list(_value, key), do: {:error, {:missing, key}}

  @doc "Coerce a scalar value to a string, or `nil`."
  @spec to_text(term()) :: String.t() | nil
  def to_text(nil), do: nil
  def to_text(value) when is_binary(value), do: value
  def to_text(value) when is_number(value) or is_atom(value), do: to_string(value)
  def to_text(_value), do: nil

  @doc "Coerce to a string, treating blank strings as `nil`."
  @spec text(term()) :: String.t() | nil
  def text(value) do
    case to_text(value) do
      nil -> nil
      text -> if String.trim(text) == "", do: nil, else: text
    end
  end

  # --- internals ---

  defp strip_fences(text) do
    text
    |> String.trim()
    |> strip_leading_fence()
    |> String.replace(~r/```\s*$/, "")
    |> String.trim()
  end

  defp strip_leading_fence(text), do: String.replace(text, ~r/^(```|~~~)[a-zA-Z0-9_-]*\s*\n?/, "")

  # Slice from the earliest opener to the latest closer, so a top-level object
  # whose inner `}` precedes a nested `]` is not truncated.
  defp slice_json(text) do
    first = min_index(text, ["[", "{"])
    last = max_index(text, ["]", "}"])

    if first != nil and last != nil and last > first,
      do: binary_part(text, first, last - first + 1),
      else: nil
  end

  defp min_index(text, needles) do
    case needles |> Enum.map(&first_index(text, &1)) |> Enum.reject(&is_nil/1) do
      [] -> nil
      indexes -> Enum.min(indexes)
    end
  end

  defp max_index(text, needles) do
    case needles |> Enum.map(&last_index(text, &1)) |> Enum.reject(&is_nil/1) do
      [] -> nil
      indexes -> Enum.max(indexes)
    end
  end

  defp first_index(text, needle) do
    case :binary.match(text, needle) do
      {pos, _len} -> pos
      :nomatch -> nil
    end
  end

  defp last_index(text, needle) do
    case :binary.matches(text, needle) do
      [] -> nil
      matches -> matches |> List.last() |> elem(0)
    end
  end
end
