defmodule Exhub.Memory.Frontmatter do
  @moduledoc """
  Frontmatter codec for memory notes.

  Memory notes live in the Brain (Obsidian) vault, so their metadata must be
  both Obsidian-friendly and cheap to parse without a YAML dependency. This
  module encodes/decodes the small, fixed shape used by `Exhub.Memory.Store`:

    * plain scalars are written as `key: value`
    * string lists (e.g. tags) as `key: [a, b]`
    * maps and nested structures as inline JSON (`{"k": v}`)

  On decode, `[...]` is treated as JSON when it contains quotes (otherwise a
  comma list), `{...}` and quoted values are decoded as JSON, and `true` /
  `false` / numeric scalars recover their types. Values that would be
  ambiguous on the way back are JSON-quoted by `encode/1`.
  """

  @spec encode(map()) :: String.t()
  def encode(meta) when is_map(meta) do
    meta
    |> Enum.reject(fn {_k, v} -> is_nil(v) or v == "" or v == [] end)
    |> Enum.map(fn {k, v} -> "#{k}: #{encode_value(v)}" end)
    |> Enum.join("\n")
  end

  @doc "Split a note into `{metadata_map, body}`. No frontmatter yields `{%{}, body}`."
  @spec decode(String.t()) :: {map(), String.t()}
  def decode(content) when is_binary(content) do
    case Regex.run(~r/\A---\n(.*?)\n---\n?(.*)\z/s, content) do
      [_, frontmatter, body] -> {parse_fields(frontmatter), strip_leading_newline(body)}
      _ -> {%{}, content}
    end
  end

  # ── encoding ─────────────────────────────────────────────────────────────

  defp encode_value(v) when is_binary(v) do
    if String.contains?(v, "\n") or ambiguous?(v), do: Jason.encode!(v), else: v
  end

  defp encode_value(v) when is_list(v) do
    if v != [] and Enum.all?(v, &is_binary/1) do
      "[" <> Enum.join(v, ", ") <> "]"
    else
      Jason.encode!(v)
    end
  end

  defp encode_value(v) when is_map(v), do: Jason.encode!(v)
  defp encode_value(v) when is_boolean(v) or is_number(v), do: to_string(v)
  defp encode_value(v), do: to_string(v)

  defp ambiguous?(v) do
    trimmed = String.trim(v)

    String.starts_with?(trimmed, ["[", "{", "\""]) or trimmed in ["true", "false"] or
      match?({_, ""}, Integer.parse(trimmed)) or match?({_, ""}, Float.parse(trimmed))
  end

  # ── decoding ─────────────────────────────────────────────────────────────

  defp strip_leading_newline("\n" <> rest), do: rest
  defp strip_leading_newline(body), do: body

  defp parse_fields(frontmatter) do
    frontmatter
    |> String.split("\n")
    |> Enum.reduce(%{}, fn line, acc ->
      case String.split(line, ":", parts: 2) do
        [key, value] -> Map.put(acc, String.trim(key), decode_value(String.trim(value)))
        _ -> acc
      end
    end)
  end

  defp decode_value(""), do: nil

  defp decode_value("[" <> _ = v) do
    if String.contains?(v, "\"") do
      case Jason.decode(v) do
        {:ok, decoded} -> decoded
        _ -> split_list(v)
      end
    else
      split_list(v)
    end
  end

  defp decode_value("{" <> _ = v), do: json_or(v)
  defp decode_value("\"" <> _ = v), do: json_or(v)
  defp decode_value("true"), do: true
  defp decode_value("false"), do: false

  defp decode_value(v) do
    case Integer.parse(v) do
      {i, ""} ->
        i

      _ ->
        case Float.parse(v) do
          {f, ""} -> f
          _ -> v
        end
    end
  end

  defp json_or(v) do
    case Jason.decode(v) do
      {:ok, decoded} -> decoded
      _ -> v
    end
  end

  defp split_list(v) do
    v
    |> String.trim_leading("[")
    |> String.trim_trailing("]")
    |> String.split(",", trim: true)
    |> Enum.map(fn item -> item |> String.trim() |> String.trim("\"") end)
    |> Enum.reject(&(&1 == ""))
  end
end
