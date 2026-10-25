defmodule Exhub.Toonflow do
  @moduledoc """
  Shared helpers for the `Exhub.Toonflow` subsystem (AI short-drama pipeline).

  See `docs/plans/2026-09-30-toonflow-design.md` for the design and
  `docs/modules/toonflow.md` for the module reference.
  """

  @doc """
  Generate a prefixed random id, e.g. `new_id("chp")` -> `"chp_1a2b3c4d5e6f"`.
  """
  @spec new_id(String.t()) :: String.t()
  def new_id(prefix),
    do: prefix <> "_" <> Base.encode16(:crypto.strong_rand_bytes(6), case: :lower)

  @doc """
  Sanitize `value` into a filename-safe key. Pure.

  Characters outside `[A-Za-z0-9._-]` become `_`, which is safe for ASCII but
  useless as a discriminator for non-ASCII text: `老盐` and `小铃` both reduce to
  `_______`, so one character's reference sheet silently overwrote the other's
  (found by generating both in one project). Any key that keeps a non-ASCII
  character therefore gets a short digest of the original appended, while an
  ASCII-only key is returned verbatim — shot ids and asset prefixes keep their
  readable names.
  """
  @spec file_key(term()) :: String.t()
  def file_key(value) do
    text = to_string(value)
    sanitized = String.replace(text, ~r/[^A-Za-z0-9._-]/, "_")

    if ascii?(text), do: sanitized, else: sanitized <> "_" <> short_digest(text)
  end

  defp ascii?(text), do: Regex.match?(~r/^[\x20-\x7E]*$/, text)

  defp short_digest(text) do
    text |> :erlang.md5() |> Base.encode16(case: :lower) |> binary_part(0, 6)
  end

  @doc "Current UTC time as ISO-8601, second precision."
  @spec now_iso() :: String.t()
  def now_iso do
    DateTime.utc_now() |> DateTime.truncate(:second) |> DateTime.to_iso8601()
  end

  @doc "Normalize a value: blank strings become `nil`; other values pass through."
  @spec blank(term()) :: term()
  def blank(nil), do: nil
  def blank(value) when is_binary(value), do: if(String.trim(value) == "", do: nil, else: value)
  def blank(value), do: value

  @doc "A single-line preview of `text`, at most `n` characters."
  @spec preview(String.t() | nil, non_neg_integer()) :: String.t()
  def preview(nil, _n), do: ""

  def preview(text, n) when is_binary(text) do
    text
    |> String.replace(~r/\s+/, " ")
    |> String.trim()
    |> String.slice(0, n)
  end

  @doc "Take at most `limit` items when `limit` is a positive integer."
  @spec maybe_limit(list(), term()) :: list()
  def maybe_limit(list, limit) when is_integer(limit) and limit > 0, do: Enum.take(list, limit)
  def maybe_limit(list, _), do: list

  @doc """
  Reduce `value` to a shape that JSON-encodes.

  Tool payloads reach the wire through `Anubis.Server.Response.structured/2`,
  which is `JSON.encode!/1`, and progress events go out over the websocket —
  both encode whatever the pipeline reports. Elixir's `JSON.Encoder` implements
  no `Tuple` at all, and nothing for structs, so a reason term like
  `{:image_failed, :boom}` or a `%LangChain.LangChainError{}` (an LLM transport
  timeout) would otherwise take the whole response down with a
  `Protocol.UndefinedError`.

  Binaries, atoms, numbers and booleans pass through; lists and maps are recursed
  into (non-atom/binary keys are inspected); anything else — a tuple, a struct,
  a pid — becomes its `inspect` form.
  """
  @spec json_safe(term()) :: term()
  def json_safe(nil), do: nil

  def json_safe(value)
      when is_binary(value) or is_number(value) or is_boolean(value) or is_atom(value),
      do: value

  def json_safe(value) when is_list(value), do: Enum.map(value, &json_safe/1)

  def json_safe(%_{} = value), do: inspect(value)

  def json_safe(value) when is_map(value),
    do: Map.new(value, fn {key, val} -> {json_key(key), json_safe(val)} end)

  def json_safe(value), do: inspect(value)

  defp json_key(key) when is_binary(key) or is_atom(key) or is_number(key), do: key
  defp json_key(key), do: inspect(key)
end
