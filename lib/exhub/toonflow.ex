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
end
