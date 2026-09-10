defmodule Exhub.MCP.Tools.Scratchpad do
  @moduledoc """
  Shared per-session scratchpad state for the `think` and `plan` tools.

  Inspired by the external-scratchpad experiment in which the think tool
  accumulates notes across calls and returns them to the model as consolidated
  working memory, instead of echoing the single thought back verbatim.

  Entries are kept in the MCP session's frame `assigns` (see
  `Anubis.Server.Frame.assign/3`). The Anubis session process persists the
  frame returned from a component's `execute/2`, so the log survives for the
  lifetime of the session without any extra supervision tree members.

  ## Guarantees

    * each entry is truncated to `@max_entry_length` characters,
    * at most `@max_entries` entries are retained (oldest dropped first),
    * non-string input never crashes — it is normalized or replaced with a
      placeholder note, mirroring the reference implementation's tolerant
      argument handling.
  """

  alias Anubis.Server.Frame

  @max_entry_length 32_000
  @max_entries 50

  @doc """
  Appends `entry` to the list stored under `key` in `frame.assigns`.

  Returns `{entries, frame}` where `entries` is the updated, bounded list and
  `frame` carries it back into the session.
  """
  @spec append(Frame.t(), atom(), String.t()) :: {[String.t()], Frame.t()}
  def append(%Frame{} = frame, key, entry) when is_atom(key) and is_binary(entry) do
    entries =
      frame
      |> Map.get(:assigns)
      |> Map.get(key, [])
      |> Kernel.++([truncate(entry)])
      |> Enum.take(-@max_entries)

    {entries, Frame.assign(frame, key, entries)}
  end

  @doc """
  Reads the current entries for `key` from a frame.
  """
  @spec entries(Frame.t(), atom()) :: [String.t()]
  def entries(%Frame{assigns: assigns}, key) when is_atom(key) do
    Map.get(assigns, key, [])
  end

  @doc """
  Normalizes a tool argument into a storable string.

  Accepts strings; anything else becomes `placeholder` (or a readable
  representation of short scalars), so malformed calls are recorded rather
  than raising.
  """
  @spec normalize(term(), String.t()) :: String.t()
  def normalize(value, _placeholder) when is_binary(value), do: value
  def normalize(value, _placeholder) when value in [nil, ""], do: "empty"

  def normalize(value, _placeholder) when is_number(value) or is_boolean(value),
    do: to_string(value)

  def normalize(_value, placeholder), do: placeholder

  @doc """
  Builds the response envelope returned by scratchpad-backed tools.

  Mirrors the reference shape: `%{"recorded" => count, "scratchpad" =>
  entries, "next" => nudge}`. The nudge tells the model to act on what it has
  already written and to call again only with materially new state, which
  discourages redundant scratchpad round-trips.
  """
  @spec envelope([String.t()], String.t()) :: map()
  def envelope(entries, nudge) do
    %{
      "recorded" => length(entries),
      "scratchpad" => entries,
      "next" => nudge
    }
  end

  @suffix "…[truncated]"

  defp truncate(entry) when byte_size(entry) <= @max_entry_length, do: entry

  defp truncate(entry) do
    # Take codepoints (not bytes) so the result is always valid UTF-8.
    String.slice(entry, 0, @max_entry_length) <> @suffix
  end
end
