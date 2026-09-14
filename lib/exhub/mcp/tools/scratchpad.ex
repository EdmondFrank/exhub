defmodule Exhub.MCP.Tools.Scratchpad do
  @moduledoc """
  Shared per-session scratchpad state for the `think` and `plan` tools.

  Inspired by the external-scratchpad experiment in which the think tool
  accumulates notes across calls and returns them to the model as consolidated
  working memory, instead of echoing the single thought back verbatim.

  Entries live in an external, session-keyed store
  (`Exhub.MCP.ScratchpadStore`) rather than the MCP frame's `assigns`. The frame
  is unreliable for persistence because ExHub serves `tools/call` through
  `Exhub.MCP.ConcurrentToolDispatcher`, which builds a fresh frame per request
  and discards the one returned by the tool. Keying on the transport-independent
  `frame.context.session_id` makes accumulation work under both the concurrent
  dispatcher and `Anubis.Server.Session`.

  ## Guarantees

    * each entry is truncated to `@max_entry_length` characters,
    * at most `@max_entries` entries are retained (oldest dropped first),
    * non-string input never crashes — it is normalized or replaced with a
      placeholder note, mirroring the reference implementation's tolerant
      argument handling.
  """

  alias Exhub.MCP.ScratchpadStore

  @max_entry_length 2_000

  @doc """
  Append `entry` to the scratchpad bucket identified by `session_id` and `key`.

  Returns the updated, bounded list of entries. The append is atomic within the
  store, so concurrent calls against the same session don't lose updates.
  """
  @spec append(String.t() | nil, atom(), String.t()) :: [String.t()]
  def append(session_id, key, entry) when is_atom(key) and is_binary(entry) do
    ScratchpadStore.append(store_name(), session_id, key, truncate(entry))
  end

  @doc "Read the current entries for `session_id` / `key`."
  @spec entries(String.t() | nil, atom()) :: [String.t()]
  def entries(session_id, key) when is_atom(key) do
    ScratchpadStore.entries(store_name(), session_id, key)
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

  # Store process name, overridable via app env so tests can point at a locally
  # started instance without relying on the supervision tree.
  defp store_name do
    Application.get_env(:exhub, __MODULE__, [])[:store] || ScratchpadStore
  end
end
