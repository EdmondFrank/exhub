defmodule Exhub.LspBridge.Document do
  @moduledoc """
  Per-buffer document state and LSP notification shaping.

  Elixir port of the content half of `core/fileaction.py` plus the
  `send_did_*_notification` builders in `core/lspserver.py`. A `Document` is a
  plain struct held in `Exhub.LspBridge.Session` state (not a process): Emacs
  is authoritative for content, and the backend mirrors it so it can answer
  full-sync servers, compute pull-diagnostic versions and render diagnostics.

  Emacs sends incremental edits (range + text, LSP UTF-16 positions);
  `apply_change/2` mirrors them onto the cached content. `did_change_params/3`
  emits incremental changes for incremental-sync servers and whole text for
  full-sync servers.
  """

  @enforce_keys [:filepath, :uri, :language_id]
  defstruct filepath: nil,
            uri: nil,
            language_id: nil,
            version: 1,
            content: "",
            servers: [],
            diagnostics: %{},
            last_change: nil

  @type t :: %__MODULE__{}

  @doc "Build a document; `version` starts at 1 (the first change's version)."
  @spec new(String.t(), String.t() | nil, String.t()) :: t()
  def new(filepath, content, language_id) do
    filepath = Path.expand(filepath)

    %__MODULE__{
      filepath: filepath,
      uri: uri(filepath),
      language_id: language_id,
      content: content || ""
    }
  end

  @doc "The `file://` URI for an absolute path."
  @spec uri(String.t()) :: String.t()
  def uri(filepath), do: "file://" <> Path.expand(filepath)

  @doc "Inverse of `uri/1`."
  @spec path_from_uri(String.t()) :: String.t()
  def path_from_uri("file://" <> path), do: path
  def path_from_uri(uri), do: uri

  # ===========================================================================
  # Notification params
  # ===========================================================================

  @doc "`textDocument/didOpen` params (initial version 0)."
  @spec did_open_params(t()) :: map()
  def did_open_params(%__MODULE__{} = d) do
    %{
      "textDocument" => %{
        "uri" => d.uri,
        "languageId" => d.language_id,
        "version" => 0,
        "text" => d.content
      }
    }
  end

  @doc """
  `textDocument/didChange` params.

  `change` is the decoded Emacs edit: `%{\"range\" => ..., \"rangeLength\" => n,
  \"text\" => t}`. For full-sync servers (`sync_kind == 1`) the whole cached
  content is sent instead, as the protocol requires.
  """
  @spec did_change_params(t(), map(), non_neg_integer() | nil) :: map()
  def did_change_params(%__MODULE__{} = d, change, sync_kind) do
    content_change =
      case sync_kind do
        1 -> %{"text" => d.content}
        _ -> Map.take(change, ["range", "rangeLength", "text"])
      end

    %{
      "textDocument" => %{"uri" => d.uri, "version" => d.version},
      "contentChanges" => [content_change]
    }
  end

  @doc "`textDocument/didSave` params, optionally carrying the whole text."
  @spec did_save_params(t(), boolean()) :: map()
  def did_save_params(%__MODULE__{} = d, include_text?) do
    text_document = %{"uri" => d.uri}

    text_document =
      if include_text?, do: Map.put(text_document, "text", d.content), else: text_document

    %{"textDocument" => text_document}
  end

  @doc "`textDocument/didClose` params."
  @spec did_close_params(t()) :: map()
  def did_close_params(%__MODULE__{} = d) do
    %{"textDocument" => %{"uri" => d.uri}}
  end

  # ===========================================================================
  # Content mirroring
  # ===========================================================================

  @doc "Apply an incremental change to the document, returning the updated struct."
  @spec apply_change(t(), map()) :: t()
  def apply_change(%__MODULE__{} = d, change) when is_map(change) do
    %{d | content: apply_change_text(d.content, change), last_change: System.monotonic_time()}
  end

  def apply_change(%__MODULE__{} = d, _change), do: d

  @doc "Apply an incremental change to a content string."
  @spec apply_change_text(String.t(), map()) :: String.t()
  def apply_change_text(content, %{"range" => %{"start" => start_pos, "end" => end_pos}} = change) do
    replace_range(content, start_pos, end_pos, Map.get(change, "text", ""))
  end

  def apply_change_text(_content, %{"text" => text}), do: text
  def apply_change_text(content, _change), do: content

  defp replace_range(content, start_pos, end_pos, text) do
    size = byte_size(content)
    start_offset = offset(content, start_pos) |> min(size)
    end_offset = offset(content, end_pos) |> min(size)

    if start_offset <= end_offset do
      binary_part(content, 0, start_offset) <>
        text <> binary_part(content, end_offset, size - end_offset)
    else
      content
    end
  end

  # (line, character-in-UTF-16-code-units) -> byte offset.
  defp offset(content, %{"line" => line, "character" => character})
       when is_integer(line) and is_integer(character) do
    lines = String.split(content, "\n")

    prefix =
      lines
      |> Enum.take(max(line, 0))
      |> Enum.reduce(0, fn l, acc -> acc + byte_size(l) + 1 end)

    prefix + utf16_to_bytes(Enum.at(lines, line, ""), character)
  end

  defp offset(_content, _position), do: 0

  defp utf16_to_bytes(str, units) when is_integer(units) and units > 0 do
    str
    |> String.graphemes()
    |> Enum.reduce_while({0, 0}, fn grapheme, {bytes, counted} ->
      len = utf16_len(grapheme)

      if counted + len > units do
        {:halt, {bytes, counted}}
      else
        {:cont, {bytes + byte_size(grapheme), counted + len}}
      end
    end)
    |> elem(0)
  end

  defp utf16_to_bytes(_str, _units), do: 0

  defp utf16_len(grapheme) do
    grapheme
    |> String.to_charlist()
    |> Enum.reduce(0, fn cp, acc -> acc + if cp > 0xFFFF, do: 2, else: 1 end)
  end
end
