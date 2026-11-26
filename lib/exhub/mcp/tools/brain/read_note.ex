defmodule Exhub.MCP.Tools.Brain.ReadNote do
  @moduledoc """
  MCP Tool: brain_read_note

  Reads a note — or one Markdown section of it — from the Obsidian vault.

  Output is bounded by `limit` lines and `max_chars` characters and reports
  whether it was truncated, so an agent can page through a large note without
  overflowing its context. Text output is line-numbered; `format: "json"`
  returns the raw body plus metadata.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Brain.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "brain_read_note"

  @impl true
  def description do
    """
    Read a note from the Obsidian brain vault, or one of its Markdown sections.

    Returns line-numbered content, tags, size, modification time and — when
    `include_backlinks: true` — the number of incoming [[wikilinks]]. `offset`
    and `limit` page through long notes; `max_chars` caps the returned body.
    The response reports `truncated` so you know whether to continue.

    Examples:
    - Whole note:   { "path": "projects/todo" }
    - One section:  { "path": "designs/api.md", "heading": "Endpoints" }
    - Second page:  { "path": "journal/2026-11-19", "offset": 201, "limit": 100 }
    - Structured:   { "path": "inbox/notes.md", "format": "json" }
    """
  end

  schema do
    field(:path, {:required, :string},
      description:
        "Note path relative to the vault root (`.md` is added when the path has no extension)"
    )

    field(:heading, :string,
      description:
        "Optional Markdown heading; reads that section only (up to the next heading of the same or higher level)"
    )

    field(:offset, :integer,
      description:
        "1-based first line to return, relative to the section when `heading` is given (default: 1)",
      default: 1
    )

    field(:limit, :integer,
      description: "Maximum lines to return (default: 200)",
      default: 200
    )

    field(:max_chars, :integer,
      description: "Maximum characters of body returned (default: 20000)",
      default: 20_000
    )

    field(:include_backlinks, :boolean,
      description:
        "Also count incoming [[wikilinks]] for the note (scans the whole vault; default: false)",
      default: false
    )

    field(:format, :string,
      description: "Output format: 'text' (default), 'json', or 'toon'",
      default: "text"
    )
  end

  @impl true
  def execute(params, frame) do
    path = Map.get(params, :path)
    heading = Map.get(params, :heading)
    offset = positive(Map.get(params, :offset), 1)
    limit = positive(Map.get(params, :limit), 200)
    max_chars = positive(Map.get(params, :max_chars), 20_000)
    backlinks? = Map.get(params, :include_backlinks) == true
    format = normalize_format(Map.get(params, :format))

    vault = Helpers.vault_path()
    full_path = Path.join(vault, note_path(path))

    with :ok <- Helpers.validate_in_vault(vault, full_path),
         :ok <- ensure_note(full_path),
         {:ok, content} <- File.read(full_path),
         {:ok, section} <- select_section(String.split(content, "\n"), heading) do
      total = length(section.lines)
      start = min(offset, total + 1)
      selected = section.lines |> Enum.drop(start - 1) |> Enum.take(limit)

      first_line = section.start_line + start - 1
      last_line = first_line + length(selected) - 1

      {raw_body, chars_truncated} = selected |> Enum.join("\n") |> truncate(max_chars)

      text_body =
        if raw_body == "" do
          ""
        else
          raw_body |> String.split("\n") |> numbered(first_line)
        end

      rel = Path.relative_to(full_path, vault)
      lines_truncated = start - 1 + length(selected) < total

      payload = %{
        vault: vault,
        path: rel,
        abs_path: full_path,
        total_lines: length(String.split(content, "\n")),
        size: byte_size(content),
        mtime: iso8601(Helpers.note_mtime(full_path)),
        tags: Helpers.extract_tags(content),
        backlinks: if(backlinks?, do: backlink_count(vault, rel), else: nil),
        section: section_info(section),
        offset: start,
        limit: limit,
        first_line: first_line,
        last_line: last_line,
        returned_lines: length(selected),
        truncated: lines_truncated or chars_truncated,
        body: raw_body
      }

      resp = Response.tool() |> render(payload, format, text_body)
      {:reply, resp, frame}
    else
      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end

  # ── output ─────────────────────────────────────────────────────────────────

  defp render(resp, payload, "json", _text_body), do: Response.json(resp, payload)
  defp render(resp, payload, "toon", _text_body), do: Helpers.toon_response(resp, payload)

  defp render(resp, payload, _text, text_body) do
    Response.text(resp, header(payload) <> "\n\n" <> text_body)
  end

  defp header(payload) do
    [
      "Vault: #{payload.vault}",
      "",
      "Note: #{payload.path} — #{payload.total_lines} line(s), #{payload.size} bytes, modified #{payload.mtime || "unknown"}",
      tags_line(payload.tags),
      backlinks_line(payload.backlinks),
      section_line(payload.section),
      lines_line(payload)
    ]
    |> Enum.reject(&is_nil/1)
    |> Enum.join("\n")
  end

  defp tags_line([]), do: nil
  defp tags_line(tags), do: "Tags: #{Enum.join(tags, ", ")}"

  defp backlinks_line(nil), do: nil
  defp backlinks_line(count), do: "Backlinks: #{count}"

  defp section_line(nil), do: nil

  defp section_line(section) do
    "Section: #{section.heading} (L#{section.start_line}–L#{section.end_line})"
  end

  defp lines_line(%{returned_lines: 0} = payload) do
    "Lines: none (offset #{payload.offset} is past the end of the note)"
  end

  defp lines_line(payload) do
    hint =
      if payload.truncated,
        do: " (truncated; continue with offset=#{payload.offset + payload.returned_lines})",
        else: ""

    "Lines: L#{payload.first_line}–L#{payload.last_line} of #{payload.total_lines}#{hint}"
  end

  defp numbered(lines, first_line) do
    lines
    |> Enum.with_index(first_line)
    |> Enum.map_join("\n", fn {text, line} -> "L#{line}: #{text}" end)
  end

  defp truncate(text, max_chars) when byte_size(text) <= max_chars, do: {text, false}

  defp truncate(text, max_chars) do
    {text |> String.split("\n") |> keep_chars(max_chars, [], 0) |> Enum.join("\n"), true}
  end

  # Keep whole lines while the running length stays within the budget, so the
  # returned body never ends mid-line (line numbers stay meaningful).
  defp keep_chars([], _budget, acc, _used), do: Enum.reverse(acc)

  defp keep_chars([line | rest], budget, acc, used) do
    size = byte_size(line) + 1

    if used + size <= budget do
      keep_chars(rest, budget, [line | acc], used + size)
    else
      Enum.reverse(acc)
    end
  end

  # ── note access ────────────────────────────────────────────────────────────

  defp note_path(path) do
    if Path.extname(path) == "", do: path <> ".md", else: path
  end

  defp ensure_note(path) do
    cond do
      File.dir?(path) -> {:error, "Path is a directory, not a note: #{path}"}
      File.exists?(path) -> :ok
      true -> {:error, "Note not found: #{path}"}
    end
  end

  defp backlink_count(vault, rel) do
    vault
    |> Helpers.list_md_files(vault)
    |> then(&Helpers.count_backlinks(vault, &1))
    |> Map.get(rel, 0)
  end

  # ── section selection ──────────────────────────────────────────────────────

  defp select_section(lines, nil) do
    {:ok, %{heading: nil, level: 0, start_line: 1, lines: lines}}
  end

  defp select_section(lines, heading) do
    wanted = normalize_heading(heading)
    indexed = Enum.with_index(lines, 1)

    case Enum.find(indexed, fn {line, _idx} -> match_heading?(line, wanted) end) do
      nil ->
        {:error, "Heading not found: #{heading}"}

      {line, start_line} ->
        {level, _title} = heading_info(line)

        {:ok,
         %{
           heading: heading,
           level: level,
           start_line: start_line,
           lines:
             Enum.slice(
               lines,
               start_line - 1,
               section_end(indexed, start_line, level) - start_line + 1
             )
         }}
    end
  end

  defp section_end(indexed, start_line, level) do
    indexed
    |> Enum.drop_while(fn {_line, idx} -> idx <= start_line end)
    |> Enum.find_value(fn {line, idx} ->
      case heading_info(line) do
        {nested_level, _} when nested_level <= level -> idx - 1
        _ -> nil
      end
    end) || length(indexed)
  end

  defp match_heading?(line, wanted) do
    case heading_info(line) do
      {_level, title} -> title == wanted
      nil -> false
    end
  end

  defp heading_info(line) do
    case Regex.run(~r/^\s{0,3}(#+)\s+(.+?)\s*#*\s*$/, line) do
      [_, hashes, title] -> {byte_size(hashes), String.downcase(String.trim(title))}
      _ -> nil
    end
  end

  defp normalize_heading(heading) do
    heading
    |> to_string()
    |> String.trim()
    |> String.trim_leading("#")
    |> String.trim()
    |> String.downcase()
  end

  defp section_info(%{heading: nil}), do: nil

  defp section_info(section) do
    %{
      heading: section.heading,
      level: section.level,
      start_line: section.start_line,
      end_line: section.start_line + length(section.lines) - 1
    }
  end

  # ── misc ───────────────────────────────────────────────────────────────────

  defp positive(value, _default) when is_integer(value) and value > 0, do: value
  defp positive(_value, default), do: default

  defp normalize_format(format) when format in ["json", "toon"], do: format
  defp normalize_format(_format), do: "text"

  defp iso8601(nil), do: nil
  defp iso8601(%DateTime{} = datetime), do: DateTime.to_iso8601(datetime)
end
