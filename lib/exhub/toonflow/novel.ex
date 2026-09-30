defmodule Exhub.Toonflow.Novel do
  @moduledoc """
  Novel ingestion for Toonflow.

  Loads source text (plain `.txt`/`.md` read directly; PDF/DOCX/images via
  `Exhub.MCP.Tools.DocExtract`), splits it into chapters with heading
  heuristics, and persists the novel plus its chapters into the project database
  (`index.db`), mirroring each chapter to `chapters/<idx>-<slug>.txt`.

  `split_chapters/1` is pure and unit-tested.
  """

  require Logger

  alias Exhub.Toonflow
  alias Exhub.Toonflow.{DB, Schema, Store, Workspace}

  @doc_formats ~w(.pdf .docx .doc .png .jpg .jpeg .tiff .bmp .gif .webp)
  @preview_chars 240

  @heading_regexes [
    ~r/^\s*第\s*[0-9０-９零一二三四五六七八九十百千万两]+\s*[章回节篇]/u,
    ~r/^\s*第\s*[0-9０-９零一二三四五六七八九十百千万两]+\s*卷/u,
    ~r/^\s*[Cc]hapter\s+\d+\b/,
    ~r/^\s*[#]{1,4}\s+\S/
  ]

  @doc """
  Ingest a novel into `project`.

  `opts` must contain either `:path` (a local file) or `:text`; `:title` is
  optional. Returns `{:ok, summary}` with the novel id, chapter count and
  per-chapter metadata, or `{:error, reason}`.
  """
  @spec add_novel(String.t(), keyword(), GenServer.server()) :: {:ok, map()} | {:error, term()}
  def add_novel(project, opts \\ [], server \\ Store) do
    with {:ok, meta} <- Store.get_project(project, server),
         {:ok, title, text, source} <- load_text(opts) do
      case split_chapters(text) do
        [] -> {:error, :empty_novel}
        chapters -> persist(meta, title, source, text, chapters, server)
      end
    end
  end

  @doc "List chapters as lightweight summaries (id, idx, title, length, preview, summary)."
  @spec list_chapters(String.t(), keyword(), GenServer.server()) ::
          {:ok, [map()]} | {:error, term()}
  def list_chapters(project, opts \\ [], server \\ Store) do
    novel_id = Toonflow.blank(Keyword.get(opts, :novel_id))
    limit = Keyword.get(opts, :limit)
    {where, params} = if novel_id, do: {" WHERE novel_id = ?", [novel_id]}, else: {"", []}

    sql =
      "SELECT id, novel_id, idx, title, length(text), summary FROM chapters" <>
        where <> " ORDER BY idx"

    Store.run_project(
      project,
      fn conn ->
        case DB.query(conn, sql, params) do
          {:ok, rows} ->
            rows = rows |> Enum.map(&decode_chapter_summary/1) |> Toonflow.maybe_limit(limit)
            {:ok, rows}

          {:error, reason} ->
            {:error, reason}
        end
      end,
      server
    )
  end

  @doc "Fetch full chapter rows (including `text`) for one chapter id, or all chapters in order."
  @spec chapters_for(String.t(), String.t() | nil, GenServer.server()) ::
          {:ok, [map()]} | {:error, term()}
  def chapters_for(project, chapter_id \\ nil, server \\ Store) do
    {sql, params} =
      if chapter_id do
        {"SELECT #{Schema.chapter_columns()} FROM chapters WHERE id = ?", [chapter_id]}
      else
        {"SELECT #{Schema.chapter_columns()} FROM chapters ORDER BY idx", []}
      end

    Store.run_project(
      project,
      fn conn ->
        case DB.query(conn, sql, params) do
          {:ok, rows} -> {:ok, Enum.map(rows, &Schema.decode_chapter/1)}
          {:error, reason} -> {:error, reason}
        end
      end,
      server
    )
  end

  @doc """
  Split raw novel `text` into chapters using heading heuristics
  (`第N章/回/节/篇/卷`, `Chapter N`, or Markdown `#`..`####` headings).

  Returns `[%{"title" => title, "text" => body}]`. Text without detectable
  headings becomes a single `"全文"` chapter; blank text yields `[]`.
  """
  @spec split_chapters(String.t()) :: [map()]
  def split_chapters(text) when is_binary(text) do
    if String.trim(text) == "" do
      []
    else
      text
      |> String.split(~r/\r\n|\r|\n/)
      |> Enum.reduce({[], nil}, &accumulate_line/2)
      |> close_current()
      |> normalize_chapters()
    end
  end

  # --- ingestion ---

  defp load_text(opts) do
    path = Toonflow.blank(Keyword.get(opts, :path))
    text = Toonflow.blank(Keyword.get(opts, :text))
    title = Toonflow.blank(Keyword.get(opts, :title))

    cond do
      path ->
        title = title || Path.basename(path, Path.extname(path))
        with {:ok, body} <- read_source(path), do: {:ok, title, body, path}

      text ->
        {:ok, title || "未命名", text, nil}

      true ->
        {:error, :missing_source}
    end
  end

  defp read_source(path) do
    ext = path |> Path.extname() |> String.downcase()

    if ext in @doc_formats do
      case Exhub.MCP.Tools.DocExtract.Client.extract(path,
             output_format: "text",
             include_image: false
           ) do
        {:ok, text} -> {:ok, text}
        {:error, reason} -> {:error, {:doc_extract, reason}}
      end
    else
      case File.read(path) do
        {:ok, text} -> {:ok, text}
        {:error, reason} -> {:error, {:read_failed, reason}}
      end
    end
  end

  defp persist(meta, title, source, text, chapters, server) do
    name = meta["name"]
    dir = meta["root_dir"]
    novel_id = Toonflow.new_id("nov")
    now = Toonflow.now_iso()

    records =
      chapters
      |> Enum.with_index(1)
      |> Enum.map(fn {ch, idx} ->
        %{id: Toonflow.new_id("chp"), idx: idx, title: ch["title"], text: ch["text"]}
      end)

    result =
      Store.run_project(
        name,
        fn conn ->
          DB.transaction(conn, fn ->
            with :ok <- insert_novel(conn, novel_id, title, source, text, now) do
              Enum.reduce_while(records, {:ok, 0}, fn record, {:ok, n} ->
                case insert_chapter(conn, record, novel_id) do
                  :ok -> {:cont, {:ok, n + 1}}
                  {:error, reason} -> {:halt, {:error, reason}}
                end
              end)
            end
          end)
        end,
        server
      )

    case result do
      {:ok, count} ->
        write_chapter_files(dir, records)

        {:ok,
         %{
           "novel_id" => novel_id,
           "title" => title,
           "source_path" => source,
           "text_length" => String.length(text),
           "chapter_count" => count,
           "chapters" => Enum.map(records, &chapter_summary/1)
         }}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp insert_novel(conn, novel_id, title, source, text, now) do
    DB.execute(
      conn,
      "INSERT INTO novels (id, title, source_path, text, meta_json, created_at) VALUES (?, ?, ?, ?, ?, ?)",
      [novel_id, title, source, text, nil, now]
    )
  end

  defp insert_chapter(conn, record, novel_id) do
    DB.execute(
      conn,
      "INSERT INTO chapters (id, novel_id, idx, title, text, summary) VALUES (?, ?, ?, ?, ?, ?)",
      [record.id, novel_id, record.idx, record.title, record.text, nil]
    )
  end

  defp write_chapter_files(_dir, []), do: :ok

  defp write_chapter_files(dir, records) do
    chapters_dir = Path.join(dir, "chapters")
    File.mkdir_p(chapters_dir)

    Enum.each(records, fn record ->
      filename = "#{pad(record.idx)}-#{slug(record.title)}.txt"
      File.write(Path.join(chapters_dir, filename), record.text || "")
    end)

    :ok
  end

  defp chapter_summary(record) do
    %{
      "id" => record.id,
      "idx" => record.idx,
      "title" => record.title,
      "text_length" => String.length(record.text || ""),
      "preview" => Toonflow.preview(record.text, @preview_chars)
    }
  end

  defp decode_chapter_summary([id, novel_id, idx, title, len, summary]) do
    %{
      "id" => id,
      "novel_id" => novel_id,
      "idx" => idx,
      "title" => title,
      "text_length" => len,
      "summary" => summary
    }
  end

  # --- chapter splitting (pure) ---

  defp accumulate_line(line, {acc, cur}) do
    cond do
      chapter_heading?(line) ->
        {push_chapter(acc, cur), %{title: clean_title(line), lines: []}}

      is_nil(cur) ->
        {acc, %{title: nil, lines: [line]}}

      true ->
        {acc, %{cur | lines: [line | cur.lines]}}
    end
  end

  defp close_current({acc, cur}), do: acc |> push_chapter(cur) |> Enum.reverse()

  defp push_chapter(acc, nil), do: acc

  defp push_chapter(acc, cur) do
    body =
      cur.lines
      |> Enum.reverse()
      |> Enum.join("\n")
      |> String.trim()

    [%{"title" => cur.title || "前言", "text" => body} | acc]
  end

  defp normalize_chapters(chapters) do
    case Enum.reject(chapters, &(&1["text"] == "")) do
      [] -> chapters
      [%{"title" => "前言"} = only] -> [%{only | "title" => "全文"}]
      kept -> kept
    end
  end

  defp chapter_heading?(line), do: Enum.any?(@heading_regexes, &Regex.match?(&1, line))

  defp clean_title(line) do
    line
    |> String.trim()
    |> String.replace(~r/^[#]+\s*/, "")
    |> String.slice(0, 200)
  end

  defp pad(idx) when is_integer(idx), do: idx |> Integer.to_string() |> String.pad_leading(3, "0")
  defp pad(idx), do: to_string(idx)

  defp slug(title) do
    case Workspace.slugify(title || "") do
      "" -> "chapter"
      slug -> String.slice(slug, 0, 40)
    end
  end
end
