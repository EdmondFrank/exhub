defmodule Exhub.Toonflow.Events do
  @moduledoc """
  Chapter event-graph extraction.

  For each chapter, asks the LLM (templates `events.system` / `events.user`)
  for a structured event list, validates the JSON (`parse_events/1`, pure) and
  stores it in the `events` table. Extraction is idempotent per chapter:
  previous events for that chapter are replaced.
  """

  alias Exhub.Toonflow
  alias Exhub.Toonflow.{DB, Json, LLM, Novel, Prompts, Schema, Store}

  @raw_preview 400

  @doc """
  Extract events for one chapter (`:chapter_id`) or all chapters, newest-first
  order preserved. `opts`: `:chapter_id`, `:limit` (max chapters).

  Returns `{:ok, summary}` with per-chapter results and any per-chapter errors
  collected, or `{:error, :no_chapters}`.
  """
  @spec extract_events(String.t(), keyword(), GenServer.server()) ::
          {:ok, map()} | {:error, term()}
  def extract_events(project, opts \\ [], server \\ Store) do
    chapter_id = Toonflow.blank(Keyword.get(opts, :chapter_id))
    limit = Keyword.get(opts, :limit)

    with {:ok, chapters} <- Novel.chapters_for(project, chapter_id, server) do
      case Toonflow.maybe_limit(chapters, limit) do
        [] ->
          {:error, :no_chapters}

        chapters ->
          chapters
          |> Enum.map(&extract_for(project, &1, server))
          |> summarize()
      end
    end
  end

  @doc "List events. `opts`: `:chapter_id`, `:kind`."
  @spec list_events(String.t(), keyword(), GenServer.server()) ::
          {:ok, [map()]} | {:error, term()}
  def list_events(project, opts \\ [], server \\ Store) do
    chapter_id = Toonflow.blank(Keyword.get(opts, :chapter_id))
    kind = Toonflow.blank(Keyword.get(opts, :kind))
    {where, params} = filters(chapter_id, kind)

    sql = "SELECT #{Schema.event_columns()} FROM events" <> where <> " ORDER BY chapter_id, idx"

    Store.run_project(
      project,
      fn conn ->
        case DB.query(conn, sql, params) do
          {:ok, rows} -> {:ok, Enum.map(rows, &Schema.decode_event/1)}
          {:error, reason} -> {:error, reason}
        end
      end,
      server
    )
  end

  @doc """
  Parse an LLM response into normalized event maps, tolerating Markdown code
  fences and a surrounding `{"events": [...]}` object.

  Each event is `%{"kind" => ..., "summary" => ..., "payload" => map}`.
  """
  @spec parse_events(String.t()) :: {:ok, [map()]} | {:error, term()}
  def parse_events(raw) when is_binary(raw) do
    with {:ok, decoded} <- Json.decode(raw),
         {:ok, list} <- extract_list(decoded) do
      {:ok, Enum.map(list, &normalize_event/1)}
    end
  end

  def parse_events(_), do: {:error, :invalid_payload}

  # --- extraction ---

  defp extract_for(project, chapter, server) do
    with {:ok, system} <- Prompts.render("events.system", %{}),
         {:ok, user} <-
           Prompts.render("events.user", %{
             "title" => chapter["title"] || "",
             "text" => chapter["text"] || ""
           }),
         {:ok, raw} <- LLM.call_llm(system, user, []) do
      case parse_events(raw) do
        {:ok, events} -> persist(project, chapter, events, server)
        {:error, reason} -> {:error, error_for(chapter, "parse_failed: #{inspect(reason)}", raw)}
      end
    else
      {:error, reason} -> {:error, error_for(chapter, inspect(reason), nil)}
    end
  end

  defp persist(project, chapter, events, server) do
    result =
      Store.run_project(
        project,
        fn conn ->
          DB.transaction(conn, fn ->
            with :ok <-
                   DB.execute(conn, "DELETE FROM events WHERE chapter_id = ?", [chapter["id"]]) do
              events
              |> Enum.with_index()
              |> Enum.reduce_while({:ok, 0}, fn {event, idx}, {:ok, n} ->
                case insert_event(conn, chapter["id"], idx, event) do
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
        {:ok, %{"chapter_id" => chapter["id"], "title" => chapter["title"], "events" => count}}

      {:error, reason} ->
        {:error, error_for(chapter, inspect(reason), nil)}
    end
  end

  defp insert_event(conn, chapter_id, idx, event) do
    DB.execute(
      conn,
      "INSERT INTO events (id, chapter_id, idx, kind, summary, payload_json) VALUES (?, ?, ?, ?, ?, ?)",
      [
        Toonflow.new_id("evt"),
        chapter_id,
        idx,
        event["kind"],
        event["summary"],
        Schema.encode_json(event["payload"])
      ]
    )
  end

  defp error_for(chapter, reason, raw) do
    base = %{"chapter_id" => chapter["id"], "title" => chapter["title"], "reason" => reason}
    if raw, do: Map.put(base, "raw", String.slice(raw, 0, @raw_preview)), else: base
  end

  defp summarize(results) do
    oks = for {:ok, result} <- results, do: result
    errors = for {:error, result} <- results, do: result

    {:ok,
     %{
       "chapters_extracted" => length(oks),
       "event_count" => Enum.sum(Enum.map(oks, & &1["events"])),
       "chapters" => oks,
       "errors" => errors
     }}
  end

  defp filters(nil, nil), do: {"", []}
  defp filters(chapter_id, nil), do: {" WHERE chapter_id = ?", [chapter_id]}
  defp filters(nil, kind), do: {" WHERE kind = ?", [kind]}
  defp filters(chapter_id, kind), do: {" WHERE chapter_id = ? AND kind = ?", [chapter_id, kind]}

  # --- JSON parsing (pure) ---

  defp extract_list(%{"events" => list}) when is_list(list), do: {:ok, list}
  defp extract_list(%{"events" => %{"events" => list}}) when is_list(list), do: {:ok, list}
  defp extract_list(list) when is_list(list), do: {:ok, list}
  defp extract_list(_), do: {:error, :missing_events}

  defp normalize_event(event) when is_map(event) do
    %{
      "kind" => to_text(event["kind"] || event["type"]) || "event",
      "summary" => to_text(event["summary"] || event["description"]) || "",
      "payload" => Map.drop(event, ["kind", "type", "summary", "description"])
    }
  end

  defp normalize_event(other) do
    %{"kind" => "event", "summary" => to_text(other) || "", "payload" => %{"raw" => other}}
  end

  defp to_text(nil), do: nil
  defp to_text(value) when is_binary(value), do: value
  defp to_text(value), do: to_string(value)
end
