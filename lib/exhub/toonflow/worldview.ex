defmodule Exhub.Toonflow.Worldview do
  @moduledoc """
  Project worldview anchors (Phase 8).

  A *worldview* note records the fixed production anchors of a drama — the map,
  region, period, art style, music style — so that every stage keeps generating
  inside the same world. Notes are `memory_notes` rows of kind `"worldview"`
  (one per title, replaced on re-write).

  Unlike project memory, the worldview is **always** injected: `append/3` folds
  the digest into the script / assets / storyboard instructions on every run
  instead of being recalled by semantic similarity.

  `digest/1` is pure and unit-tested.
  """

  alias Exhub.Toonflow
  alias Exhub.Toonflow.{DB, Memory, Schema, Store}

  @kind "worldview"
  @default_title "世界观"

  @doc """
  Store (or replace) a worldview note. `opts`: `:text` (required), `:title`
  (default `"世界观"`), `:meta`.
  """
  @spec set(String.t(), keyword(), GenServer.server()) :: {:ok, map()} | {:error, term()}
  def set(project, opts \\ [], server \\ Store) do
    text = Toonflow.blank(Keyword.get(opts, :text))
    title = Toonflow.blank(Keyword.get(opts, :title)) || @default_title
    meta = Keyword.get(opts, :meta) || %{}

    if is_nil(text) do
      {:error, :missing_text}
    else
      id = Toonflow.new_id("mem")
      now = Toonflow.now_iso()

      result =
        Store.run_project(
          project,
          fn conn ->
            with {:ok, existing} <- find_id(conn, title) do
              case existing do
                nil ->
                  DB.execute(
                    conn,
                    "INSERT INTO memory_notes (id, kind, title, text, meta_json, created_at) VALUES (?, ?, ?, ?, ?, ?)",
                    [id, @kind, title, text, Schema.encode_json(meta), now]
                  )

                found ->
                  DB.execute(
                    conn,
                    "UPDATE memory_notes SET text = ?, meta_json = ? WHERE id = ?",
                    [text, Schema.encode_json(meta), found]
                  )
              end
            end
          end,
          server
        )

      case result do
        :ok -> {:ok, %{"kind" => @kind, "title" => title, "text" => text}}
        {:error, reason} -> {:error, reason}
      end
    end
  end

  @doc "The project's worldview notes. `opts`: `:title` (exact match), `:limit`."
  @spec get(String.t(), keyword(), GenServer.server()) :: {:ok, [map()]} | {:error, term()}
  def get(project, opts \\ [], server \\ Store) do
    title = Toonflow.blank(Keyword.get(opts, :title))
    limit = Keyword.get(opts, :limit)

    with {:ok, notes} <- Memory.list_notes(project, [kind: @kind], server) do
      notes =
        notes
        |> then(fn notes ->
          if title, do: Enum.filter(notes, &(&1["title"] == title)), else: notes
        end)
        |> Toonflow.maybe_limit(limit)

      {:ok, notes}
    end
  end

  @doc """
  A prompt fragment listing the worldview anchors. Pure; `""` when there are
  none (so `append/3` can skip it entirely).
  """
  @spec digest([map()]) :: String.t()
  def digest(notes) when is_list(notes) do
    items =
      notes
      |> Enum.map(fn note -> {note["title"] || @default_title, Toonflow.blank(note["text"])} end)
      |> Enum.reject(fn {_title, text} -> is_nil(text) end)
      |> Enum.map(fn {title, text} -> "- #{title}：#{Toonflow.preview(text, 400)}" end)

    case items do
      [] -> ""
      items -> "（世界观设定，必须保持一致）\n" <> Enum.join(items, "\n")
    end
  end

  @doc "The project's worldview digest, or `{:ok, \"\"}` when there is none."
  @spec digest_for(String.t(), GenServer.server()) :: {:ok, String.t()}
  def digest_for(project, server \\ Store) do
    case get(project, [], server) do
      {:ok, notes} -> {:ok, digest(notes)}
      {:error, _reason} -> {:ok, ""}
    end
  end

  @doc """
  Append the worldview digest to a stage's instructions. Never fatal: a lookup
  failure leaves the instructions untouched.
  """
  @spec append(String.t(), String.t() | nil, GenServer.server()) :: String.t()
  def append(project, instructions, server \\ Store) do
    case digest_for(project, server) do
      {:ok, ""} -> instructions || ""
      {:ok, digest} when instructions in [nil, ""] -> digest
      {:ok, digest} -> instructions <> "\n\n" <> digest
    end
  end

  @doc "The `memory_notes.kind` used for worldview anchors."
  @spec kind() :: String.t()
  def kind, do: @kind

  # --- internals ---

  defp find_id(conn, title) do
    sql = "SELECT id FROM memory_notes WHERE kind = ? AND title = ? LIMIT 1"

    case DB.query_one(conn, sql, [@kind, title]) do
      {:ok, [id]} -> {:ok, id}
      {:ok, nil} -> {:ok, nil}
      {:error, reason} -> {:error, reason}
    end
  end
end
