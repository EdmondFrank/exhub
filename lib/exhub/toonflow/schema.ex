defmodule Exhub.Toonflow.Schema do
  @moduledoc """
  SQL DDL and row codecs for the Toonflow SQLite stores.

  Two databases are used:

    * **registry** — `<root>/toonflow.db`: one row per project.
    * **project**  — `<root>/workspaces/<project>/index.db`: the project's own
      data (novels, chapters, events, scripts, characters, shots, assets, jobs,
      memory notes).

  Vector tables (`sqlite-vec`) are intentionally absent until Phase 4. Every
  function here is pure; `Exhub.Toonflow.Store` applies the DDL to connections.
  """

  @registry_ddl [
    """
    CREATE TABLE IF NOT EXISTS projects (
      id TEXT PRIMARY KEY,
      name TEXT NOT NULL UNIQUE,
      root_dir TEXT NOT NULL,
      meta_json TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    );
    """,
    "CREATE INDEX IF NOT EXISTS idx_projects_name ON projects(name);"
  ]

  @project_ddl [
    """
    CREATE TABLE IF NOT EXISTS novels (
      id TEXT PRIMARY KEY,
      title TEXT,
      source_path TEXT,
      text TEXT,
      meta_json TEXT,
      created_at TEXT NOT NULL
    );
    """,
    """
    CREATE TABLE IF NOT EXISTS chapters (
      id TEXT PRIMARY KEY,
      novel_id TEXT NOT NULL,
      idx INTEGER NOT NULL,
      title TEXT,
      text TEXT,
      summary TEXT
    );
    """,
    "CREATE INDEX IF NOT EXISTS idx_chapters_novel ON chapters(novel_id, idx);",
    """
    CREATE TABLE IF NOT EXISTS events (
      id TEXT PRIMARY KEY,
      chapter_id TEXT,
      idx INTEGER NOT NULL DEFAULT 0,
      kind TEXT,
      summary TEXT,
      payload_json TEXT
    );
    """,
    "CREATE INDEX IF NOT EXISTS idx_events_chapter ON events(chapter_id, idx);",
    """
    CREATE TABLE IF NOT EXISTS scripts (
      id TEXT PRIMARY KEY,
      chapter_id TEXT,
      version INTEGER NOT NULL DEFAULT 1,
      format TEXT NOT NULL DEFAULT 'markdown',
      content TEXT,
      meta_json TEXT,
      created_at TEXT NOT NULL
    );
    """,
    "CREATE INDEX IF NOT EXISTS idx_scripts_chapter ON scripts(chapter_id, version);",
    """
    CREATE TABLE IF NOT EXISTS characters (
      id TEXT PRIMARY KEY,
      name TEXT NOT NULL,
      appearance TEXT,
      refs_json TEXT,
      meta_json TEXT
    );
    """,
    "CREATE INDEX IF NOT EXISTS idx_characters_name ON characters(name);",
    """
    CREATE TABLE IF NOT EXISTS shots (
      id TEXT PRIMARY KEY,
      script_id TEXT,
      idx INTEGER NOT NULL DEFAULT 0,
      scene TEXT,
      shot_desc TEXT,
      size TEXT,
      camera TEXT,
      lighting TEXT,
      motion TEXT,
      prompt TEXT,
      meta_json TEXT
    );
    """,
    "CREATE INDEX IF NOT EXISTS idx_shots_script ON shots(script_id, idx);",
    """
    CREATE TABLE IF NOT EXISTS assets (
      id TEXT PRIMARY KEY,
      shot_id TEXT,
      character_id TEXT,
      kind TEXT NOT NULL,
      path TEXT,
      url TEXT,
      prompt TEXT,
      meta_json TEXT,
      created_at TEXT NOT NULL
    );
    """,
    "CREATE INDEX IF NOT EXISTS idx_assets_shot ON assets(shot_id, kind);",
    """
    CREATE TABLE IF NOT EXISTS jobs (
      id TEXT PRIMARY KEY,
      type TEXT NOT NULL,
      status TEXT NOT NULL,
      params_json TEXT,
      result_json TEXT,
      error TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    );
    """,
    "CREATE INDEX IF NOT EXISTS idx_jobs_status ON jobs(status, type);",
    """
    CREATE TABLE IF NOT EXISTS memory_notes (
      id TEXT PRIMARY KEY,
      kind TEXT,
      title TEXT,
      text TEXT,
      meta_json TEXT,
      created_at TEXT NOT NULL
    );
    """,
    "CREATE INDEX IF NOT EXISTS idx_memory_kind ON memory_notes(kind);"
  ]

  @project_columns "id, name, root_dir, meta_json, created_at, updated_at"
  @novel_columns "id, title, source_path, text, meta_json, created_at"
  @chapter_columns "id, novel_id, idx, title, text, summary"
  @event_columns "id, chapter_id, idx, kind, summary, payload_json"
  @script_columns "id, chapter_id, version, format, content, meta_json, created_at"
  @character_columns "id, name, appearance, refs_json, meta_json"
  @shot_columns "id, script_id, idx, scene, shot_desc, size, camera, lighting, motion, prompt, meta_json"
  @asset_columns "id, shot_id, character_id, kind, path, url, prompt, meta_json, created_at"
  @memory_note_columns "id, kind, title, text, meta_json, created_at"

  @doc "The SELECT column list for the `projects` table."
  @spec project_columns() :: String.t()
  def project_columns, do: @project_columns

  @doc "The SELECT column list for the `novels` table."
  @spec novel_columns() :: String.t()
  def novel_columns, do: @novel_columns

  @doc "The SELECT column list for the `chapters` table."
  @spec chapter_columns() :: String.t()
  def chapter_columns, do: @chapter_columns

  @doc "The SELECT column list for the `events` table."
  @spec event_columns() :: String.t()
  def event_columns, do: @event_columns

  @doc "The SELECT column list for the `scripts` table."
  @spec script_columns() :: String.t()
  def script_columns, do: @script_columns

  @doc "The SELECT column list for the `characters` table."
  @spec character_columns() :: String.t()
  def character_columns, do: @character_columns

  @doc "The SELECT column list for the `shots` table."
  @spec shot_columns() :: String.t()
  def shot_columns, do: @shot_columns

  @doc "The SELECT column list for the `assets` table."
  @spec asset_columns() :: String.t()
  def asset_columns, do: @asset_columns

  @doc "The SELECT column list for the `memory_notes` table."
  @spec memory_note_columns() :: String.t()
  def memory_note_columns, do: @memory_note_columns

  @doc "DDL for the registry database."
  @spec registry_ddl() :: [String.t()]
  def registry_ddl, do: @registry_ddl

  @doc "DDL for a project database."
  @spec project_ddl() :: [String.t()]
  def project_ddl, do: @project_ddl

  @doc "All DDL (registry + project)."
  @spec all_ddl() :: [String.t()]
  def all_ddl, do: registry_ddl() ++ project_ddl()

  @doc "Decode a `projects` row (order of `project_columns/0`) into a string-keyed map."
  @spec decode_project([term()]) :: map()
  def decode_project([id, name, root_dir, meta_json, created_at, updated_at]) do
    %{
      "id" => id,
      "name" => name,
      "root_dir" => root_dir,
      "meta" => decode_json(meta_json),
      "created_at" => created_at,
      "updated_at" => updated_at
    }
  end

  @doc "Decode a `novels` row (order of `novel_columns/0`)."
  @spec decode_novel([term()]) :: map()
  def decode_novel([id, title, source_path, text, meta_json, created_at]) do
    %{
      "id" => id,
      "title" => title,
      "source_path" => source_path,
      "text" => text,
      "meta" => decode_json(meta_json),
      "created_at" => created_at
    }
  end

  @doc "Decode a `chapters` row (order of `chapter_columns/0`)."
  @spec decode_chapter([term()]) :: map()
  def decode_chapter([id, novel_id, idx, title, text, summary]) do
    %{
      "id" => id,
      "novel_id" => novel_id,
      "idx" => idx,
      "title" => title,
      "text" => text,
      "summary" => summary
    }
  end

  @doc "Decode an `events` row (order of `event_columns/0`)."
  @spec decode_event([term()]) :: map()
  def decode_event([id, chapter_id, idx, kind, summary, payload_json]) do
    %{
      "id" => id,
      "chapter_id" => chapter_id,
      "idx" => idx,
      "kind" => kind,
      "summary" => summary,
      "payload" => decode_json(payload_json)
    }
  end

  @doc "Decode a `scripts` row (order of `script_columns/0`)."
  @spec decode_script([term()]) :: map()
  def decode_script([id, chapter_id, version, format, content, meta_json, created_at]) do
    %{
      "id" => id,
      "chapter_id" => chapter_id,
      "version" => version,
      "format" => format,
      "content" => content,
      "meta" => decode_json(meta_json),
      "created_at" => created_at
    }
  end

  @doc "Decode a `memory_notes` row (order of `memory_note_columns/0`)."
  @spec decode_memory_note([term()]) :: map()
  def decode_memory_note([id, kind, title, text, meta_json, created_at]) do
    %{
      "id" => id,
      "kind" => kind,
      "title" => title,
      "text" => text,
      "meta" => decode_json(meta_json),
      "created_at" => created_at
    }
  end

  @doc "Decode a `characters` row (order of `character_columns/0`)."
  @spec decode_character([term()]) :: map()
  def decode_character([id, name, appearance, refs_json, meta_json]) do
    %{
      "id" => id,
      "name" => name,
      "appearance" => appearance,
      "refs" => decode_json(refs_json) || [],
      "meta" => decode_json(meta_json)
    }
  end

  @doc "Decode a `shots` row (order of `shot_columns/0`)."
  @spec decode_shot([term()]) :: map()
  def decode_shot([
        id,
        script_id,
        idx,
        scene,
        shot_desc,
        size,
        camera,
        lighting,
        motion,
        prompt,
        meta_json
      ]) do
    %{
      "id" => id,
      "script_id" => script_id,
      "idx" => idx,
      "scene" => scene,
      "shot_desc" => shot_desc,
      "size" => size,
      "camera" => camera,
      "lighting" => lighting,
      "motion" => motion,
      "prompt" => prompt,
      "meta" => decode_json(meta_json)
    }
  end

  @doc "Decode an `assets` row (order of `asset_columns/0`)."
  @spec decode_asset([term()]) :: map()
  def decode_asset([id, shot_id, character_id, kind, path, url, prompt, meta_json, created_at]) do
    %{
      "id" => id,
      "shot_id" => shot_id,
      "character_id" => character_id,
      "kind" => kind,
      "path" => path,
      "url" => url,
      "prompt" => prompt,
      "meta" => decode_json(meta_json),
      "created_at" => created_at
    }
  end

  @doc "Decode a JSON text column, returning `nil` when absent or invalid."
  @spec decode_json(String.t() | nil) :: term()
  def decode_json(nil), do: nil
  def decode_json(""), do: nil

  def decode_json(text) when is_binary(text) do
    case Jason.decode(text) do
      {:ok, value} -> value
      {:error, _} -> nil
    end
  end

  @doc "Encode a value as JSON text, or `nil`."
  @spec encode_json(term()) :: String.t() | nil
  def encode_json(nil), do: nil
  def encode_json(value), do: Jason.encode!(value)
end
