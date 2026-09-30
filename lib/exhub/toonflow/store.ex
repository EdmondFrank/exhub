defmodule Exhub.Toonflow.Store do
  @moduledoc """
  GenServer over the Toonflow SQLite stores.

  Owns the **registry** connection (`<root>/toonflow.db`) and serializes all
  access to it — the same pattern as `Exhub.MCP.Brain.RAG.VectorIndex`. Project
  databases (`<root>/workspaces/<project>/index.db`) are opened, initialized and
  closed on demand.

  Public functions return `{:ok, ...}` / `{:error, reason}`.
  """

  use GenServer
  require Logger

  alias Exhub.Toonflow.{Config, DB, Schema, Workspace}

  @type project :: map()
  @type server :: GenServer.server()

  # --- Client API ---

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc "List all projects, newest first."
  @spec list_projects(server()) :: {:ok, [project()]} | {:error, term()}
  def list_projects(server \\ __MODULE__), do: GenServer.call(server, :list_projects)

  @doc """
  Create a project: the workspace directory tree, `index.db`, `project.json`, and
  a registry row. `opts` may carry `:description` (stored under `meta`).
  """
  @spec create_project(String.t(), keyword(), server()) :: {:ok, project()} | {:error, term()}
  def create_project(name, opts \\ [], server \\ __MODULE__) do
    GenServer.call(server, {:create_project, name, opts})
  end

  @doc "Fetch a project by name or id."
  @spec get_project(String.t(), server()) :: {:ok, project()} | {:error, term()}
  def get_project(name_or_id, server \\ __MODULE__) do
    GenServer.call(server, {:get_project, name_or_id})
  end

  @doc "Project metadata plus workspace statistics."
  @spec project_info(String.t(), server()) :: {:ok, map()} | {:error, term()}
  def project_info(name_or_id, server \\ __MODULE__) do
    GenServer.call(server, {:project_info, name_or_id})
  end

  @doc """
  Run `fun` against the project's `index.db` connection, inside the Store
  process so all project-database access stays serialized. The connection is
  opened on demand and closed afterwards. `fun` receives the `Exqlite`
  connection and must return `{:ok, value}` or `{:error, reason}`.
  """
  @spec run_project(String.t(), (term() -> {:ok, term()} | {:error, term()}), server()) ::
          {:ok, term()} | {:error, term()}
  def run_project(name_or_id, fun, server \\ __MODULE__) when is_function(fun, 1) do
    GenServer.call(server, {:run_project, name_or_id, fun}, :infinity)
  end

  # --- Server callbacks ---

  @impl true
  def init(opts) do
    root = Keyword.get(opts, :root_dir) || Config.root_dir()
    path = Keyword.get(opts, :registry_path) || Workspace.registry_db(root)
    expanded_root = Path.expand(root)

    case DB.open(path, Schema.registry_ddl()) do
      {:ok, conn} ->
        {:ok, %{conn: conn, root: expanded_root, path: path}}

      {:error, reason} ->
        Logger.error("[Toonflow.Store] registry unavailable at #{path}: #{inspect(reason)}")
        {:ok, %{conn: nil, root: expanded_root, path: path}}
    end
  end

  @impl true
  def handle_call(:list_projects, _from, %{conn: nil} = state) do
    {:reply, {:error, :registry_unavailable}, state}
  end

  def handle_call(:list_projects, _from, state) do
    sql = "SELECT #{Schema.project_columns()} FROM projects ORDER BY created_at DESC"

    case DB.query(state.conn, sql, []) do
      {:ok, rows} -> {:reply, {:ok, Enum.map(rows, &Schema.decode_project/1)}, state}
      {:error, reason} -> {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:create_project, name, opts}, _from, state) do
    {:reply, do_create_project(state, name, opts), state}
  end

  def handle_call({:get_project, _key}, _from, %{conn: nil} = state) do
    {:reply, {:error, :registry_unavailable}, state}
  end

  def handle_call({:get_project, key}, _from, state) do
    {:reply, lookup(state.conn, key), state}
  end

  def handle_call({:project_info, _key}, _from, %{conn: nil} = state) do
    {:reply, {:error, :registry_unavailable}, state}
  end

  def handle_call({:project_info, key}, _from, state) do
    case lookup(state.conn, key) do
      {:ok, project} -> {:reply, {:ok, build_info(state.root, project)}, state}
      {:error, reason} -> {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:run_project, _key, _fun}, _from, %{conn: nil} = state) do
    {:reply, {:error, :registry_unavailable}, state}
  end

  def handle_call({:run_project, key, fun}, _from, state) do
    reply =
      case lookup(state.conn, key) do
        {:ok, project} -> run_in_project(state.root, project["name"], fun)
        {:error, reason} -> {:error, reason}
      end

    {:reply, reply, state}
  end

  # --- registry helpers ---

  defp do_create_project(%{conn: nil}, _name, _opts), do: {:error, :registry_unavailable}

  defp do_create_project(state, name, opts) do
    cond do
      not Workspace.valid_name?(name) ->
        {:error, {:invalid_name, name}}

      match?({:ok, _}, lookup(state.conn, name)) ->
        {:error, :already_exists}

      true ->
        create_project_files(state, name, opts)
    end
  end

  defp create_project_files(state, name, opts) do
    root = state.root
    id = new_id("prj")
    now = now_iso()
    meta = build_meta(opts)
    dir = Workspace.project_dir(root, name)
    dirs = [dir | Enum.map(Workspace.project_subdirs(), &Path.join(dir, &1))]

    with :ok <- mkdirs(dirs),
         :ok <- init_project_db(root, name),
         :ok <- write_project_json(root, name, id, now, meta),
         :ok <-
           insert_project_row(state.conn, %{
             id: id,
             name: name,
             root_dir: dir,
             meta_json: Schema.encode_json(meta),
             created_at: now,
             updated_at: now
           }) do
      {:ok, project_map(id, name, dir, meta, now)}
    end
  end

  defp insert_project_row(conn, p) do
    DB.execute(
      conn,
      """
      INSERT INTO projects (id, name, root_dir, meta_json, created_at, updated_at)
      VALUES (?, ?, ?, ?, ?, ?)
      """,
      [p.id, p.name, p.root_dir, p.meta_json, p.created_at, p.updated_at]
    )
  end

  defp lookup(_conn, nil), do: {:error, :not_found}

  defp lookup(conn, key) when is_binary(key) do
    sql = "SELECT #{Schema.project_columns()} FROM projects WHERE name = ? OR id = ? LIMIT 1"

    case DB.query(conn, sql, [key, key]) do
      {:ok, [row | _]} -> {:ok, Schema.decode_project(row)}
      {:ok, []} -> {:error, :not_found}
      {:error, reason} -> {:error, reason}
    end
  end

  defp lookup(_conn, _key), do: {:error, :not_found}

  defp build_info(root, project) do
    name = project["name"]

    %{
      "project" => project,
      "path" => Workspace.project_dir(root, name),
      "exists" => File.dir?(Workspace.project_dir(root, name)),
      "db" => Workspace.project_db(root, name),
      "counts" => %{
        "novels" => count_files(Workspace.novels_dir(root, name)),
        "chapters" => count_files(Workspace.chapters_dir(root, name)),
        "scripts" => count_files(Workspace.scripts_dir(root, name)),
        "characters" => count_files(Workspace.characters_dir(root, name)),
        "storyboards" => count_files(Workspace.storyboards_dir(root, name)),
        "images" => count_files(Workspace.assets_dir(root, name, "images")),
        "videos" => count_files(Workspace.assets_dir(root, name, "videos")),
        "audio" => count_files(Workspace.assets_dir(root, name, "audio")),
        "output" => count_files(Workspace.output_dir(root, name))
      }
    }
  end

  defp count_files(dir) do
    case File.ls(dir) do
      {:ok, entries} -> length(Enum.reject(entries, &String.starts_with?(&1, ".")))
      {:error, _} -> 0
    end
  end

  # --- filesystem helpers ---

  defp init_project_db(root, name) do
    path = Workspace.project_db(root, name)

    case DB.open(path, Schema.project_ddl()) do
      {:ok, conn} ->
        DB.close(conn)
        :ok

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp write_project_json(root, name, id, now, meta) do
    data = %{"id" => id, "name" => name, "created_at" => now, "meta" => meta}
    File.write(Workspace.project_json(root, name), Jason.encode!(data, pretty: true))
  end

  defp build_meta(opts) do
    case Keyword.get(opts, :description) do
      desc when is_binary(desc) and desc != "" -> %{"description" => desc}
      _ -> nil
    end
  end

  defp project_map(id, name, dir, meta, now) do
    %{
      "id" => id,
      "name" => name,
      "root_dir" => dir,
      "meta" => meta,
      "created_at" => now,
      "updated_at" => now
    }
  end

  defp mkdirs(dirs) do
    Enum.reduce_while(dirs, :ok, fn dir, :ok ->
      case File.mkdir_p(dir) do
        :ok -> {:cont, :ok}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp new_id(prefix),
    do: prefix <> "_" <> Base.encode16(:crypto.strong_rand_bytes(6), case: :lower)

  defp now_iso do
    DateTime.utc_now() |> DateTime.truncate(:second) |> DateTime.to_iso8601()
  end

  # --- project database ---

  defp run_in_project(root, name, fun) do
    path = Workspace.project_db(root, name)

    case DB.open(path, Schema.project_ddl()) do
      {:ok, conn} ->
        try do
          fun.(conn)
        rescue
          e -> {:error, {:exception, Exception.message(e)}}
        after
          DB.close(conn)
        end

      {:error, reason} ->
        {:error, reason}
    end
  end
end
