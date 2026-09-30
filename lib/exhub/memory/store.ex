defmodule Exhub.Memory.Store do
  @moduledoc """
  Vault-backed store for memory notes.

  A memory is a single markdown note in the Brain (Obsidian) vault, under the
  configured `:vault_folder` (default `memory/`). Its frontmatter carries the
  whole lifecycle — `status`, `kind`, `project`, `evaluation`, `evidence`,
  supersede links — and the body holds the lesson text. Because memories are
  plain vault notes they stay greppable, linkable and visible to every existing
  Brain tool.

  The store touches only the filesystem and `Exhub.MCP.Brain.Helpers`, so tests
  can point `:exhub, :obsidian_vault_path` at a temporary directory.

  ## Record shape

  `read/1`, `all/0` and `list/1` return:

      %{meta: map(), body: String.t(), file: String.t(), full_path: String.t()}

  `meta` uses string keys as decoded from the note frontmatter.
  """

  alias Exhub.MCP.Brain.Helpers
  alias Exhub.Memory.Frontmatter

  @defaults [
    vault_folder: "memory",
    skill_folder: "memory/skills",
    kinds: ~w(workflow correction debugging_pattern gotcha convention),
    statuses: ~w(candidate approved rejected superseded)
  ]

  @doc "Effective `:exhub, :memory` configuration merged over in-code defaults."
  @spec config() :: keyword()
  def config do
    Keyword.merge(@defaults, Application.get_env(:exhub, :memory, []) || [])
  end

  @doc "Absolute path of the memory folder for the current vault."
  @spec folder() :: String.t()
  def folder, do: Path.join(Helpers.vault_path(), config() |> Keyword.fetch!(:vault_folder))

  @doc "Vault-relative memory folder."
  @spec relative_folder() :: String.t()
  def relative_folder, do: config() |> Keyword.fetch!(:vault_folder)

  @doc "Absolute path of a memory note."
  @spec path(String.t()) :: String.t()
  def path(memory_id), do: Path.join(folder(), memory_id <> ".md")

  @doc "Generate a fresh `memory_<hex>` id."
  @spec new_id() :: String.t()
  def new_id do
    "memory_" <> (:crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower))
  end

  @doc "Valid lifecycle statuses."
  @spec statuses() :: [String.t()]
  def statuses, do: config() |> Keyword.fetch!(:statuses)

  @doc "Valid memory kinds."
  @spec kinds() :: [String.t()]
  def kinds, do: config() |> Keyword.fetch!(:kinds)

  @doc "Current UTC timestamp as an ISO-8601 string."
  @spec now_iso() :: String.t()
  def now_iso, do: DateTime.utc_now() |> DateTime.to_iso8601()

  @doc """
  Write a new memory note. Missing `memory_id`/`status`/timestamps are filled in.
  Returns `{:ok, memory_id, path}`.
  """
  @spec create(map(), String.t()) :: {:ok, String.t(), String.t()} | {:error, term()}
  def create(meta, body) do
    meta = meta |> stringify() |> put_new_defaults()

    case write(meta, body) do
      {:ok, path} -> {:ok, meta["memory_id"], path}
      other -> other
    end
  end

  @doc "Overwrite a memory note with the given metadata and body."
  @spec write(map(), String.t()) :: {:ok, String.t()} | {:error, term()}
  def write(meta, body) do
    meta = stringify(meta)
    path = path(meta["memory_id"])

    with :ok <- File.mkdir_p(Path.dirname(path)) do
      content = "---\n" <> Frontmatter.encode(meta) <> "\n---\n\n" <> body

      case File.write(path, content) do
        :ok -> {:ok, path}
        other -> other
      end
    end
  end

  @doc "Merge `changes` into a note's frontmatter, optionally replacing the body."
  @spec update(String.t(), map(), String.t() | nil) :: {:ok, map()} | {:error, term()}
  def update(memory_id, changes, body \\ nil) do
    with {:ok, record} <- read(memory_id) do
      meta =
        record.meta
        |> Map.merge(stringify(changes))
        |> Map.put("updated_at", now_iso())

      case write(meta, body || record.body) do
        {:ok, _path} -> {:ok, meta}
        other -> other
      end
    end
  end

  @doc "Read a single memory by id. Returns `{:error, :not_found}` when absent."
  @spec read(String.t()) :: {:ok, map()} | {:error, :not_found}
  def read(memory_id) do
    path = path(memory_id)

    case File.read(path) do
      {:ok, content} -> {:ok, record(path, content)}
      {:error, _} -> {:error, :not_found}
    end
  end

  @doc "All memory records currently on disk."
  @spec all() :: [map()]
  def all do
    vault = Helpers.vault_path()

    vault
    |> Helpers.list_md_files(folder())
    |> Enum.flat_map(fn rel ->
      full = Path.join(vault, rel)

      case File.read(full) do
        {:ok, content} -> [record(full, content)]
        {:error, _} -> []
      end
    end)
  end

  @doc """
  List memories filtered by `:status`, `:kind` and `:project` (all optional).
  """
  @spec list(keyword()) :: [map()]
  def list(opts \\ []) do
    status = opts[:status]
    kind = opts[:kind]
    project = opts[:project]

    all()
    |> Enum.filter(&(is_nil(status) or &1.meta["status"] == status))
    |> Enum.filter(&(is_nil(kind) or &1.meta["kind"] == kind))
    |> Enum.filter(&(is_nil(project) or project_match?(&1.meta, project)))
  end

  @doc """
  Whether a memory's metadata belongs to `project`, matching either the
  explicit `project` field or a `project/<name>` / `proj/<name>` tag.
  """
  @spec project_match?(map(), String.t()) :: boolean()
  def project_match?(meta, project) do
    tags = Map.get(meta, "tags", []) || []

    Map.get(meta, "project") == project or
      Enum.any?(tags, fn tag ->
        tag == project or tag == "project/" <> project or tag == "proj/" <> project
      end)
  end

  # ── private ──────────────────────────────────────────────────────────────

  defp record(full_path, content) do
    {meta, body} = Frontmatter.decode(content)
    vault = Helpers.vault_path()

    %{meta: meta, body: body, file: Path.relative_to(full_path, vault), full_path: full_path}
  end

  defp put_new_defaults(meta) do
    now = now_iso()

    meta
    |> Map.put_new("memory_id", new_id())
    |> Map.put_new("status", "candidate")
    |> Map.put_new("created_at", now)
    |> Map.put("updated_at", now)
  end

  defp stringify(meta) do
    Map.new(meta, fn {k, v} -> {to_string(k), v} end)
  end
end
