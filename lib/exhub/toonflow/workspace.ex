defmodule Exhub.Toonflow.Workspace do
  @moduledoc """
  Pure path helpers for Toonflow project workspaces.

  Layout (`<root>` defaults to `~/.config/exhub/toonflow`):

      <root>/
        toonflow.db                 # project registry
        workspaces/
          <project>/
            project.json
            index.db                # project data (novels, scripts, ...)
            novels/ chapters/ scripts/ characters/ storyboards/
            assets/ images/ videos/ audio/
            output/

  Every function takes the workspace `root` explicitly (with 0/1-arity
  conveniences that read `Exhub.Toonflow.Config.root_dir/0`) so tests can use a
  temporary root without touching global config.
  """

  alias Exhub.Toonflow.Config

  # Lowercase slug: starts alphanumeric, then alphanumerics, dot, dash or
  # underscore. Deliberately restrictive — the name becomes a directory name.
  @name_re ~r/^[a-z0-9][a-z0-9._-]*$/
  @max_name_bytes 64

  @doc "The configured workspace root."
  @spec root() :: String.t()
  def root, do: Config.root_dir()

  @doc "The `workspaces/` directory under `root`."
  def workspaces_root(root) when is_binary(root), do: Path.join(root, "workspaces")
  def workspaces_root, do: workspaces_root(root())

  @doc "Path to the project registry SQLite database."
  def registry_db(root) when is_binary(root), do: Path.join(root, "toonflow.db")
  def registry_db, do: registry_db(root())

  @doc "Directory for a project."
  def project_dir(root, name) when is_binary(root), do: Path.join(workspaces_root(root), name)
  def project_dir(name), do: project_dir(root(), name)

  @doc "Path to a project's data SQLite database."
  def project_db(root, name), do: Path.join(project_dir(root, name), "index.db")
  def project_db(name), do: project_db(root(), name)

  @doc "Path to a project's portable metadata mirror."
  def project_json(root, name), do: Path.join(project_dir(root, name), "project.json")
  def project_json(name), do: project_json(root(), name)

  @doc "A direct subdirectory of a project."
  def subdir(root, name, sub), do: Path.join(project_dir(root, name), sub)
  def subdir(name, sub), do: subdir(root(), name, sub)

  def novels_dir(root, name), do: subdir(root, name, "novels")
  def novels_dir(name), do: novels_dir(root(), name)

  def chapters_dir(root, name), do: subdir(root, name, "chapters")
  def chapters_dir(name), do: chapters_dir(root(), name)

  def scripts_dir(root, name), do: subdir(root, name, "scripts")
  def scripts_dir(name), do: scripts_dir(root(), name)

  def characters_dir(root, name), do: subdir(root, name, "characters")
  def characters_dir(name), do: characters_dir(root(), name)

  def storyboards_dir(root, name), do: subdir(root, name, "storyboards")
  def storyboards_dir(name), do: storyboards_dir(root(), name)

  @doc "Directory holding a project's exported memory notes (`memory/notes`)."
  def memory_notes_dir(root, name), do: Path.join(subdir(root, name, "memory"), "notes")
  def memory_notes_dir(name), do: memory_notes_dir(root(), name)

  def output_dir(root, name), do: subdir(root, name, "output")
  def output_dir(name), do: output_dir(root(), name)

  @doc ~S(The asset directory for a given kind, e.g. "images", "videos", "audio".)
  def assets_dir(root, name, kind), do: Path.join(subdir(root, name, "assets"), kind)
  def assets_dir(name, kind), do: assets_dir(root(), name, kind)

  @doc "All subdirectories created for a new project (excluding the project dir)."
  @spec project_subdirs() :: [String.t()]
  def project_subdirs do
    [
      "novels",
      "chapters",
      "scripts",
      "characters",
      "storyboards",
      "memory/notes",
      "assets/images",
      "assets/videos",
      "assets/audio",
      "output"
    ]
  end

  @doc "Whether `name` is usable as a project name (and thus a directory name)."
  @spec valid_name?(term()) :: boolean()
  def valid_name?(name) when is_binary(name) do
    byte_size(name) <= @max_name_bytes and Regex.match?(@name_re, name)
  end

  def valid_name?(_), do: false

  @doc "Normalize an arbitrary string into a candidate project slug."
  @spec slugify(String.t()) :: String.t()
  def slugify(name) when is_binary(name) do
    name
    |> String.downcase()
    |> String.replace(~r/[^a-z0-9._-]+/u, "-")
    |> String.trim("-")
  end
end
