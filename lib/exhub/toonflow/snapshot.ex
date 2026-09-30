defmodule Exhub.Toonflow.Snapshot do
  @moduledoc """
  Read-only, UI-shaped views of a Toonflow project (Phase 5).

  `build/2` assembles everything the canvas web view needs in one shot — project
  metadata, workspace counts, `Pipeline.plan/3` stage readiness, recent `Jobs`,
  the storyboard with per-shot asset URLs, and the assembled outputs — so both
  the REST route and the websocket snapshot share one implementation.

  Asset URLs are rewritten to local `/toonflow/media/:name/<relative-path>`
  links when the file lives inside the project directory (falling back to the
  stored remote `url`); `resolve_media/3` is the reverse mapping used by the
  media route and rejects anything that escapes the project directory.
  """

  alias Exhub.Toonflow.{Jobs, Media, Pipeline, Store, Storyboard, Workspace}

  @media_prefix "/toonflow/media"
  # Only files under these project subdirectories may be served.
  @serve_dirs ~w(assets output storyboards)

  @type snapshot :: map()

  @doc "Build the full UI snapshot for `project` (name or id)."
  @spec build(String.t(), GenServer.server()) :: {:ok, snapshot()} | {:error, term()}
  def build(project, server \\ Store) do
    with {:ok, info} <- Store.project_info(project, server) do
      name = info["project"]["name"]
      dir = info["path"]

      {:ok, jobs} = Jobs.list(project, [limit: 10], server)
      {:ok, shots} = shots_with_assets(project, name, dir, server)

      {:ok,
       %{
         "project" => info["project"],
         "path" => dir,
         "exists" => info["exists"],
         "counts" => info["counts"],
         "plan" => plan(project, server),
         "jobs" => jobs,
         "shots" => shots,
         "outputs" => outputs(dir),
         "at" => now()
       }}
    end
  end

  @doc "Lightweight project list for the index page."
  @spec project_list(GenServer.server()) :: {:ok, [map()]} | {:error, term()}
  def project_list(server \\ Store) do
    case Store.list_projects(server) do
      {:ok, projects} ->
        {:ok,
         Enum.map(projects, fn p ->
           info = Store.project_info(p["id"], server)

           %{
             "id" => p["id"],
             "name" => p["name"],
             "meta" => p["meta"],
             "created_at" => p["created_at"],
             "updated_at" => p["updated_at"],
             "counts" => counts_from(info)
           }
         end)}

      error ->
        error
    end
  end

  @doc """
  Turn a stored asset into a browser URL.

  Prefers a local `/toonflow/media/...` link when `asset` has a `path` inside the
  project directory, else the stored remote `url`.
  """
  @spec asset_url(map() | nil, String.t(), String.t()) :: String.t() | nil
  def asset_url(nil, _name, _dir), do: nil

  def asset_url(asset, name, dir) do
    path = asset["path"]
    url = asset["url"]

    cond do
      is_binary(path) and inside?(path, dir) -> media_url(name, Path.relative_to(path, dir))
      is_binary(url) and url != "" -> url
      true -> nil
    end
  end

  @doc "Build a `/toonflow/media/...` URL from a project-relative path."
  @spec media_url(String.t(), String.t()) :: String.t()
  def media_url(name, rel) do
    @media_prefix <> "/" <> name <> "/" <> String.trim_leading(rel, "/")
  end

  @doc """
  Resolve a `/toonflow/media/:name/*rel` request to an absolute file path.

  Returns `{:ok, path}` only when `rel` stays inside the project directory **and**
  under one of `assets/`, `output/`, `storyboards/`, and the target is a regular
  file. Rejects `..` traversal, absolute paths and symlink escapes.
  """
  @spec resolve_media(String.t(), String.t(), String.t()) :: {:ok, String.t()} | {:error, term()}
  def resolve_media(name, rel, root \\ nil) do
    root = root || Workspace.root()
    base = Workspace.project_dir(root, name)

    with :ok <- valid_media_rel(rel),
         {:ok, resolved} <- resolve_in_base(base, rel),
         :ok <- check_within(resolved, base),
         :ok <- check_allowlist(resolved, base),
         :ok <- check_no_symlink(resolved, base),
         :ok <- check_regular(resolved) do
      {:ok, resolved}
    end
  end

  # ── internals ─────────────────────────────────────────────────────────

  defp plan(project, server) do
    case Pipeline.plan(project, [], server) do
      {:ok, plan} -> plan
      _ -> %{"stages" => [], "next" => nil}
    end
  end

  defp shots_with_assets(project, name, dir, server) do
    with {:ok, shots} <- Storyboard.list_shots(project, [], server),
         {:ok, assets} <- Media.list_assets(project, [], server) do
      latest = latest_assets(assets)

      {:ok,
       Enum.map(shots, fn shot ->
         id = shot["id"]

         %{
           "id" => id,
           "script_id" => shot["script_id"],
           "idx" => shot["idx"],
           "scene" => shot["scene"],
           "description" => shot["shot_desc"],
           "size" => shot["size"],
           "camera" => shot["camera"],
           "lighting" => shot["lighting"],
           "motion" => shot["motion"],
           "prompt" => shot["prompt"],
           "dialogue" => dialogue(shot),
           "characters" => characters(shot),
           "image" => asset_url(Map.get(latest, {id, "image"}), name, dir),
           "video" => asset_url(Map.get(latest, {id, "video"}), name, dir),
           "audio" => asset_url(Map.get(latest, {id, "audio"}), name, dir)
         }
       end)}
    else
      _ -> {:ok, []}
    end
  end

  # Assets are listed oldest-first, so later entries win.
  defp latest_assets(assets) do
    Enum.reduce(assets, %{}, fn a, acc -> Map.put(acc, {a["shot_id"], a["kind"]}, a) end)
  end

  defp dialogue(shot) do
    meta = shot["meta"] || %{}

    cond do
      is_binary(meta["dialogue"]) and meta["dialogue"] != "" -> meta["dialogue"]
      is_binary(meta["台词"]) and meta["台词"] != "" -> meta["台词"]
      true -> nil
    end
  end

  defp characters(shot) do
    case (shot["meta"] || %{})["characters"] do
      list when is_list(list) -> Enum.map(list, &to_string/1)
      name when is_binary(name) and name != "" -> [name]
      _ -> []
    end
  end

  defp outputs(dir) do
    out = Path.join([dir, "output"])

    case File.ls(out) do
      {:ok, entries} ->
        entries
        |> Enum.reject(&String.starts_with?(&1, "."))
        |> Enum.map(fn file ->
          path = Path.join(out, file)

          %{
            "name" => file,
            "url" => media_url(Path.basename(dir), Path.join("output", file)),
            "size" => size(path)
          }
        end)
        |> Enum.sort_by(& &1["name"])

      {:error, _} ->
        []
    end
  end

  defp size(path) do
    case File.stat(path) do
      {:ok, %{size: size}} -> size
      _ -> 0
    end
  end

  defp counts_from({:ok, info}), do: info["counts"]
  defp counts_from(_), do: %{}

  defp valid_media_rel(rel) when is_binary(rel) and rel != "" do
    cond do
      String.contains?(rel, "..") -> {:error, :invalid_path}
      Path.type(rel) == :absolute -> {:error, :invalid_path}
      true -> :ok
    end
  end

  defp valid_media_rel(_), do: {:error, :invalid_path}

  defp resolve_in_base(base, rel) do
    path = Path.expand(Path.join(base, rel))

    case File.stat(path) do
      {:ok, _stat} -> {:ok, path}
      {:error, _} -> {:error, :not_found}
    end
  end

  defp check_within(path, base) do
    base = Path.expand(base)
    path = Path.expand(path)

    if path == base or String.starts_with?(path, base <> "/") do
      :ok
    else
      {:error, :invalid_path}
    end
  end

  defp check_allowlist(path, base) do
    rel = path |> Path.expand() |> Path.relative_to(Path.expand(base))
    top = rel |> Path.split() |> List.first()

    if top in @serve_dirs, do: :ok, else: {:error, :forbidden}
  end

  # Reject a symlink anywhere along the path (File.stat would follow it).
  defp check_no_symlink(path, base) do
    rel = path |> Path.expand() |> Path.relative_to(Path.expand(base))

    rel
    |> Path.split()
    |> Enum.reduce_while(Path.expand(base), fn part, acc ->
      case File.lstat(Path.join(acc, part)) do
        {:ok, %{type: :symlink}} -> {:halt, {:error, :invalid_path}}
        _ -> {:cont, Path.join(acc, part)}
      end
    end)
    |> case do
      {:error, _} = error -> error
      _ -> :ok
    end
  end

  defp check_regular(path) do
    case File.stat(path) do
      {:ok, %{type: :regular}} -> :ok
      _ -> {:error, :not_found}
    end
  end

  defp inside?(path, dir) do
    dir = Path.expand(dir)
    path = Path.expand(path)
    String.starts_with?(path, dir <> "/")
  end

  defp now do
    DateTime.utc_now() |> DateTime.truncate(:second) |> DateTime.to_iso8601()
  end
end
