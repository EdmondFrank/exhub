defmodule Exhub.LspBridge.Project do
  @moduledoc """
  Project root, language and server selection.

  Elixir port of the selection logic spread across `core/utils.py`
  (`get_project_path`), `lsp_bridge.py::LspBridge.find_project_root` and
  `open_file`. Given a buffer's file path it answers both *where* the project
  is (git toplevel, an Emacs-supplied override, or the file itself) and *which*
  language servers serve it (a single server by name/language, or every server
  referenced by a multi-server profile).

  The result feeds `Exhub.LspBridge.Session`, which keys itself on
  `{root, profile}`:

      {:ok, %{root: "/proj", profile: {:single, "elixirLS"}, multi: false,
              servers: [%Exhub.LspBridge.Config{}, ...]}}
  """

  alias Exhub.LspBridge.{Config, MultiServer}

  @max_depth 20

  # Directories holding vendored/installed code rather than a user project. A
  # file under one of these must not start a language server: the enclosing
  # "project" is a tool install (Homebrew and asdf are themselves git
  # checkouts), so a server would root at the install tree (and asdf's gopls
  # shim fails with exit 126). Override with:
  #
  #     config :exhub, Exhub.LspBridge.Project, ignored_prefixes: [...]
  @default_ignored_prefixes [
    "~/.asdf",
    "~/.gem",
    "~/.cache",
    "~/.cargo/registry",
    "~/.emacs.d/elpa",
    "~/.local/share",
    "~/go/pkg/mod",
    "/opt/homebrew",
    "/usr/local",
    "/nix/store"
  ]

  @typedoc "A resolved server selection for one buffer."
  @type selection :: %{
          root: String.t(),
          profile: term(),
          multi: boolean(),
          servers: [Config.t()]
        }

  @doc """
  Resolve the project root and server selection for `filepath`.

  `opts` (keyword or a JSON-decoded string-keyed map) may carry:

    * `:project_path` / `"project-path"` — authoritative root from Emacs;
    * `:multi` — a multi-server profile name;
    * `:single` — a single language server name;
    * `:language_id` / `"language-id"` — fall back to the server for a language.
  """
  @spec resolve(String.t(), keyword() | map() | nil) :: {:ok, selection()} | {:error, term()}
  def resolve(filepath, opts \\ []) do
    opts = normalize_opts(opts)

    if ignored_path?(filepath) do
      {:error, {:ignored_path, filepath}}
    else
      resolve_in_project(filepath, opts)
    end
  end

  defp resolve_in_project(filepath, opts) do
    root = project_path(filepath, opts)

    cond do
      is_binary(opts[:multi]) -> multi_selection(opts[:multi], root)
      is_binary(opts[:single]) -> single_by_name(opts[:single], root, filepath)
      is_binary(opts[:language_id]) -> single_by_language(opts[:language_id], root, filepath)
      true -> {:error, :no_language}
    end
  end

  @doc """
  Root directory for `filepath`.

  Prefers an explicit override, then `git rev-parse --show-toplevel`, and
  finally the file itself (a standalone file outside any repository).
  """
  @spec project_path(String.t(), keyword() | map() | nil) :: String.t()
  def project_path(filepath, opts \\ []) do
    opts = normalize_opts(opts)

    case opts[:project_path] do
      path when is_binary(path) and path != "" -> Path.expand(path)
      _ -> git_root_or_path(filepath)
    end
  end

  @doc """
  Walk up from `filepath` (at most `max_depth` levels) looking for any of
  `project_files`; return that directory or `nil`.
  """
  @spec find_project_root(String.t(), [String.t()], non_neg_integer()) :: String.t() | nil
  def find_project_root(filepath, project_files, max_depth \\ @max_depth) do
    start =
      if File.dir?(filepath) do
        Path.expand(filepath)
      else
        Path.dirname(Path.expand(filepath))
      end

    do_find_root(start, List.wrap(project_files), max_depth)
  end

  defp do_find_root(_dir, _files, depth) when depth <= 0, do: nil

  defp do_find_root(dir, files, depth) do
    if Enum.any?(files, &File.regular?(Path.join(dir, &1))) do
      dir
    else
      parent = Path.dirname(dir)

      if parent == dir do
        nil
      else
        do_find_root(parent, files, depth - 1)
      end
    end
  end

  # ===========================================================================
  # Selection
  # ===========================================================================

  defp single_by_name(name, root, filepath) do
    case Config.for_name(name) do
      {:ok, info} -> single_selection(info, root, filepath)
      {:error, :not_found} -> {:error, {:unknown_server, name}}
    end
  end

  defp single_by_language(language_id, root, filepath) do
    case Config.for_language(language_id) do
      {:ok, info} -> single_selection(info, root, filepath)
      {:error, :not_found} -> {:error, {:no_server_for_language, language_id}}
    end
  end

  # A server that does not support single-file operation must be rooted at a
  # directory containing one of its `projectFiles` (e.g. elixirLS at `mix.exs`).
  defp single_selection(info, root, filepath) do
    cond do
      File.dir?(root) ->
        {:ok, selection(root, {:single, info.name}, false, [info])}

      info.support_single_file == false ->
        case find_project_root(filepath, info.project_files) do
          nil -> {:error, {:unsupported_single_file, info.name}}
          project_root -> {:ok, selection(project_root, {:single, info.name}, false, [info])}
        end

      true ->
        {:ok, selection(root, {:single, info.name}, false, [info])}
    end
  end

  defp multi_selection(name, root) do
    case Config.multi(name) do
      {:ok, profile} ->
        infos =
          profile
          |> MultiServer.all_servers()
          |> Enum.flat_map(&infos_for/1)

        case infos do
          [] -> {:error, {:unknown_multiserver, name}}
          infos -> {:ok, selection(root, {:multi, name}, true, infos)}
        end

      {:error, :not_found} ->
        {:error, {:unknown_multiserver, name}}
    end
  end

  defp infos_for(server_name) do
    case Config.for_name(server_name) do
      {:ok, info} -> [info]
      {:error, :not_found} -> []
    end
  end

  defp selection(root, profile, multi, servers) do
    %{root: root, profile: profile, multi: multi, servers: servers}
  end

  # ===========================================================================
  # Root detection
  # ===========================================================================

  defp git_root_or_path(filepath) do
    expanded = Path.expand(filepath)
    dir = if File.dir?(expanded), do: expanded, else: Path.dirname(expanded)

    case git_toplevel(dir) do
      {:ok, root} -> root
      :error -> expanded
    end
  end

  defp git_toplevel(dir) do
    case System.cmd("git", ["-C", dir, "rev-parse", "--show-toplevel"], stderr_to_stdout: true) do
      {out, 0} -> {:ok, String.trim(out)}
      _ -> :error
    end
  rescue
    _ -> :error
  end

  # ===========================================================================
  # Ignored paths
  # ===========================================================================

  @doc false
  def ignored_path?(filepath) do
    path = Path.expand(filepath)

    Enum.any?(ignored_prefixes(), fn prefix ->
      path == prefix or String.starts_with?(path, prefix <> "/")
    end)
  end

  defp ignored_prefixes do
    :exhub
    |> Application.get_env(__MODULE__, [])
    |> Keyword.get(:ignored_prefixes, @default_ignored_prefixes)
    |> Enum.map(&Path.expand/1)
  end

  # ===========================================================================
  # Options
  # ===========================================================================

  defp normalize_opts(opts) when is_list(opts), do: opts

  defp normalize_opts(opts) when is_map(opts) do
    [
      multi: opts["multi"],
      single: opts["single"],
      language_id: opts["language-id"],
      project_path: opts["project-path"]
    ]
  end

  defp normalize_opts(_other), do: []
end
