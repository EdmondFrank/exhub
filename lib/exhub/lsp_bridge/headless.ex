defmodule Exhub.LspBridge.Headless do
  @moduledoc """
  Workspace-agnostic entry point to lsp-bridge sessions for callers that are
  not Emacs — the `lsp` MCP server, scripts and tests.

  The Emacs front end supplies a buffer's content and pins the project root
  with every command. A headless caller only has a path, so this module fills
  the gaps:

    * **root** — resolved from the file itself (`Exhub.LspBridge.Project`:
      explicit override, then `git rev-parse --show-toplevel`, then the
      server's `projectFiles` markers), never from a "current workspace";
    * **language** — from the extension or file name
      (`Exhub.LspBridge.Language`), with the preferred server for that
      language tried first;
    * **content** — read from disk and mirrored into the session with an
      mtime+size stamp, so a request always sees what the agent last wrote.
      A document an *editor* owns is never overwritten (`:editor_owned`);
    * **results** — raw LSP payloads, collected synchronously. Handlers in
      `Exhub.LspBridge.Handlers` shape results for Emacs rendering; a
      headless caller wants the JSON, so requests go through
      `Exhub.LspBridge.Session.request/5` directly.

  Sessions (and therefore language-server processes) are shared with Emacs:
  the same `{root, profile}` key means one `zls`/`gopls`/… per project, warm
  for both clients. `lsp_status`/1 exposes what is running and `shutdown/1`
  stops it.
  """

  alias Exhub.LspBridge.{Config, Document, Language, Ledger, Project, Session}

  @default_timeout 20_000
  @default_wait 10_000
  @poll_interval 120

  @typedoc "One server's reply: the raw LSP result or the failure reason."
  @type reply :: {String.t(), {:ok, term()} | {:error, term()}}

  # ===========================================================================
  # Public API
  # ===========================================================================

  @doc """
  Issue `method` for `path` and collect every target server's reply.

  `opts` (atom or string keys): `:server` (pin one server by name), `:multi`
  (multi-server profile), `:language_id`, `:project_path` (authoritative root),
  `:timeout_ms`, `:refresh` (default `true`; re-read the file from disk).

  Returns `{:ok, %{root, servers, sync, results}}` where `results` is a map of
  server name → `{:ok, raw_lsp_result}` / `{:error, reason}`.
  """
  @spec request(Path.t(), String.t(), map() | nil, keyword() | map()) ::
          {:ok, map()} | {:error, term()}
  def request(path, method, params \\ nil, opts \\ []) do
    path = Path.expand(path)
    timeout = opt(opts, :timeout_ms) || @default_timeout

    with_session(path, opts, fn pid, path, selection, sync ->
      case safe_request(pid, path, method, params || %{}) do
        {:ok, ids} ->
          {:ok,
           %{
             path: path,
             root: selection.root,
             servers: Enum.map(selection.servers, & &1.name),
             sync: sync,
             results: collect(ids, timeout)
           }}

        other ->
          {:error, other}
      end
    end)
  end

  @doc """
  Diagnostics for one path or a list of paths, freshly synced from disk and
  (for pull-capable servers) pulled rather than awaited.

  Every entry is either `%{path, root, servers, sync, diagnostics}` or
  `%{path, error}`; one unreadable file does not fail the batch.
  """
  @spec diagnostics(Path.t() | [Path.t()], keyword() | map()) :: {:ok, [map()]}
  def diagnostics(paths, opts \\ []) do
    paths = paths |> List.wrap() |> Enum.map(&Path.expand/1)
    {:ok, Enum.map(paths, &diagnose_one(&1, opts))}
  end

  @doc "Running sessions, their documents/capabilities, and the Ledger summary."
  @spec status() :: map()
  def status do
    sessions =
      Exhub.LspBridge.Registry
      |> Registry.select([{{{:session, :"$1", :"$2"}, :"$3", :_}, [], [{{:"$1", :"$2", :"$3"}}]}])
      |> Enum.map(fn {root, profile, pid} ->
        %{root: root, profile: profile_text(profile), info: sanitize_info(safe_info(pid))}
      end)

    %{sessions: sessions, ledger: Ledger.summary()}
  end

  @doc """
  Stop the session that owns `path` (or every session rooted at `path` when no
  document is open there). Its language-server processes are terminated.
  """
  @spec shutdown(Path.t()) :: {:ok, non_neg_integer()} | {:error, term()}
  def shutdown(path) do
    target = Path.expand(path)

    pids =
      case Session.find_by_path(target) do
        {:ok, pid} -> [pid]
        :error -> pids_for_root(target) ++ pids_for_ancestor(target)
      end

    case pids do
      [] ->
        {:error, :no_session}

      pids ->
        Enum.each(pids, fn pid ->
          try do
            Session.shutdown(pid)
          catch
            :exit, _ -> :ok
          end
        end)

        {:ok, length(pids)}
    end
  end

  @doc "Whether a language server is configured for `path`'s language."
  @spec supported?(Path.t()) :: boolean()
  def supported?(path), do: Language.supported?(path)

  # ===========================================================================
  # Result shaping
  # ===========================================================================

  @doc """
  Flatten `Location`/`LocationLink` payloads from every server into 1-based
  `%{"file", "line", "character", "end_line", "end_character"}` maps,
  de-duplicated across servers.
  """
  @spec locations(map() | [reply()]) :: [map()]
  def locations(%{results: results}), do: locations(results)

  def locations(results) when is_list(results) or is_map(results) do
    results
    |> Enum.flat_map(fn
      {_server, {:ok, value}} -> normalize_locations(value)
      _ -> []
    end)
    |> Enum.uniq_by(&{&1["file"], &1["line"], &1["character"]})
  end

  @doc "Plain-text hover contents per server (`MarkupContent`/`MarkedString`)."
  @spec hovers(map() | [reply()]) :: [{String.t(), String.t()}]
  def hovers(%{results: results}), do: hovers(results)

  def hovers(results) when is_list(results) or is_map(results) do
    Enum.flat_map(results, fn
      {server, {:ok, %{"contents" => contents}}} ->
        case render_markup(contents) do
          nil -> []
          text -> [{server, text}]
        end

      _ ->
        []
    end)
  end

  # ===========================================================================
  # Session plumbing
  # ===========================================================================

  defp with_session(path, opts, fun) do
    with :ok <- readable(path),
         {:ok, selection} <- resolve(path, opts),
         {:ok, pid} <- ensure_session(selection, opts) do
      subscribe(pid)

      case sync_document(pid, path, selection, opts) do
        {:ok, sync} -> fun.(pid, path, selection, sync)
        {:error, reason} -> {:error, reason}
      end
    end
  end

  defp safe_request(pid, path, method, params) do
    Session.request(pid, path, method, params, self())
  catch
    :exit, reason -> {:error, {:session_down, reason}}
  end

  defp readable(path) do
    if File.regular?(path), do: :ok, else: {:error, {:not_a_file, path}}
  end

  defp resolve(path, opts) do
    base =
      %{}
      |> put_if("project-path", opt(opts, :project_path))
      |> put_if("multi", opt(opts, :multi))

    result =
      cond do
        single = opt(opts, :server) ->
          Project.resolve(path, Map.put(base, "single", single))

        language = opt(opts, :language_id) ->
          resolve_language(path, base, language)

        language = Language.language_for(path) ->
          resolve_language(path, base, language)

        true ->
          {:error, {:no_language_server, Path.extname(path)}}
      end

    case result do
      {:ok, selection} -> {:ok, directory_root(selection)}
      other -> other
    end
  end

  # A standalone file outside any repository resolves to the file itself;
  # language servers are spawned with the root as their cwd, so it has to be a
  # directory.
  defp directory_root(%{root: root} = selection) do
    if File.dir?(root), do: selection, else: %{selection | root: Path.dirname(root)}
  end

  defp resolve_language(path, base, language) do
    base = marker_base(path, base, language)

    case Language.candidates(language) do
      [] ->
        {:error, {:no_server_for_language, language}}

      candidates ->
        candidates
        |> Enum.sort_by(fn info -> if installed?(info), do: 0, else: 1 end)
        |> Enum.find_value({:error, {:no_server_for_language, language}}, fn info ->
          case Project.resolve(path, Map.put(base, "single", info.name)) do
            {:ok, selection} -> {:ok, selection}
            {:error, _reason} -> nil
          end
        end)
    end
  end

  # Outside git there is no toplevel to fall back on; root the server at the
  # language's build marker instead of at the file's directory.
  defp marker_base(path, base, language) do
    cond do
      base["project-path"] != nil ->
        base

      in_git?(path) ->
        base

      true ->
        case Project.find_project_root(path, Language.project_files(language)) do
          nil -> base
          root -> Map.put(base, "project-path", root)
        end
    end
  end

  defp in_git?(path) do
    Project.project_path(path) != Path.expand(path)
  end

  defp installed?(%Config{} = info) do
    case Config.command_args(info) do
      [executable | _] -> executable?(executable)
      nil -> false
    end
  end

  defp executable?(executable) do
    cond do
      Path.type(executable) == :absolute -> File.regular?(executable)
      String.contains?(executable, "/") -> File.regular?(Path.expand(executable))
      true -> System.find_executable(executable) != nil
    end
  end

  defp ensure_session(selection, opts) do
    subscriber = Process.whereis(Ledger)

    session_opts = %{
      multi: selection.multi,
      server_infos: selection.servers,
      owner: subscriber,
      exec_path: opt(opts, :exec_path) || [],
      idle_stop: opt(opts, :idle_stop)
    }

    case Session.ensure(selection.root, selection.profile, session_opts) do
      {:ok, pid} ->
        case alive_servers(pid, selection) do
          {:ok, pid} -> {:ok, pid}
          {:error, reason} -> {:error, reason}
        end

      {:error, reason} ->
        {:error, {:session, reason}}
    end
  end

  # `Session.ensure/3` reuses an idle-stopped session as-is, so a headless call
  # after the idle timeout would otherwise report every server as unavailable.
  defp alive_servers(pid, selection) do
    case safe_info(pid) do
      %{servers: []} when selection.servers != [] ->
        Session.wake(pid)

        case safe_info(pid) do
          %{servers: []} ->
            {:error, {:server_unavailable, Enum.map(selection.servers, & &1.name)}}

          _ ->
            {:ok, pid}
        end

      _ ->
        {:ok, pid}
    end
  end

  defp subscribe(pid) do
    case Process.whereis(Ledger) do
      nil -> :ok
      ledger -> Session.subscribe(pid, ledger)
    end
  end

  defp sync_document(pid, path, selection, opts) do
    refresh = opt(opts, :refresh)
    refresh = if is_nil(refresh), do: true, else: refresh
    language_id = selection.servers |> List.first() |> lang_id()

    if refresh do
      with {:ok, stat} <- File.stat(path, time: :posix),
           {:ok, content} <- File.read(path) do
        Session.ensure_open(pid, path, content, {stat.mtime, stat.size}, %{
          "language-id" => language_id
        })
      end
    else
      {:ok, :unchanged}
    end
  end

  defp lang_id(%Config{language_id: id}), do: id
  defp lang_id(_), do: ""

  defp collect(ids, timeout) do
    deadline = System.monotonic_time(:millisecond) + timeout
    do_collect(ids, %{}, deadline)
  end

  defp do_collect([], acc, _deadline), do: sort_replies(acc)

  defp do_collect(pending, acc, deadline) do
    wait = max(deadline - System.monotonic_time(:millisecond), 0)

    receive do
      {:lsp_response, name, id, result} ->
        case take_pending(pending, name, id) do
          {:ok, rest} -> do_collect(rest, Map.put(acc, name, {:ok, result}), deadline)
          :stale -> do_collect(pending, acc, deadline)
        end

      {:lsp_error, name, id, reason} ->
        case take_pending(pending, name, id) do
          {:ok, rest} -> do_collect(rest, Map.put(acc, name, {:error, reason}), deadline)
          :stale -> do_collect(pending, acc, deadline)
        end
    after
      wait ->
        pending
        |> Enum.reduce(acc, fn {name, _id}, acc -> Map.put_new(acc, name, {:error, :timeout}) end)
        |> sort_replies()
    end
  end

  defp take_pending(pending, name, id) do
    if Enum.member?(pending, {name, id}) do
      {:ok, List.delete(pending, {name, id})}
    else
      :stale
    end
  end

  defp sort_replies(acc), do: acc |> Map.to_list() |> Enum.sort_by(fn {name, _} -> name end)

  # ===========================================================================
  # Diagnostics
  # ===========================================================================

  defp diagnose_one(path, opts) do
    result =
      with_session(path, opts, fn pid, path, selection, sync ->
        Session.pull_now(pid, path)

        {diagnostics, published} =
          await_diagnostics(pid, path, opt(opts, :wait_ms) || @default_wait, sync)

        {:ok,
         %{
           path: path,
           root: selection.root,
           language: selection.servers |> List.first() |> lang_id(),
           servers: Enum.map(selection.servers, & &1.name),
           sync: sync,
           published: published,
           diagnostics: diagnostics
         }}
      end)

    case result do
      {:ok, entry} -> entry
      {:error, reason} -> %{path: path, error: reason_text(reason)}
    end
  end

  # Pull diagnostics land in the cache asynchronously; push diagnostics arrive
  # after `didOpen`/`didChange` debounced by `diag_idle`. Poll until the list
  # stops changing (`[]` is a valid, stable answer), so a clean file does not
  # burn the whole budget and a fresh error is never missed.
  defp await_diagnostics(pid, path, wait, sync) do
    now = System.monotonic_time(:millisecond)
    settle(pid, path, now + wait, now + settle_floor(sync))
  end

  # A freshly opened or rewritten document makes the server re-analyse: give
  # it a floor before an empty (or still-old) list may be trusted as stable.
  defp settle_floor(:opened), do: 800
  defp settle_floor(:synced), do: 800
  defp settle_floor(_other), do: 0

  defp settle(pid, path, deadline, floor) do
    state =
      case safe_diagnostics_state(pid, path) do
        {:ok, value} -> value
        _ -> %{published: false, diagnostics: []}
      end

    now = System.monotonic_time(:millisecond)

    cond do
      state.published and now >= floor ->
        {state.diagnostics, true}

      now >= deadline ->
        {state.diagnostics, state.published}

      true ->
        Process.sleep(@poll_interval)
        settle(pid, path, deadline, floor)
    end
  end

  defp safe_diagnostics_state(pid, path) do
    Session.diagnostics_state(pid, path)
  catch
    :exit, _ -> {:error, :session_down}
  end

  # ===========================================================================
  # Small helpers
  # ===========================================================================

  defp safe_info(pid) do
    Session.info(pid)
  catch
    :exit, _ -> %{}
  end

  # Keep every value JSON-safe: the MCP layer encodes this map as-is.
  defp sanitize_info(info) when is_map(info) do
    info
    |> Map.new(fn
      {:profile, profile} -> {:profile, profile_text(profile)}
      {:documents, documents} when is_list(documents) -> {:documents, documents}
      other -> other
    end)
  end

  defp sanitize_info(other), do: other

  defp pids_for_root(root) do
    session_pids()
    |> Enum.filter(fn {session_root, _pid} -> session_root == root end)
    |> Enum.map(fn {_root, pid} -> pid end)
  end

  # Stopping a directory *inside* a project should stop that project's session,
  # so walk up from the target looking for a session root that contains it.
  defp pids_for_ancestor(target) do
    session_pids()
    |> Enum.filter(fn {session_root, _pid} ->
      String.starts_with?(target, session_root <> "/")
    end)
    |> Enum.map(fn {_root, pid} -> pid end)
  end

  defp session_pids do
    Exhub.LspBridge.Registry
    |> Registry.select([{{{:session, :"$1", :_}, :"$3", :_}, [], [{{:"$1", :"$3"}}]}])
  end

  defp profile_text({:single, name}), do: "single:#{name}"
  defp profile_text({:multi, name}), do: "multi:#{name}"
  defp profile_text(other), do: inspect(other)

  defp normalize_locations(nil), do: []

  defp normalize_locations(list) when is_list(list),
    do: Enum.flat_map(list, &normalize_locations/1)

  defp normalize_locations(%{"targetUri" => uri} = link) do
    range = link["targetSelectionRange"] || link["targetRange"] || %{}
    [location(uri, range)]
  end

  defp normalize_locations(%{"uri" => uri, "range" => range}), do: [location(uri, range)]

  defp normalize_locations(_other), do: []

  defp location(uri, range) do
    start = range["start"] || %{}
    stop = range["end"] || %{}

    %{
      "file" => Document.path_from_uri(uri),
      "line" => line(start["line"]),
      "character" => line(start["character"]),
      "end_line" => line(stop["line"]),
      "end_character" => line(stop["character"])
    }
  end

  defp line(nil), do: nil
  defp line(position) when is_integer(position), do: position + 1

  defp render_markup(text) when is_binary(text), do: text
  defp render_markup(%{"value" => value}) when is_binary(value), do: value

  defp render_markup(%{"language" => _language, "value" => value}) when is_binary(value),
    do: value

  defp render_markup(list) when is_list(list) do
    list
    |> Enum.map(&render_markup/1)
    |> Enum.reject(&is_nil/1)
    |> Enum.join("\n\n")
    |> case do
      "" -> nil
      text -> text
    end
  end

  defp render_markup(_other), do: nil

  defp reason_text(reason) when is_binary(reason), do: reason
  defp reason_text(reason), do: inspect(reason)

  defp opt(opts, key) when is_list(opts), do: Keyword.get(opts, key)

  defp opt(opts, key) when is_map(opts) do
    Map.get(opts, key) || Map.get(opts, Atom.to_string(key))
  end

  defp opt(_opts, _key), do: nil

  defp put_if(map, _key, nil), do: map
  defp put_if(map, key, value), do: Map.put(map, key, value)
end
