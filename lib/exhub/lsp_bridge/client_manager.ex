defmodule Exhub.LspBridge.ClientManager do
  @moduledoc """
  Coordinator for the lsp-bridge port — the Elixir equivalent of the Python
  `LspBridge` class in `lsp_bridge.py`.

  It is the WebSocket command router and the single place that pushes results
  back to Emacs as elisp. Commands arrive as
  `[\"func\", [\"lsp-bridge\", action, …args]]`; this module resolves the
  project/selection for the addressed buffer (via `Exhub.LspBridge.Project`),
  finds or starts the matching `Exhub.LspBridge.Session`, and forwards the
  lifecycle call to it.

  ## Command surface

  | Command | Args | Effect |
  |---------|------|--------|
  | `ping` | — | replies `(exhub-lsp-pong)` |
  | `open-file` | `path`, `content`, `opts` | resolve + start session, `didOpen` |
  | `change-file` | `path`, `change` | `didChange` (+ pull diagnostics) |
  | `save-file` / `close-file` | `path` | `didSave` / `didClose` |
  | `update-file` | `path`, `content` | full-text `didChange` (after elisp-applied edits) |
  | `change-cursor` | `path`, `position` | record cursor time |
  | `request` / `notify` | `path`, `method`, `params` | route a raw JSON-RPC call |
  | `diagnostics` / `list-diagnostics` | `path`, `opts` | merged diagnostics |
  | `hover` | `path`, `position` | `(exhub-lsp--hover path markdown)` |
  | `find-define` / `find-type-define` / `find-implementation` | `path`, `position` | `(exhub-lsp--locations path kind json)` |
  | `find-references` | `path`, `position` | `(exhub-lsp--locations path "references" json)` |
  | `document-symbol` | `path` | `(exhub-lsp--symbols path json)` |
  | `workspace-symbol` | `path`, `query` | `(exhub-lsp--workspace-symbols query json)` |
  | `signature-help` | `path`, `position` | `(exhub-lsp--signature-help path json)` |
  | `completion` | `path`, `position`, `char`, `prefix`, `opts` | `(exhub-lsp--completion path server candidates items meta)` |
  | `completion-item-resolve` | `path`, `key`, `server`, `item` | `(exhub-lsp--completion-doc path server key doc edits)` |
  | `prepare-rename` | `path`, `position` | `(exhub-lsp--rename-range path range)` |
  | `rename` | `path`, `position`, `new_name` | `(exhub-lsp--workspace-edit edit message)` |
  | `format` / `range-format` | `path`, `opts` / `range`, `opts` | `(exhub-lsp--format path edits)` |
  | `code-action` | `path`, `range`, `opts` | `(exhub-lsp--code-actions path actions)` |
  | `execute-command` | `path`, `command`, `arguments` | `(exhub-lsp--workspace-edit …)` / message |
  | `call-hierarchy-prepare` | `path`, `position` | `(exhub-lsp--call-hierarchy-items path items)` |
  | `call-hierarchy-incoming` / `-outgoing` | `path`, `item` | `(exhub-lsp--call-hierarchy path direction calls)` |
  | `inlay-hint` | `path`, `range` | `(exhub-lsp--inlay-hints path hints)` |
  | `semantic-tokens` | `path` | `(exhub-lsp--semantic-tokens path tokens)` |
  | `shutdown` | `path` | stop the session and its servers |
  | `start-server` / `stop-server` | `name`, `project_path` | raw server control |

  Emacs callbacks emitted: `(exhub-lsp-ready path servers)`,
  `(exhub-lsp-diagnostics path diagnostics count)`,
  `(exhub-lsp-response server id result)`,
  `(exhub-lsp-error-response server id error)`,
  `(exhub-lsp-notification server method params)`, `(exhub-lsp-message msg)` and
  `(exhub-lsp-error msg)`. Payloads are JSON strings parsed with
  `json-parse-string` on the elisp side.

  Lazily started (`ensure_started/0`) so the feature works after a hot reload
  without a VM restart; also supervised at boot.
  """

  use GenServer
  require Logger

  alias Exhub.LspBridge.{Config, Elisp, Project, Server, Session}

  @registry Exhub.LspBridge.Registry
  @server_sup Exhub.LspBridge.Supervisor

  # ===========================================================================
  # Public API
  # ===========================================================================

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def ensure_started do
    case Process.whereis(__MODULE__) do
      nil ->
        case GenServer.start(__MODULE__, %{}, name: __MODULE__) do
          {:ok, pid} -> {:ok, pid}
          {:error, {:already_started, pid}} -> {:ok, pid}
          {:error, reason} -> {:error, reason}
        end

      pid ->
        {:ok, pid}
    end
  end

  @doc "Handle one `lsp-bridge` WebSocket command (args list without the tag)."
  def handle_command(args) when is_list(args) do
    ensure_started()
    GenServer.cast(__MODULE__, {:command, args})
  end

  # ===========================================================================
  # GenServer callbacks
  # ===========================================================================

  @impl true
  def init(_opts), do: {:ok, %{}}

  @impl true
  def handle_cast({:command, args}, state) do
    {:noreply, dispatch(args, state)}
  end

  @impl true
  def handle_info({:lsp_initialized, {:server, project, name}, caps}, state) do
    emacs(form("exhub-lsp-ready", [Elisp.string(project), Elisp.json([name])]))
    _ = caps
    {:noreply, state}
  end

  def handle_info({:lsp_diagnostics_update, path, diagnostics, count}, state) do
    emacs(
      form("exhub-lsp-diagnostics", [
        Elisp.string(path),
        Elisp.json(diagnostics),
        Integer.to_string(count)
      ])
    )

    {:noreply, state}
  end

  def handle_info({:lsp_notification, server, method, params}, state) do
    emacs(
      form("exhub-lsp-notification", [
        Elisp.string(server),
        Elisp.string(method),
        Elisp.json(params)
      ])
    )

    {:noreply, state}
  end

  def handle_info({:lsp_response, server, id, result}, state) do
    emacs(
      form("exhub-lsp-response", [
        Elisp.string(server),
        Elisp.string(to_string(id)),
        Elisp.json(result)
      ])
    )

    {:noreply, state}
  end

  def handle_info({:lsp_error, server, id, error}, state) do
    emacs(
      form("exhub-lsp-error-response", [
        Elisp.string(server),
        Elisp.string(to_string(id)),
        Elisp.json(error)
      ])
    )

    {:noreply, state}
  end

  def handle_info({:lsp_error, message}, state) when is_binary(message) do
    emacs_error(message)
    {:noreply, state}
  end

  def handle_info({:lsp_handler_result, payload}, state) do
    emacs(render(payload))
    {:noreply, state}
  end

  def handle_info({:lsp_handler_error, message}, state) when is_binary(message) do
    emacs_error(message)
    {:noreply, state}
  end

  def handle_info(_other, state), do: {:noreply, state}

  # ===========================================================================
  # Command dispatch
  # ===========================================================================

  defp dispatch(["ping"], state) do
    emacs(form("exhub-lsp-pong", []))
    state
  end

  defp dispatch(["open-file", path, content | rest], state) do
    opts = normalize_map(List.first(rest))

    case Project.resolve(path, opts) do
      {:ok, selection} ->
        open_in_session(selection, path, content, opts)

      {:error, {:ignored_path, _}} ->
        Logger.debug("[LspBridge.ClientManager] ignoring vendored path #{path}")

      {:error, reason} ->
        emacs_error("cannot open #{path}: #{describe(reason)}")
    end

    state
  end

  defp dispatch(["change-file", path, change], state) do
    with_session(path, fn pid -> Session.change_file(pid, path, change || %{}) end)
    state
  end

  defp dispatch(["save-file", path], state) do
    with_session(path, fn pid -> Session.save_file(pid, path) end)
    state
  end

  defp dispatch(["close-file", path], state) do
    with_session(path, fn pid -> Session.close_file(pid, path) end)
    state
  end

  defp dispatch(["update-file", path, content], state) do
    with_session(path, fn pid -> Session.update_file(pid, path, content) end)
    state
  end

  defp dispatch(["change-cursor", path, position], state) do
    with_session(path, fn pid -> Session.change_cursor(pid, path, position) end)
    state
  end

  defp dispatch(["request", path, method, params], state) do
    with_session(path, fn pid -> Session.request(pid, path, method, params || %{}, self()) end)
    state
  end

  defp dispatch(["notify", path, method, params], state) do
    with_session(path, fn pid -> Session.notify(pid, path, method, params || %{}) end)
    state
  end

  defp dispatch(["hover", path, position], state) do
    perform(path, "hover", %{"position" => position})
    state
  end

  defp dispatch(["find-define", path, position], state) do
    perform(path, "find-define", %{"position" => position})
    state
  end

  defp dispatch(["find-type-define", path, position], state) do
    perform(path, "find-type-define", %{"position" => position})
    state
  end

  defp dispatch(["find-implementation", path, position], state) do
    perform(path, "find-implementation", %{"position" => position})
    state
  end

  defp dispatch(["find-references", path, position], state) do
    perform(path, "find-references", %{"position" => position})
    state
  end

  defp dispatch(["signature-help", path, position], state) do
    perform(path, "signature-help", %{"position" => position})
    state
  end

  defp dispatch(["completion", path, position, char, prefix | rest], state) do
    opts = normalize_map(List.first(rest))

    args =
      opts
      |> Map.put("position", position)
      |> Map.put("char", char)
      |> Map.put("prefix", prefix)

    perform(path, "completion", args)
    state
  end

  defp dispatch(["completion-item-resolve", path, key, server, item], state) do
    perform(path, "completion-item-resolve", %{"key" => key, "server" => server, "item" => item})
    state
  end

  defp dispatch(["prepare-rename", path, position], state) do
    perform(path, "prepare-rename", %{"position" => position})
    state
  end

  defp dispatch(["rename", path, position, new_name], state) do
    perform(path, "rename", %{"position" => position, "newName" => new_name})
    state
  end

  defp dispatch(["format", path | rest], state) do
    perform(path, "format", normalize_map(List.first(rest)))
    state
  end

  defp dispatch(["range-format", path, range | rest], state) do
    args = List.first(rest) |> normalize_map() |> Map.put("range", range)
    perform(path, "range-format", args)
    state
  end

  defp dispatch(["code-action", path, range | rest], state) do
    args = List.first(rest) |> normalize_map() |> Map.put("range", range)
    perform(path, "code-action", args)
    state
  end

  defp dispatch(["execute-command", path, command | rest], state) do
    arguments = List.first(rest)

    args = %{
      "command" => command,
      "arguments" => if(is_list(arguments), do: arguments, else: [])
    }

    perform(path, "execute-command", args)
    state
  end

  defp dispatch(["call-hierarchy-prepare", path, position], state) do
    perform(path, "call-hierarchy-prepare", %{"position" => position})
    state
  end

  defp dispatch(["call-hierarchy-incoming", path, item], state) do
    perform(path, "call-hierarchy-incoming", %{"item" => item})
    state
  end

  defp dispatch(["call-hierarchy-outgoing", path, item], state) do
    perform(path, "call-hierarchy-outgoing", %{"item" => item})
    state
  end

  defp dispatch(["inlay-hint", path | rest], state) do
    range = List.first(rest)
    args = if range, do: %{"range" => range}, else: %{}
    perform(path, "inlay-hint", args)
    state
  end

  defp dispatch(["semantic-tokens", path], state) do
    perform(path, "semantic-tokens", %{})
    state
  end

  defp dispatch(["document-symbol", path], state) do
    perform(path, "document-symbol", %{})
    state
  end

  defp dispatch(["workspace-symbol", path, query], state) do
    perform(path, "workspace-symbol", %{"query" => query})
    state
  end

  defp dispatch(["diagnostics", path | rest], state) do
    emit_diagnostics(path, normalize_map(List.first(rest)))
    state
  end

  defp dispatch(["list-diagnostics", path | rest], state) do
    emit_diagnostics(path, normalize_map(List.first(rest)))
    state
  end

  defp dispatch(["shutdown", path], state) do
    with_session(path, fn pid -> Session.shutdown(pid) end)
    state
  end

  defp dispatch(["start-server", name | rest], state) do
    project_path = List.first(rest) || File.cwd!()
    start_raw_server(name, project_path)
    state
  end

  defp dispatch(["stop-server", name], state) do
    stop_raw_servers(name)
    state
  end

  defp dispatch(unknown, state) do
    Logger.debug("[LspBridge.ClientManager] unknown command: #{inspect(unknown)}")
    state
  end

  # ===========================================================================
  # Session lifecycle
  # ===========================================================================

  defp open_in_session(selection, path, content, opts) do
    session_opts = %{
      multi: selection.multi,
      server_infos: selection.servers,
      owner: self(),
      exec_path: Map.get(opts, "exec-path", []),
      diag_idle: Map.get(opts, "diag-idle"),
      hide_severities: Map.get(opts, "hide-severities")
    }

    case Session.ensure(selection.root, selection.profile, session_opts) do
      {:ok, pid} ->
        case safe_session(&Session.open_file(&1, path, content, opts), pid) do
          {:ok, servers} ->
            emacs(form("exhub-lsp-ready", [Elisp.string(path), Elisp.json(servers)]))

          other ->
            emacs_error("cannot open #{path}: #{inspect(other)}")
        end

      {:error, reason} ->
        emacs_error("cannot start session for #{path}: #{inspect(reason)}")
    end
  end

  defp perform(path, command, args) do
    case Session.find_by_path(path) do
      {:ok, pid} ->
        safe_session(fn p -> Session.perform(p, path, command, args) end, pid)

      :error ->
        emacs_error("no session for #{path}")
    end
  end

  defp with_session(path, fun) do
    case Session.find_by_path(path) do
      {:ok, pid} ->
        safe_session(fun, pid)

      :error ->
        Logger.debug("[LspBridge.ClientManager] no session for #{path}")
        :ignore
    end
  end

  # A language server can die mid-call; the resulting `exit' would otherwise
  # crash the ClientManager and take the whole LspBridge subtree with it.
  defp safe_session(fun, pid) do
    fun.(pid)
  catch
    :exit, reason ->
      Logger.debug("[LspBridge.ClientManager] session call exited: #{inspect(reason)}")
      {:error, reason}
  end

  defp emit_diagnostics(path, opts) do
    case Session.find_by_path(path) do
      {:ok, pid} ->
        case safe_session(&Session.diagnostics(&1, path, opts), pid) do
          {:ok, diagnostics} ->
            emacs(
              form("exhub-lsp-diagnostics", [
                Elisp.string(path),
                Elisp.json(diagnostics),
                Integer.to_string(length(diagnostics))
              ])
            )

          other ->
            emacs_error("diagnostics for #{path}: #{inspect(other)}")
        end

      :error ->
        emacs_error("no session for #{path}")
    end
  end

  # ===========================================================================
  # Raw server control (debugging)
  # ===========================================================================

  defp start_raw_server(name, project_path) do
    case Config.for_name(name) do
      {:ok, info} ->
        key = Server.key(project_path, name)

        spec = %{
          id: key,
          start: {Server, :start_link, [info, project_path, [owner: self(), key: key]]},
          restart: :temporary
        }

        case DynamicSupervisor.start_child(@server_sup, spec) do
          {:ok, _pid} ->
            :ok

          {:error, reason} ->
            emacs_error("failed to start #{name}: #{inspect(reason)}")
        end

      {:error, :not_found} ->
        emacs_error("unknown langserver: #{name}")
    end
  end

  defp stop_raw_servers(name) do
    @registry
    |> Registry.select([{{{:server, :_, name}, :"$1", :_}, [], [:"$1"]}])
    |> Enum.each(&DynamicSupervisor.terminate_child(@server_sup, &1))
  end

  # ===========================================================================
  # Emacs helpers
  # ===========================================================================

  defp emacs(text), do: Exhub.send_message(text)

  defp emacs_error(message) do
    emacs(form("exhub-lsp-error", [Elisp.string(message)]))
  end

  defp form(name, args), do: Elisp.form(name, args)
  # Render a handler payload (`Exhub.LspBridge.Handler.payload/0`) as the elisp
  # form the front end evaluates.
  defp render({:hover, path, markdown}) do
    form("exhub-lsp--hover", [Elisp.string(path), Elisp.string(markdown)])
  end

  defp render({:locations, path, kind, locations}) do
    form("exhub-lsp--locations", [Elisp.string(path), Elisp.string(kind), Elisp.json(locations)])
  end

  defp render({:symbols, path, symbols}) do
    form("exhub-lsp--symbols", [Elisp.string(path), Elisp.json(symbols)])
  end

  defp render({:workspace_symbols, query, symbols}) do
    form("exhub-lsp--workspace-symbols", [Elisp.string(query), Elisp.json(symbols)])
  end

  defp render({:signature_help, path, result}) do
    form("exhub-lsp--signature-help", [Elisp.string(path), Elisp.json(result)])
  end

  defp render({:completion, path, server, candidates, items, meta}) do
    form("exhub-lsp--completion", [
      Elisp.string(path),
      Elisp.string(server),
      Elisp.json(candidates),
      Elisp.json(items),
      Elisp.json(meta)
    ])
  end

  defp render({:completion_doc, path, server, key, documentation, edits}) do
    form("exhub-lsp--completion-doc", [
      Elisp.string(path),
      Elisp.string(server),
      Elisp.string(key),
      Elisp.string(documentation),
      Elisp.json(edits)
    ])
  end

  defp render({:rename_range, path, range}) do
    form("exhub-lsp--rename-range", [Elisp.string(path), Elisp.json(range)])
  end

  defp render({:workspace_edit, edit, message}) do
    form("exhub-lsp--workspace-edit", [Elisp.json(edit), Elisp.string(message)])
  end

  defp render({:format, path, edits}) do
    form("exhub-lsp--format", [Elisp.string(path), Elisp.json(edits)])
  end

  defp render({:code_actions, path, actions}) do
    form("exhub-lsp--code-actions", [Elisp.string(path), Elisp.json(actions)])
  end

  defp render({:call_hierarchy_items, path, items}) do
    form("exhub-lsp--call-hierarchy-items", [Elisp.string(path), Elisp.json(items)])
  end

  defp render({:call_hierarchy, path, direction, calls}) do
    form("exhub-lsp--call-hierarchy", [
      Elisp.string(path),
      Elisp.string(direction),
      Elisp.json(calls)
    ])
  end

  defp render({:inlay_hints, path, hints}) do
    form("exhub-lsp--inlay-hints", [Elisp.string(path), Elisp.json(hints)])
  end

  defp render({:semantic_tokens, path, tokens}) do
    form("exhub-lsp--semantic-tokens", [Elisp.string(path), Elisp.json(tokens)])
  end

  defp render({:message, message}) do
    form("exhub-lsp-message", [Elisp.string(message)])
  end

  defp render({:error, message}) do
    form("exhub-lsp-error", [Elisp.string(message)])
  end

  defp render(_other), do: form("exhub-lsp-message", [Elisp.string("")])

  defp normalize_map(nil), do: %{}
  defp normalize_map(map) when is_map(map), do: map
  defp normalize_map(list) when is_list(list), do: Map.new(list)
  defp normalize_map(_other), do: %{}

  defp describe(reason) when is_binary(reason), do: reason
  defp describe({:unsupported_single_file, name}), do: "#{name} does not support single files"

  defp describe({:no_server_for_language, language_id}),
    do: "no server for language #{language_id}"

  defp describe({:unknown_server, name}), do: "unknown langserver: #{name}"
  defp describe({:ignored_path, path}), do: "ignored path #{path}"
  defp describe(reason), do: inspect(reason)
end
