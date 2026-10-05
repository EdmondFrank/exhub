defmodule Exhub.LspBridge.Session do
  @moduledoc """
  One project + server-profile session: the open documents and their language
  servers.

  The Elixir/OTP consolidation of lsp-bridge's `core/fileaction.py::FileAction`
  and the per-server `LspServer.attach` bookkeeping. A session is keyed by
  `{project_path, profile}` where `profile` is a single server name
  (`{:single, "elixirLS"}`) or a multi-server profile name
  (`{:multi, "pyright_ruff"}`), and it owns:

    * one `Exhub.LspBridge.Server` child per server in the profile (started
      eagerly at session start, under `Exhub.LspBridge.Supervisor`);
    * the open `Exhub.LspBridge.Document`s (`filepath` → struct), including
      their per-server diagnostics caches and versions;
    * each server's derived `Exhub.LspBridge.Capabilities`, learned from the
      `{:lsp_initialized, …}` message the server sends its owner.

  Emacs drives the lifecycle with `open_file/4`, `change_file/3`, `save_file/2`
  and `close_file/2`; diagnostics arrive from the servers (push) and are pulled
  on change (`textDocument/diagnostic`, gated on the server's capability). Both
  are debounced and delivered to the session's `:owner` — the
  `Exhub.LspBridge.ClientManager` — as `{:lsp_diagnostics_update, path, list,
  count}` for rendering in Emacs. Non-diagnostic server notifications, readiness
  and errors are likewise reported to the owner.
  """

  use GenServer
  require Logger

  alias Exhub.LspBridge.{
    Capabilities,
    Config,
    Diagnostics,
    Document,
    Handlers,
    MultiServer,
    Server
  }

  @registry Exhub.LspBridge.Registry
  @server_sup Exhub.LspBridge.Supervisor
  @session_sup Exhub.LspBridge.SessionSupervisor

  @default_diag_idle 400
  @default_max_diagnostics 100
  @call_timeout 30_000

  # Stop a session's language servers after this much inactivity; they restart
  # lazily on the next edit or feature request. `:infinity` disables it.
  @default_idle_stop_ms 300_000

  # ===========================================================================
  # Public API
  # ===========================================================================

  @doc "Registry key for a session."
  @spec session_key(String.t(), term()) :: {:session, String.t(), term()}
  def session_key(project_path, profile), do: {:session, project_path, profile}

  def start_link(project_path, profile, opts \\ []) do
    GenServer.start_link(__MODULE__, {project_path, profile, opts},
      name: via(project_path, profile)
    )
  end

  defp via(project_path, profile) do
    {:via, Registry, {@registry, session_key(project_path, profile)}}
  end

  @doc "Find-or-start the session for `{project_path, profile}`."
  @spec ensure(String.t(), term(), keyword() | map()) :: {:ok, pid()} | {:error, term()}
  def ensure(project_path, profile, opts) do
    key = session_key(project_path, profile)

    case Registry.lookup(@registry, key) do
      [{pid, _}] ->
        {:ok, pid}

      [] ->
        spec = %{
          id: {__MODULE__, key},
          start: {__MODULE__, :start_link, [project_path, profile, opts]},
          restart: :temporary
        }

        case DynamicSupervisor.start_child(@session_sup, spec) do
          {:ok, pid} -> {:ok, pid}
          {:error, {:already_started, pid}} -> {:ok, pid}
          {:error, reason} -> {:error, reason}
        end
    end
  end

  @doc "The session owning `filepath`, if its document is open."
  @spec find_by_path(String.t()) :: {:ok, pid()} | :error
  def find_by_path(path) do
    case Registry.lookup(@registry, {:doc, Path.expand(path)}) do
      [{pid, _}] -> {:ok, pid}
      [] -> :error
    end
  end

  def open_file(pid, path, content, opts \\ %{}) do
    GenServer.call(pid, {:open_file, path, content, opts}, @call_timeout)
  end

  def change_file(pid, path, change) do
    GenServer.call(pid, {:change_file, path, change}, @call_timeout)
  end

  @doc """
  Replace a document's mirrored content wholesale and notify its servers.

  Used after Emacs applies a `WorkspaceEdit`/`TextEdit` under
  `inhibit-modification-hooks`, which suppresses the incremental
  `after-change-functions` sync. The change is sent as a full-text
  `TextDocumentContentChangeEvent` (no `range`), which is valid for both full-
  and incremental-sync servers.
  """
  def update_file(pid, path, content) do
    GenServer.call(pid, {:update_file, path, content}, @call_timeout)
  end

  def save_file(pid, path), do: GenServer.call(pid, {:save_file, path}, @call_timeout)
  def close_file(pid, path), do: GenServer.call(pid, {:close_file, path}, @call_timeout)

  def change_cursor(pid, path, position),
    do: GenServer.cast(pid, {:change_cursor, path, position})

  def request(pid, path, method, params, from) do
    GenServer.call(pid, {:request, path, method, params, from}, @call_timeout)
  end

  def notify(pid, path, method, params) do
    GenServer.call(pid, {:notify, path, method, params}, @call_timeout)
  end

  @doc "Run the read-only feature `command` for the document at `path`."
  def perform(pid, path, command, args) do
    GenServer.call(pid, {:perform, path, command, args}, @call_timeout)
  end

  def diagnostics(pid, path, opts \\ []) do
    GenServer.call(pid, {:diagnostics, path, opts}, @call_timeout)
  end

  def shutdown(pid), do: GenServer.call(pid, :shutdown, @call_timeout)

  # ===========================================================================
  # GenServer callbacks
  # ===========================================================================

  @impl true
  def init({project_path, profile, opts}) do
    opts = normalize_opts(opts)

    state = %{
      project_path: project_path,
      profile: profile,
      multi: Map.get(opts, :multi, false),
      owner: Map.get(opts, :owner),
      exec_path: Map.get(opts, :exec_path, []),
      servers: %{},
      server_infos: %{},
      capabilities: %{},
      pending: MapSet.new(),
      documents: %{},
      diag_idle: Map.get(opts, :diag_idle) || @default_diag_idle,
      hide_severities: Map.get(opts, :hide_severities) || [],
      diag_timers: %{},
      pull_timers: %{},
      pull_requests: %{},
      pending_handlers: %{},
      cursor: %{},
      idle_timeout: idle_timeout(opts),
      last_activity: now_ms(),
      idle_timer: nil,
      servers_stopped: false
    }

    {state, errors} =
      start_servers(Map.get(opts, :server_infos, []), state)

    for {name, reason} <- errors do
      notify_owner(state, {:lsp_error, "failed to start #{name}: #{inspect(reason)}"})
    end

    {:ok, arm_idle(state)}
  end

  @impl true
  def handle_call({:open_file, path, content, opts}, _from, state) do
    path = Path.expand(path)
    state = state |> ensure_servers() |> touch()

    case Map.fetch(state.documents, path) do
      {:ok, _doc} ->
        {:reply, {:ok, Map.keys(state.servers)}, state}

      :error ->
        language_id = language_id(opts, state)
        doc = Document.new(path, content, language_id)
        register_document(path)
        doc = %{doc | servers: Map.keys(state.servers)}
        state = put_document(state, doc)

        state =
          broadcast(state, "textDocument/didOpen", fn _name -> Document.did_open_params(doc) end)

        state = schedule_pull(state, path)
        {:reply, {:ok, Map.keys(state.servers)}, state}
    end
  end

  def handle_call({:change_file, path, change}, _from, state) do
    path = Path.expand(path)
    state = state |> ensure_servers() |> touch()

    case Map.fetch(state.documents, path) do
      :error ->
        {:reply, {:error, :no_document}, state}

      {:ok, doc} ->
        doc = Document.apply_change(doc, change)
        state = put_document(state, doc)

        state =
          broadcast(state, "textDocument/didChange", fn name ->
            Document.did_change_params(doc, change, sync_kind(state, name))
          end)

        state = put_document(state, %{doc | version: doc.version + 1})
        state = schedule_pull(state, path)
        {:reply, :ok, state}
    end
  end

  def handle_call({:save_file, path}, _from, state) do
    path = Path.expand(path)
    state = state |> ensure_servers() |> touch()

    case Map.fetch(state.documents, path) do
      :error ->
        {:reply, {:error, :no_document}, state}

      {:ok, doc} ->
        state =
          broadcast(state, "textDocument/didSave", fn name ->
            Document.did_save_params(doc, save_include_text?(state, name))
          end)

        {:reply, :ok, schedule_pull(state, path)}
    end
  end

  def handle_call({:update_file, path, content}, _from, state) do
    path = Path.expand(path)
    state = state |> ensure_servers() |> touch()

    case Map.fetch(state.documents, path) do
      :error ->
        {:reply, {:error, :no_document}, state}

      {:ok, doc} ->
        doc = %{doc | content: content || ""}
        state = put_document(state, doc)

        state =
          broadcast(state, "textDocument/didChange", fn _name ->
            %{
              "textDocument" => %{"uri" => doc.uri, "version" => doc.version},
              "contentChanges" => [%{"text" => doc.content}]
            }
          end)

        state = put_document(state, %{doc | version: doc.version + 1})
        {:reply, :ok, schedule_pull(state, path)}
    end
  end

  def handle_call({:close_file, path}, _from, state) do
    path = Path.expand(path)

    case Map.pop(state.documents, path) do
      {nil, _documents} ->
        {:reply, :ok, state}

      {doc, documents} ->
        broadcast(state, "textDocument/didClose", fn _name -> Document.did_close_params(doc) end)
        Registry.unregister(@registry, {:doc, path})
        state = cancel_timers(state, path)
        state = %{state | documents: documents}

        if map_size(documents) == 0 do
          stop_servers(state)
          {:stop, :normal, :ok, state}
        else
          {:reply, :ok, state}
        end
    end
  end

  def handle_call({:request, path, method, params, from}, _from, state) do
    state = state |> ensure_servers() |> touch()
    params = ensure_text_document(params, path, method)

    ids =
      state
      |> target_servers(method)
      |> Enum.map(fn name ->
        id = Server.request(Server.key(state.project_path, name), method, params, from)
        {name, id}
      end)

    {:reply, {:ok, ids}, state}
  end

  def handle_call({:notify, path, method, params}, _from, state) do
    state = state |> ensure_servers() |> touch()
    params = ensure_text_document(params, path, method)

    for name <- target_servers(state, method) do
      Server.notify(Server.key(state.project_path, name), method, params)
    end

    {:reply, :ok, state}
  end

  def handle_call({:perform, path, command, args}, _from, state) do
    state = state |> ensure_servers() |> touch()
    {:reply, :ok, do_perform(state, path, command, args)}
  end

  def handle_call({:diagnostics, path, opts}, _from, state) do
    path = Path.expand(path)

    case Map.fetch(state.documents, path) do
      {:ok, doc} ->
        merged =
          Diagnostics.merge(doc,
            hide_severities: hide_severities(opts) || state.hide_severities,
            max: max_diagnostics(opts)
          )

        {:reply, {:ok, merged}, state}

      :error ->
        {:reply, {:error, :no_document}, state}
    end
  end

  def handle_call(:shutdown, _from, state) do
    stop_servers(state)
    {:stop, :normal, :ok, state}
  end

  @impl true
  def handle_cast({:change_cursor, path, _position}, state) do
    cursor = Map.put(state.cursor, Path.expand(path), System.monotonic_time())
    {:noreply, touch(%{state | cursor: cursor})}
  end

  @impl true
  def handle_info({:lsp_initialized, {:server, _project, name}, capabilities}, state) do
    state = %{
      state
      | capabilities: Map.put(state.capabilities, name, capabilities),
        pending: MapSet.delete(state.pending, name)
    }

    # Capabilities are only known now, so (re)schedule a pull for open documents
    # — an open_file that raced ahead of the handshake could not pull yet.
    state = Enum.reduce(Map.keys(state.documents), state, &schedule_pull(&2, &1))

    {:noreply, state}
  end

  def handle_info({:lsp_notification, name, "textDocument/publishDiagnostics", params}, state) do
    path = Document.path_from_uri(Map.get(params, "uri", ""))
    {:noreply, record_diagnostics(state, path, name, Map.get(params, "diagnostics", []))}
  end

  def handle_info({:lsp_notification, name, method, params}, state) do
    notify_owner(state, {:lsp_notification, name, method, params})
    {:noreply, state}
  end

  def handle_info({:lsp_response, name, id, result}, state) do
    case Map.pop(state.pull_requests, id) do
      {nil, _} ->
        {:noreply, handle_handler_response(state, id, result)}

      {{path, server}, pull_requests} ->
        state = %{state | pull_requests: pull_requests}

        case Diagnostics.from_pull_result(result) do
          nil -> {:noreply, state}
          items -> {:noreply, record_diagnostics(state, path, server || name, items)}
        end
    end
  end

  def handle_info({:lsp_error, _name, id, error}, state) do
    case Map.pop(state.pull_requests, id) do
      {nil, _} ->
        {:noreply, handle_handler_error(state, id, error)}

      {{_path, _server}, pull_requests} ->
        Logger.debug("[LspBridge.Session] pull diagnostic failed: #{inspect(error)}")
        {:noreply, %{state | pull_requests: pull_requests}}
    end
  end

  def handle_info({:push_diagnostics, path}, state) do
    state = %{state | diag_timers: Map.delete(state.diag_timers, path)}

    case Map.fetch(state.documents, path) do
      {:ok, doc} ->
        merged = Diagnostics.merge(doc, hide_severities: state.hide_severities)
        notify_owner(state, {:lsp_diagnostics_update, path, merged, Diagnostics.count(doc)})

      :error ->
        :ok
    end

    {:noreply, state}
  end

  def handle_info({:pull_diagnostics, path}, state) do
    state = %{state | pull_timers: Map.delete(state.pull_timers, path)}
    {:noreply, pull_diagnostics(state, path)}
  end

  def handle_info(:idle_check, state) do
    cond do
      is_nil(Map.get(state, :idle_timeout, @default_idle_stop_ms)) ->
        {:noreply, state}

      Map.get(state, :servers_stopped, false) ->
        {:noreply, state}

      now_ms() - Map.get(state, :last_activity, now_ms()) >=
        Map.get(state, :idle_timeout, @default_idle_stop_ms) and
          MapSet.size(state.pending) == 0 ->
        Logger.debug("[LspBridge.Session] idling out servers for #{state.project_path}")
        stop_servers(state)

        {:noreply,
         %{
           state
           | servers: %{},
             capabilities: %{},
             pending: MapSet.new(),
             servers_stopped: true,
             idle_timer: nil
         }}

      true ->
        {:noreply, arm_idle(state)}
    end
  end

  def handle_info(_other, state), do: {:noreply, state}

  @impl true
  def terminate(_reason, state) do
    stop_servers(state)
    :ok
  end

  # ===========================================================================
  # Servers
  # ===========================================================================

  defp start_servers(infos, state) do
    Enum.reduce(infos, {state, []}, fn info, {st, errors} ->
      key = Server.key(state.project_path, info.name)

      spec = %{
        id: key,
        start:
          {Server, :start_link,
           [info, state.project_path, [owner: self(), key: key, exec_path: state.exec_path]]},
        restart: :temporary
      }

      case DynamicSupervisor.start_child(@server_sup, spec) do
        {:ok, pid} ->
          state = %{
            st
            | servers: Map.put(st.servers, info.name, pid),
              server_infos: Map.put(st.server_infos, info.name, info),
              pending: MapSet.put(st.pending, info.name)
          }

          {state, errors}

        {:error, reason} ->
          {st, errors ++ [{info.name, reason}]}
      end
    end)
  end

  defp stop_servers(state) do
    for {_name, pid} <- state.servers do
      DynamicSupervisor.terminate_child(@server_sup, pid)
    end

    :ok
  end

  # Lazily (re)start this session's servers after an idle stop, re-opening every
  # document they already knew about. Capabilities are unknown until each
  # handshake completes, so pulls re-arm from `:lsp_initialized`.
  # Hot reload does not migrate a live GenServer's state, so read the new keys
  # with Map.get: a session started by the previous code must keep working until
  # its next activity re-touches it.
  defp ensure_servers(state) do
    if Map.get(state, :servers_stopped, false) do
      {state, errors} = start_servers(Map.values(state.server_infos), state)

      for {name, reason} <- errors do
        notify_owner(state, {:lsp_error, "failed to start #{name}: #{inspect(reason)}"})
      end

      state =
        Enum.reduce(Map.values(state.documents), state, fn doc, st ->
          broadcast(st, "textDocument/didOpen", fn _name -> Document.did_open_params(doc) end)
        end)

      # Keep `pending' as `start_servers/2' set it, so an idle check cannot reap
      # a server that is still completing its `initialize' handshake.
      %{state | servers_stopped: false, capabilities: %{}}
    else
      state
    end
  end

  defp touch(state) do
    state =
      state
      |> Map.put(:last_activity, now_ms())
      |> Map.put_new(:idle_timeout, @default_idle_stop_ms)
      |> Map.put_new(:idle_timer, nil)
      |> Map.put_new(:servers_stopped, false)

    arm_idle(state)
  end

  defp arm_idle(%{idle_timeout: timeout} = state) when is_integer(timeout) and timeout > 0 do
    if is_reference(state.idle_timer), do: Process.cancel_timer(state.idle_timer)
    ref = Process.send_after(self(), :idle_check, timeout)
    %{state | idle_timer: ref}
  end

  defp arm_idle(state), do: %{state | idle_timer: nil}

  defp now_ms, do: System.monotonic_time(:millisecond)

  defp idle_timeout(opts) do
    case Map.get(opts, :idle_stop) do
      :infinity -> nil
      ms when is_integer(ms) and ms > 0 -> ms
      _ -> @default_idle_stop_ms
    end
  end

  defp broadcast(state, method, params_fun) do
    for {name, _pid} <- state.servers do
      Server.notify(Server.key(state.project_path, name), method, params_fun.(name))
    end

    state
  end

  # ===========================================================================
  # Diagnostics
  # ===========================================================================

  defp record_diagnostics(state, path, server, diagnostics) do
    path = Path.expand(path)

    case Map.fetch(state.documents, path) do
      :error ->
        state

      {:ok, doc} ->
        doc = Diagnostics.record(doc, server, diagnostics)
        state |> put_document(doc) |> schedule_push(path)
    end
  end

  defp schedule_push(state, path) do
    {existing, timers} = Map.pop(state.diag_timers, path)
    if is_reference(existing), do: Process.cancel_timer(existing)
    ref = Process.send_after(self(), {:push_diagnostics, path}, state.diag_idle)
    %{state | diag_timers: Map.put(timers, path, ref)}
  end

  defp schedule_pull(state, path) do
    {existing, timers} = Map.pop(state.pull_timers, path)
    if is_reference(existing), do: Process.cancel_timer(existing)
    ref = Process.send_after(self(), {:pull_diagnostics, path}, state.diag_idle)
    %{state | pull_timers: Map.put(timers, path, ref)}
  end

  defp pull_diagnostics(%{servers_stopped: true} = state, _path), do: state

  defp pull_diagnostics(state, path) do
    case Map.fetch(state.documents, path) do
      :error ->
        state

      {:ok, _doc} ->
        state
        |> target_servers("textDocument/diagnostic")
        |> Enum.filter(&supports?(state, &1, "diagnostic"))
        |> Enum.reduce(state, fn name, st ->
          capabilities = Map.get(st.capabilities, name)
          identifier = capabilities && capabilities.diagnostic_identifier

          params =
            Diagnostics.pull_params(identifier || name, nil)
            |> Map.put("textDocument", %{"uri" => Document.uri(path)})

          id =
            Server.request(
              Server.key(st.project_path, name),
              "textDocument/diagnostic",
              params,
              self()
            )

          %{st | pull_requests: Map.put(st.pull_requests, id, {path, name})}
        end)
    end
  end

  # ===========================================================================
  # Handlers (read-only features)
  # ===========================================================================

  # Dispatch a read-only feature request: gate on the capability, fan the LSP
  # request out to the supporting servers and remember each request id so the
  # response can be routed back to its handler.
  defp do_perform(state, path, command, args) do
    path = Path.expand(path)
    args = normalize_args(args)

    with handler when not is_nil(handler) <- Handlers.fetch(command),
         {:ok, doc} <- Map.fetch(state.documents, path),
         targets when targets != [] <- handler_targets(state, handler, args) do
      handler_ctx = %{
        path: path,
        language_id: doc.language_id,
        args: args,
        version: doc.version,
        trigger_characters: trigger_characters(state, targets),
        server_names: targets,
        diagnostics: Diagnostics.all(doc),
        semantic_tokens_legend: semantic_token_legend(state, targets)
      }

      params = handler.request_params(args, handler_ctx)
      params = ensure_text_document(params, path, handler.method())
      at = System.monotonic_time()

      Enum.reduce(targets, state, fn name, st ->
        case safe_request(Server.key(st.project_path, name), handler.method(), params, self()) do
          id when is_integer(id) ->
            entry = %{
              handler: handler,
              path: path,
              at: at,
              args: args,
              ctx: handler_ctx,
              server: name
            }

            %{st | pending_handlers: Map.put(st.pending_handlers, id, entry)}

          _other ->
            st
        end
      end)
    else
      nil ->
        notify_owner(state, {:lsp_handler_error, "unknown command: #{command}"})
        state

      :error ->
        notify_owner(state, {:lsp_handler_error, "no open document for #{path}"})
        state

      [] ->
        notify_owner(state, {:lsp_handler_error, unsupported_message(command)})
        state
    end
  end

  defp handler_targets(state, handler, args) do
    provider = handler.provider()
    # `completion-item-resolve` must go back to the server that produced the
    # candidate; callers pin it with an explicit `server` argument.
    requested = Map.get(args, "server")

    state
    |> target_servers(handler.method())
    |> Enum.filter(fn name -> is_nil(requested) or requested == name end)
    |> Enum.filter(fn name ->
      # A server whose capabilities are not yet known is still initializing:
      # let the request through (the server queues it until `initialized`)
      # rather than rejecting it as unsupported. A `nil` provider means the
      # feature is never capability-gated (e.g. `workspace/executeCommand`).
      case {provider, Map.get(state.capabilities, name)} do
        {nil, _} -> true
        {_provider, nil} -> true
        {provider, capabilities} -> Capabilities.supports?(capabilities, provider)
      end
    end)
  end

  # Union of the target servers' advertised completion trigger characters —
  # a single fan-out request cannot carry per-server `context`.
  defp trigger_characters(state, targets) do
    targets
    |> Enum.flat_map(fn name ->
      case Map.get(state.capabilities, name) do
        %Capabilities{trigger_characters: chars} -> chars
        _ -> []
      end
    end)
    |> Enum.uniq()
  end

  # The legend of the first target server that advertises one, so semantic-token
  # handler responses can resolve type/modifier indices to names.
  defp semantic_token_legend(state, targets) do
    Enum.find_value(targets, fn name ->
      case Map.get(state.capabilities, name) do
        %Capabilities{semantic_tokens: %{"legend" => legend}} -> legend
        _ -> nil
      end
    end)
  end

  defp handle_handler_response(state, id, result) do
    case Map.pop(state.pending_handlers, id) do
      {nil, _} ->
        state

      {entry, pending} ->
        state = %{state | pending_handlers: pending}

        if stale?(state, entry) do
          state
        else
          base_ctx =
            Map.get(entry, :ctx) ||
              %{path: entry.path, language_id: entry.language_id, args: entry.args}

          handler_ctx = Map.put(base_ctx, :server, Map.get(entry, :server))

          case entry.handler.process_response(result, handler_ctx) do
            nil ->
              state

            payload ->
              notify_owner(state, {:lsp_handler_result, payload})
              state
          end
        end
    end
  end

  defp handle_handler_error(state, id, error) do
    case Map.pop(state.pending_handlers, id) do
      {nil, _} ->
        state

      {_entry, pending} ->
        state = %{state | pending_handlers: pending}
        notify_owner(state, {:lsp_handler_error, "request failed: #{inspect(error)}"})
        state
    end
  end

  # Drop a response whose document changed after the request was sent, matching
  # lsp-bridge's `cancel_on_change` + `last_change` guard.
  defp stale?(state, entry) do
    if entry.handler.cancel_on_change?() do
      case Map.get(state.documents, entry.path) do
        %Document{last_change: last} when is_integer(last) -> last > entry.at
        _ -> false
      end
    else
      false
    end
  end

  defp safe_request(key, method, params, from) do
    Server.request(key, method, params, from)
  catch
    :exit, reason ->
      Logger.debug("[LspBridge.Session] request to #{inspect(key)} exited: #{inspect(reason)}")
      {:error, reason}
  end

  defp unsupported_message(command) do
    "#{command} is not supported by the current server(s)"
  end

  defp normalize_args(args) when is_map(args), do: args
  defp normalize_args(args) when is_list(args), do: Map.new(args)
  defp normalize_args(_other), do: %{}

  defp cancel_timers(state, path) do
    {push, diag_timers} = Map.pop(state.diag_timers, path)
    {pull, pull_timers} = Map.pop(state.pull_timers, path)
    if is_reference(push), do: Process.cancel_timer(push)
    if is_reference(pull), do: Process.cancel_timer(pull)

    %{
      state
      | diag_timers: diag_timers,
        pull_timers: pull_timers,
        pull_requests: Map.reject(state.pull_requests, fn {_id, {p, _s}} -> p == path end),
        pending_handlers:
          Map.reject(state.pending_handlers, fn {_id, entry} -> entry.path == path end)
    }
  end

  # ===========================================================================
  # Helpers
  # ===========================================================================

  defp put_document(state, doc) do
    %{state | documents: Map.put(state.documents, doc.filepath, doc)}
  end

  defp register_document(path) do
    case Registry.lookup(@registry, {:doc, path}) do
      [] -> Registry.register(@registry, {:doc, path}, nil)
      _ -> :ok
    end
  end

  defp language_id(opts, state) do
    Map.get(opts, "language-id") || Map.get(opts, :language_id) ||
      case state.server_infos |> Map.values() |> List.first() do
        %Config{language_id: id} -> id
        _ -> ""
      end
  end

  defp sync_kind(state, name) do
    case Map.get(state.capabilities, name) do
      %Capabilities{sync_kind: kind} -> kind
      _ -> 2
    end
  end

  defp save_include_text?(state, name) do
    case Map.get(state.capabilities, name) do
      %Capabilities{save_include_text: flag} -> flag
      _ -> false
    end
  end

  defp supports?(state, name, capability) do
    case Map.get(state.capabilities, name) do
      %Capabilities{} = capabilities -> Capabilities.supports?(capabilities, capability)
      _ -> false
    end
  end

  # Server names to target for a method: for a multi-server profile, the profile's
  # ordered list (falling back to the default); otherwise every server.
  defp target_servers(%{profile: {:multi, profile}} = state, method) do
    case MultiServer.servers(profile, method) do
      [] -> Map.keys(state.servers)
      names -> Enum.filter(names, &Map.has_key?(state.servers, &1))
    end
  end

  defp target_servers(state, _method), do: Map.keys(state.servers)

  defp ensure_text_document(params, path, method) when is_map(params) do
    if String.starts_with?(method, "textDocument/") and not Map.has_key?(params, "textDocument") do
      Map.put(params, "textDocument", %{"uri" => Document.uri(path)})
    else
      params
    end
  end

  defp ensure_text_document(params, _path, _method), do: params

  defp hide_severities(opts) do
    case fetch_opt(opts, :hide_severities, "hide-severities") do
      nil -> nil
      list -> List.wrap(list)
    end
  end

  defp max_diagnostics(opts) do
    fetch_opt(opts, :max, "max") || @default_max_diagnostics
  end

  defp fetch_opt(opts, atom_key, _string_key) when is_list(opts), do: Keyword.get(opts, atom_key)

  defp fetch_opt(opts, atom_key, string_key),
    do: Map.get(opts, atom_key) || Map.get(opts, string_key)

  # `ensure/3` accepts either a keyword list or a map (ClientManager passes a
  # map; callers/tests may pass a keyword list) — normalize so `init/1` and the
  # server startup code can use `Map.get/3` uniformly.
  defp normalize_opts(opts) when is_list(opts), do: Map.new(opts)
  defp normalize_opts(opts) when is_map(opts), do: opts

  defp notify_owner(%{owner: pid}, msg) when is_pid(pid), do: send(pid, msg)
  defp notify_owner(_state, _msg), do: :ok
end
