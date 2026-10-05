defmodule Exhub.LspBridge.Server do
  @moduledoc """
  One running language server process.

  This is the Elixir/OTP port of lsp-bridge's `core/lspserver.py::LspServer`.
  Where the Python original needs three threads (a `LspServerSender`, a
  `LspServerReceiver`, and an `lsp_message_dispatcher`) coordinated through a
  shared queue, a single `GenServer` covers all of it:

    * **spawn / own** the external LSP process via an Erlang `Port`
      (`:exit_status` so we also reap the OS exit code);
    * **send** — encode framed JSON-RPC to the port (replaces the sender thread);
    * **receive** — read raw bytes from the port, feed them through
      `Exhub.LspBridge.Protocol.decode/1`, dispatch each frame (replaces the
      receiver + dispatcher threads);
    * **correlate** — track outgoing request ids and their caller so responses
      are routed back (replaces `record_request_id`).

  ## Identity

  A server is registered as `{:server, project_path, name}` — the same
  *one process per project per server* rule as lsp-bridge's
  `project_path#server_name` key. Requests and notifications are addressed by
  that key via `request/4` / `notify/3`.

  ## Lifecycle

  Started under `Exhub.LspBridge.Application`'s `DynamicSupervisor`, owned by
  its `Exhub.LspBridge.Session` (`:owner`). The `initialize` handshake runs
  asynchronously in `handle_continue/3`. Requests **and notifications** issued
  before the handshake completes are queued and flushed, in order, once the
  server is `initialized` — mirroring lsp-bridge's `init_queue` vs `queue`
  split.

  ## Server-initiated requests

  Servers ask the client for things during startup (`workspace/configuration`,
  `client/registerCapability`, `window/workDoneProgress/create`) and may block
  until answered. See `default_client_response/3`.
  """

  use GenServer
  require Logger

  alias Exhub.LspBridge.{Capabilities, Config, Document, Protocol}

  @client_capabilities %{
    "workspace" => %{
      "configuration" => true,
      "didChangeConfiguration" => %{"dynamicRegistration" => true},
      "workspaceFolders" => true,
      "applyEdit" => true
    },
    "textDocument" => %{
      "synchronization" => %{
        "dynamicRegistration" => false,
        "willSave" => false,
        "didSave" => true
      },
      "completion" => %{
        "completionItem" => %{
          "snippetSupport" => true,
          "documentationFormat" => ["markdown", "plaintext"],
          "resolveSupport" => %{
            "properties" => ["documentation", "detail", "additionalTextEdits"]
          }
        }
      },
      "hover" => %{"contentFormat" => ["markdown", "plaintext"]},
      "publishDiagnostics" => %{"relatedInformation" => true},
      "signatureHelp" => %{
        "signatureInformation" => %{"documentationFormat" => ["markdown", "plaintext"]}
      }
    },
    "window" => %{"workDoneProgress" => true}
  }

  @type key :: {:server, String.t(), String.t()}

  # ===========================================================================
  # Public API
  # ===========================================================================

  @doc "Registry key for a server in `project_path` with the given name."
  @spec key(String.t(), String.t()) :: key()
  def key(project_path, name), do: {:server, project_path, name}

  def start_link(%Config{} = info, project_path, opts \\ []) do
    key = Keyword.get_lazy(opts, :key, fn -> key(project_path, info.name) end)

    GenServer.start_link(__MODULE__, {key, info, project_path, opts}, name: via(key))
  end

  defp via(key), do: {:via, Registry, {Exhub.LspBridge.Registry, key}}

  @doc """
  Send a JSON-RPC request. `from` receives `{:lsp_response, name, id, result}`
  or `{:lsp_error, name, id, reason}` when the correlated response arrives.
  Returns the assigned request id immediately.
  """
  def request(key, method, params, from \\ nil) do
    GenServer.call(via(key), {:request, method, params, from})
  end

  @doc "Send a JSON-RPC notification (fire-and-forget)."
  def notify(key, method, params) do
    GenServer.cast(via(key), {:notify, method, params})
  end

  @doc "Derived capabilities once the handshake has completed."
  def capabilities(key), do: GenServer.call(via(key), :capabilities)

  @doc "Whether the `initialize` handshake has completed."
  def initialized?(key), do: GenServer.call(via(key), :initialized?)

  @doc "Gracefully shut the server down (shutdown request + exit notification)."
  def shutdown(key), do: GenServer.cast(via(key), :shutdown_server)

  # ===========================================================================
  # GenServer callbacks
  # ===========================================================================

  @impl true
  def init({key, info, project_path, opts}) do
    state = %{
      key: key,
      info: info,
      project_path: project_path,
      owner: Keyword.get(opts, :owner),
      exec_path: Keyword.get(opts, :exec_path, []),
      port: nil,
      buffer: "",
      next_id: 1,
      # id -> from | :internal
      pending: %{},
      capabilities: %Capabilities{},
      initialized: false,
      # request id of the in-flight `initialize` handshake (nil once done)
      initialize_id: nil,
      # notifications + requests issued before `initialized`, written on flush
      outbox: []
    }

    case open_port(info, project_path, opts) do
      {:ok, port} ->
        {:ok, %{state | port: port}, {:continue, :initialize}}

      {:error, reason} ->
        {:stop, {:port_open_failed, reason}, state}
    end
  end

  defp open_port(info, project_path, opts) do
    # `{:spawn_executable, exe}` makes the OS use `exe` as argv[0]; `:args` must
    # therefore carry only the arguments that follow it. Do NOT use
    # `Config.command_args/1` here — it prepends the executable, which the child
    # would then receive as its first positional argument (for `elixir` that
    # means "run this file", i.e. the shell wrapper itself).
    with {:ok, executable} <- resolve_command(info.command, opts) do
      try do
        port =
          Port.open(
            {:spawn_executable, executable},
            [
              :binary,
              :use_stdio,
              :exit_status,
              :hide,
              {:args, info.args},
              {:cd, working_dir(project_path)}
            ]
          )

        {:ok, port}
      rescue
        e -> {:error, Exception.message(e)}
      end
    end
  end

  # Erlang's `spawn_executable` requires an absolute path; lsp-bridge resolves it
  # via `which` (merging Emacs `exec-path`). We do the same, honouring an
  # optional `:exec_path` from the command.
  defp resolve_command(nil, _opts), do: {:error, :no_command}

  defp resolve_command(command, opts) do
    if Path.type(command) == :absolute do
      {:ok, command}
    else
      case find_in_path(command, Keyword.get(opts, :exec_path, [])) do
        nil -> {:error, {:command_not_found, command}}
        path -> {:ok, path}
      end
    end
  end

  defp find_in_path(command, extra_dirs) do
    extra =
      extra_dirs
      |> List.wrap()
      |> Enum.map(&Path.join(&1, command))
      |> Enum.find(&File.regular?/1)

    extra || System.find_executable(command)
  end

  defp working_dir(project_path) do
    if File.dir?(project_path), do: project_path, else: Path.dirname(project_path)
  end

  @impl true
  def handle_continue(:initialize, state) do
    params = %{
      "processId" => System.pid() |> String.to_integer(),
      "clientInfo" => %{"name" => "exhub-lsp-bridge", "version" => "0.1.0"},
      "rootPath" => state.project_path,
      "rootUri" => Document.uri(state.project_path),
      "capabilities" => @client_capabilities,
      "initializationOptions" => state.info.settings,
      "workspaceFolders" => [
        %{"uri" => Document.uri(state.project_path), "name" => Path.basename(state.project_path)}
      ]
    }

    # Send directly (not queued): the outbox cannot flush until the handshake
    # completes, and the handshake cannot complete until this is sent.
    id = state.next_id
    envelope = %{"id" => id, "method" => "initialize", "params" => params}

    state = %{
      state
      | next_id: id + 1,
        pending: Map.put(state.pending, id, :internal),
        initialize_id: id
    }

    {:noreply, write(envelope, state)}
  end

  @impl true
  def handle_call({:request, method, params, from}, _from, state) do
    caller = from || self()

    if state.initialized do
      {id, state} = send_request(method, params, caller, state)
      {:reply, id, state}
    else
      {id, state} = queue_request(method, params, caller, state)
      {:reply, id, state}
    end
  end

  def handle_call(:capabilities, _from, state) do
    {:reply, state.capabilities, state}
  end

  def handle_call(:initialized?, _from, state) do
    {:reply, state.initialized, state}
  end

  @impl true
  def handle_cast({:notify, method, params}, state) do
    {:noreply, send_notification(method, params, state)}
  end

  def handle_cast(:shutdown_server, state) do
    {_id, state} = send_request("shutdown", %{}, :internal, state)
    state = send_notification("exit", %{}, state)
    {:noreply, state}
  end

  @impl true
  def handle_info({port_ref, {:data, chunk}}, %{port: port_ref} = state) do
    {frames, buffer} = Protocol.decode(state.buffer <> chunk)

    state = %{state | buffer: buffer}
    state = Enum.reduce(frames, state, &handle_frame(&1, &2))
    {:noreply, state}
  end

  def handle_info({port_ref, {:exit_status, status}}, %{port: port_ref} = state) do
    Logger.info("[LspBridge.Server] #{state.info.name} exited with status #{status}")
    {:stop, {:normal, status}, state}
  end

  def handle_info(_other, state), do: {:noreply, state}

  @impl true
  def terminate(_reason, %{port: port}) when is_port(port) do
    try do
      Port.close(port)
    catch
      _, _ -> :ok
    end
  end

  def terminate(_reason, _state), do: :ok

  # ===========================================================================
  # Frame handling
  # ===========================================================================

  defp handle_frame(msg, state) do
    cond do
      Map.has_key?(msg, "method") && Map.has_key?(msg, "id") ->
        handle_server_request(msg, state)

      Map.has_key?(msg, "method") ->
        handle_server_notification(msg, state)

      Map.has_key?(msg, "id") ->
        handle_response(msg, state)

      true ->
        state
    end
  end

  defp handle_response(%{"id" => id} = msg, state) do
    case Map.pop(state.pending, id) do
      {nil, _} ->
        state

      {from, pending} ->
        state = %{state | pending: pending}

        if state.initialize_id == id do
          complete_initialize(msg, state)
        else
          deliver(from, reply_for(state.info.name, id, msg))
          state
        end
    end
  end

  # The `initialize` response landed: cache capabilities, tell the server we are
  # ready and push its settings, then flush anything queued during the handshake.
  defp complete_initialize(%{"result" => result}, state) do
    capabilities = Capabilities.from_initialize(result, state.info.settings || %{})
    state = %{state | capabilities: capabilities, initialize_id: nil}

    state = write(%{"method" => "initialized", "params" => %{}}, state)

    state =
      write(
        %{
          "method" => "workspace/didChangeConfiguration",
          "params" => %{"settings" => state.info.settings || %{}}
        },
        state
      )

    state = %{state | initialized: true}
    notify_owner(state, {:lsp_initialized, state.key, capabilities})
    flush_outbox(state)
  end

  defp complete_initialize(_error, state), do: %{state | initialize_id: nil}

  defp reply_for(name, id, %{"error" => error}), do: {:lsp_error, name, id, error}
  defp reply_for(name, id, msg), do: {:lsp_response, name, id, msg["result"]}

  defp handle_server_notification(msg, state) do
    notify_owner(state, {:lsp_notification, state.info.name, msg["method"], msg["params"]})
    state
  end

  defp handle_server_request(%{"id" => id, "method" => method} = msg, state) do
    result = default_client_response(method, msg["params"] || %{}, state.info)
    send_response(id, result, state)
  end

  # Sane defaults so servers don't block on client requests during startup.
  defp default_client_response("workspace/configuration", params, info) do
    settings = info.settings || %{}
    items = params["items"] || []

    # Some servers (e.g. zls) crash on an empty list, so answer with one entry
    # per request item — `nil` when nothing is configured.
    if map_size(settings) == 0 do
      Enum.map(items, fn _ -> nil end)
    else
      Enum.map(items, fn item ->
        section = item["section"] || info.name
        Map.get(settings, section, %{})
      end)
    end
  end

  defp default_client_response("window/workDoneProgress/create", _params, _info), do: nil
  defp default_client_response("workspace/applyEdit", _params, _info), do: %{"applied" => true}
  defp default_client_response("client/registerCapability", _params, _info), do: nil
  defp default_client_response("client/unregisterCapability", _params, _info), do: nil
  defp default_client_response(_method, _params, _info), do: nil

  # ===========================================================================
  # Sending
  # ===========================================================================

  # Write immediately (when initialized), recording id -> caller; otherwise
  # queue the frame for the post-handshake flush.
  defp send_request(method, params, from, state) do
    id = state.next_id
    envelope = %{"id" => id, "method" => method, "params" => params}
    state = %{state | next_id: id + 1, pending: Map.put(state.pending, id, from)}
    {id, enqueue(envelope, state)}
  end

  # Assign an id but defer the write until `initialize` completes.
  defp queue_request(method, params, from, state) do
    id = state.next_id
    envelope = %{"id" => id, "method" => method, "params" => params}

    state = %{
      state
      | next_id: id + 1,
        pending: Map.put(state.pending, id, from),
        outbox: state.outbox ++ [envelope]
    }

    {id, state}
  end

  defp send_notification(method, params, state) do
    enqueue(%{"method" => method, "params" => params}, state)
  end

  defp send_response(id, result, state) do
    write(%{"id" => id, "result" => result}, state)
  end

  defp enqueue(envelope, %{initialized: true} = state), do: write(envelope, state)
  defp enqueue(envelope, state), do: %{state | outbox: state.outbox ++ [envelope]}

  defp flush_outbox(state) do
    Enum.reduce(state.outbox, %{state | outbox: []}, &write/2)
  end

  defp write(envelope, %{port: port} = state) when is_port(port) do
    case Protocol.encode(envelope) do
      {:ok, iodata} ->
        Port.command(port, IO.iodata_to_binary(iodata))
        state

      {:error, reason} ->
        Logger.warning("[LspBridge.Server] encode failed: #{inspect(reason)}")
        state
    end
  end

  defp write(_envelope, state), do: state

  # ===========================================================================
  # Delivery helpers
  # ===========================================================================

  defp deliver(nil, _msg), do: :ok
  defp deliver(:internal, _msg), do: :ok
  defp deliver(pid, msg) when is_pid(pid), do: send(pid, msg)
  defp deliver(_other, _msg), do: :ok

  defp notify_owner(%{owner: pid}, msg) when is_pid(pid), do: send(pid, msg)
  defp notify_owner(_state, _msg), do: :ok
end
