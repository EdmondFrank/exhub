defmodule Exhub.KuriDaemon do
  @moduledoc """
  Supervised daemon that auto-starts the `kuri` HTTP server binary via Exile.

  Kuri is a Zig-based browser automation server that manages a Chrome instance
  via CDP and exposes an HTTP API (tabs, navigate, snapshot, action, etc.).
  This GenServer ensures kuri is running whenever Exhub boots, providing the
  browser backend for `kuri-agent` and the BrowserUse MCP tools.

  ## Robustness Features

  - **Deep health checks**: Verifies both HTTP liveness (`/health`) and Chrome/CDP
    functionality (`/tab/new` with auth). Detects "zombie" state where kuri is up but
    Chrome is dead.
  - **Auto-recovery**: After 3 consecutive failed deep checks, kills and restarts kuri.
  - **Exponential backoff**: Restart delay grows 5s → 10s → 20s → 40s (cap 60s),
    resets on successful startup.
  - **Proper process management**: Tracks the OS pid via Exile port for clean SIGTERM.

  ## Duplicate Server Prevention

  Before starting a new kuri server, the daemon checks if a server is already
  running on the configured host:port. If a healthy kuri server is detected,
  the daemon will skip starting a new process and use the existing one instead.

  ## Configuration (application env)

  | Key                 | Default                         | Description                        |
  |---------------------|---------------------------------|------------------------------------|
  | `:kuri_enabled`     | `true`                          | Enable/disable the daemon          |
  | `:kuri_port`        | `18080`                         | HTTP listen port                   |
  | `:kuri_host`        | `"127.0.0.1"`                   | Bind address                       |
  | `:kuri_headless`    | `true`                          | Run Chrome headless                |
  | `:kuri_cdp_port`    | `9222`                          | Chrome CDP port                    |
  | `:kuri_binary`      | `nil` (auto-detect)             | Explicit path to `kuri` binary     |

  ## Binary resolution order

  1. `:kuri_binary` application env (if set)
  2. `System.find_executable("kuri")` (PATH)
  3. `~/Code/kuri/zig-out/bin/kuri` (local dev build)
  """

  use GenServer
  require Logger

  @default_port 18080
  @default_host "127.0.0.1"
  @default_cdp_port 9222
  @health_check_interval_ms 30_000
  @base_restart_delay_ms 5_000
  @max_restart_delay_ms 60_000
  @startup_timeout_ms 15_000
  @startup_poll_interval_ms 500
  @max_consecutive_failures 3

  # ── Client API ──────────────────────────────────────────────────────────────

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc "Returns `:healthy`, `:degraded`, `:unhealthy`, `:starting`, `:disabled`, or `:stopped`."
  def status do
    GenServer.call(__MODULE__, :status)
  end

  @doc "Returns the base URL of the running kuri server, e.g. `\"http://127.0.0.1:18080\"`."
  def base_url do
    GenServer.call(__MODULE__, :base_url)
  end

  @doc "Returns the current cached health status."
  def health do
    GenServer.call(__MODULE__, :health)
  end

  @doc """
  Returns the kuri API token for authenticating HTTP requests.

  The token is generated at startup and passed to the kuri process via
  the `KURI_API_TOKEN` environment variable. When an existing server is
  detected, the token is resolved from `~/.kuri/api.token` or env vars.
  """
  def api_token do
    GenServer.call(__MODULE__, :api_token)
  end

  # ── Server callbacks ────────────────────────────────────────────────────────

  @impl true
  def init(_opts) do
    # Ensure :inets is started for :httpc (health checks)
    :inets.start()
    :ssl.start()

    port = Application.get_env(:exhub, :kuri_port, @default_port)
    host = Application.get_env(:exhub, :kuri_host, @default_host)
    headless = Application.get_env(:exhub, :kuri_headless, true)
    cdp_port = Application.get_env(:exhub, :kuri_cdp_port, @default_cdp_port)
    enabled = Application.get_env(:exhub, :kuri_enabled, true)

    state = %{
      port: port,
      host: host,
      headless: headless,
      cdp_port: cdp_port,
      status: :stopped,
      binary: nil,
      daemon_ref: nil,
      daemon_pid: nil,
      os_pid: nil,
      chrome_pid: nil,
      token: generate_token(),
      health_timer: nil,
      restart_timer: nil,
      consecutive_failures: 0,
      restart_count: 0
    }

    if enabled do
      case resolve_binary() do
        nil ->
          Logger.warning(
            "[KuriDaemon] `kuri` binary not found — daemon disabled. " <>
              "Install kuri or set :exhub, :kuri_binary in config."
          )

          {:ok, %{state | status: :disabled}}

        binary ->
          Logger.info("[KuriDaemon] Found kuri binary at #{binary}")
          send(self(), :start_daemon)
          {:ok, %{state | status: :starting, binary: binary}}
      end
    else
      Logger.info("[KuriDaemon] Disabled via :exhub, :kuri_enabled = false")
      {:ok, %{state | status: :disabled}}
    end
  end

  @impl true
  def handle_call(:status, _from, state) do
    {:reply, state.status, state}
  end

  def handle_call(:base_url, _from, state) do
    {:reply, "http://#{state.host}:#{state.port}", state}
  end

  def handle_call(:health, _from, state) do
    # Reply immediately with current status; don't block on deep health check
    # The health check is performed asynchronously via :health_check messages
    result =
      case state.status do
        :healthy -> {:ok, :full}
        :degraded -> {:ok, :http_only}
        :starting -> {:error, :starting}
        :stopped -> {:error, :stopped}
        :disabled -> {:error, :disabled}
        _ -> {:error, state.status}
      end

    {:reply, result, state}
  end

  def handle_call(:api_token, _from, state) do
    {:reply, state.token, state}
  end

  @impl true
  def handle_info(:start_daemon, state) do
    Logger.info("[KuriDaemon] Starting kuri server on #{state.host}:#{state.port}...")

    parent = self()

    # Check if a server is already running on this port (non-blocking)
    # Use a Task to avoid blocking the GenServer during HTTP checks
    Task.start(fn ->
      case check_existing_server(state.host, state.port, state.token) do
        {:ok, _body, working_token} ->
          Logger.info(
            "[KuriDaemon] ✓ kuri server already running at http://#{state.host}:#{state.port}"
          )
          send(parent, {:existing_server_found, :healthy, working_token})

        {:error, {:port_in_use, status}} ->
          Logger.warning(
            "[KuriDaemon] Port #{state.port} is in use but returned status #{status}. " <>
              "Proceeding with caution — may conflict with existing service."
          )
          send(parent, {:existing_server_found, :port_in_use, nil})

        {:error, :no_server} ->
          send(parent, {:existing_server_found, :no_server, nil})
      end
    end)

    {:noreply, %{state | status: :starting}}
  end

  @impl true
  def handle_info({:existing_server_found, :healthy, working_token}, state) do
    timer = schedule_health_check()
    {:noreply, %{state | status: :healthy, token: working_token, health_timer: timer, consecutive_failures: 0}}
  end

  @impl true
  def handle_info({:existing_server_found, :port_in_use, _}, state) do
    do_start_new_daemon(state)
  end

  @impl true
  def handle_info({:existing_server_found, :no_server, _}, state) do
    do_start_new_daemon(state)
  end

  def handle_info({:DOWN, ref, :process, _pid, reason}, %{daemon_ref: ref} = state) do
    Logger.warning("[KuriDaemon] kuri process crashed: #{inspect(reason)}")

    schedule_restart(state)
  end

  def handle_info(:health_check, state) do
    # Run health check in a Task to avoid blocking the GenServer
    parent = self()

    Task.start(fn ->
      result = deep_health_check(state.host, state.port, state.token)
      send(parent, {:health_check_result, result})
    end)

    {:noreply, state}
  end

  @impl true
  def handle_info({:health_check_result, result}, state) do
    {new_status, new_failures} =
      case result do
        {:ok, :full} ->
          {:healthy, 0}

        {:ok, :http_only} ->
          # HTTP is up but Chrome/CDP is dead — degraded
          {:degraded, state.consecutive_failures + 1}

        {:error, _} ->
          {:unhealthy, state.consecutive_failures + 1}
      end

    if new_status != state.status do
      Logger.info("[KuriDaemon] Health status changed: #{state.status} → #{new_status}")
    end

    # Auto-restart after too many consecutive failures
    if new_failures >= @max_consecutive_failures and state.daemon_pid do
      Logger.warning(
        "[KuriDaemon] #{new_failures} consecutive failures — restarting kuri daemon"
      )

      kill_daemon(state)
      timer = Process.send_after(self(), :start_daemon, restart_delay(state.restart_count))

      {:noreply,
       %{
         state
         | status: :stopped,
           daemon_ref: nil,
           daemon_pid: nil,
           os_pid: nil,
           chrome_pid: nil,
           health_timer: nil,
           restart_timer: timer,
           consecutive_failures: 0,
           restart_count: state.restart_count + 1
       }}
    else
      timer = schedule_health_check()
      {:noreply, %{state | status: new_status, health_timer: timer, consecutive_failures: new_failures}}
    end
  end

  @impl true
  def handle_info({:wait_for_startup, ref, _pid, host, port}, %{daemon_ref: daemon_ref} = state)
      when ref == daemon_ref do
    parent = self()
    token = state.token

    Task.start(fn ->
      result = wait_for_startup(host, port, token)
      send(parent, {:startup_check_result, ref, result})
    end)

    {:noreply, state}
  end

  @impl true
  def handle_info({:startup_check_result, ref, result}, %{daemon_ref: daemon_ref} = state)
      when ref == daemon_ref do
    case result do
      :ok ->
        Logger.info("[KuriDaemon] ✓ kuri server is healthy at http://#{state.host}:#{state.port}")

        os_pid = find_pid_by_port(state.port)
        chrome_pid = find_pid_by_port(state.cdp_port)

        timer = schedule_health_check()

        {:noreply,
         %{
           state
           | status: :healthy,
             os_pid: os_pid,
             chrome_pid: chrome_pid,
             health_timer: timer,
             consecutive_failures: 0,
             restart_count: 0
         }}

      :timeout ->
        Logger.warning(
          "[KuriDaemon] kuri started but health check timed out after #{@startup_timeout_ms}ms"
        )

        timer = schedule_health_check()

        {:noreply,
         %{
           state
           | status: :unhealthy,
             health_timer: timer
         }}
    end
  end

  # Catch-all for stale startup check results from a previous daemon ref
  def handle_info({:startup_check_result, ref, _result}, state) do
    Logger.debug("[KuriDaemon] Ignoring stale :startup_check_result for ref #{inspect(ref)}")
    {:noreply, state}
  end

  # Catch-all for stale startup messages from a previous daemon ref
  def handle_info({:wait_for_startup, ref, _pid, _host, _port}, state) do
    Logger.debug("[KuriDaemon] Ignoring stale :wait_for_startup for ref #{inspect(ref)}")
    {:noreply, state}
  end

  defp do_start_new_daemon(state) do
    env = build_env(state)

    {pid, ref} =
      spawn_monitor(fn ->
        run_kuri(state.binary, env)
      end)

    # Store the ref and pid, then wait asynchronously
    send(self(), {:wait_for_startup, ref, pid, state.host, state.port})

    {:noreply, %{state | daemon_ref: ref, daemon_pid: pid, status: :starting}}
  end

  @impl true
  def terminate(_reason, state) do
    cancel_timer(state.health_timer)
    cancel_timer(state.restart_timer)
    kill_daemon(state)
    :ok
  end

  # ── Private helpers ─────────────────────────────────────────────────────────

  defp resolve_binary do
    explicit = Application.get_env(:exhub, :kuri_binary)

    dev_path = Path.join([System.user_home!(), "Code", "kuri", "zig-out", "bin", "kuri"])

    cond do
      is_binary(explicit) and File.exists?(explicit) ->
        explicit

      path = System.find_executable("kuri") ->
        path

      File.exists?(dev_path) ->
        dev_path

      true ->
        nil
    end
  end

  defp generate_token do
    :crypto.strong_rand_bytes(32)
    |> Base.encode16(case: :lower)
  end

  defp build_env(state) do
    # Start with filtered parent environment so Chrome has access to
    # HOME, PATH, DYLD_*, etc. Exile.stream(:env) replaces the entire
    # environment, so we must include essential vars explicitly.
    parent_env =
      System.get_env()
      |> Enum.reject(fn {k, _} ->
        String.starts_with?(k, "RELEASE") or k in ["PROGNAME", "ROOTDIR", "BINDIR"]
      end)
      |> Enum.reject(fn {k, _} ->
        k in ["HOST", "PORT", "HEADLESS", "KURI_API_TOKEN", "STATE_DIR", "CDP_PORT"]
      end)
      |> Enum.to_list()

    overrides = [
      {"HOST", state.host},
      {"PORT", Integer.to_string(state.port)},
      {"HEADLESS", to_string(state.headless)},
      {"KURI_API_TOKEN", state.token},
      {"STATE_DIR", Path.join([System.user_home!(), ".kuri"])},
      {"CDP_PORT", Integer.to_string(state.cdp_port)}
    ]

    (parent_env ++ overrides)
    |> Enum.reject(fn {_k, v} -> v == "" end)
  end

  defp run_kuri(binary, env) do
    Logger.info("[KuriDaemon] Executing: #{binary}")

    Exile.stream([binary], env: env, stderr: :consume)
    |> Enum.reduce(:ok, fn
      {:stdout, data}, acc ->
        data
        |> String.trim()
        |> String.split("\n")
        |> Enum.each(fn line -> Logger.debug("[kuri] #{line}") end)

        acc

      {:stderr, data}, acc ->
        data
        |> String.trim()
        |> String.split("\n")
        |> Enum.each(fn line -> Logger.warning("[kuri:err] #{line}") end)

        acc

      {:exit, {:status, code}}, _acc ->
        Logger.warning("[kuri] Process exited with status #{code}")
        {:exit, code}

      {:exit, :epipe}, _acc ->
        Logger.info("[kuri] Process exited (epipe)")
        {:exit, 0}

      _event, acc ->
        acc
    end)
  end

  defp wait_for_startup(host, port, token, elapsed \\ 0) do
    if elapsed >= @startup_timeout_ms do
      :timeout
    else
      case do_http_health_check(host, port, token) do
        {:ok, _} ->
          :ok

        {:error, reason} ->
          Logger.debug("[KuriDaemon] Startup poll failed (#{elapsed}ms): #{inspect(reason)}")
          :timer.sleep(@startup_poll_interval_ms)
          wait_for_startup(host, port, token, elapsed + @startup_poll_interval_ms)
      end
    end
  end

  @doc false
  # Deep health check: verifies HTTP liveness AND Chrome/CDP functionality.
  # Returns:
  #   {:ok, :full}       — both HTTP and CDP are working
  #   {:ok, :http_only}  — HTTP is up but CDP/Chrome is dead (degraded)
  #   {:error, reason}   — HTTP is unreachable
  defp deep_health_check(host, port, token) do
    case do_http_health_check(host, port, token) do
      {:ok, _body} ->
        # HTTP is up — now verify Chrome/CDP via /tab/new
        case do_tab_creation_check(host, port, token) do
          {:ok, _} -> {:ok, :full}
          {:error, _} -> {:ok, :http_only}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp do_http_health_check(host, port, token) do
    url = "http://#{host}:#{port}/health"

    headers =
      if token && token != "" do
        [{~c"authorization", to_charlist("Bearer #{token}")}]
      else
        []
      end

    case :httpc.request(:get, {to_charlist(url), headers}, [timeout: 5_000], body_format: :binary) do
      {:ok, {{_, 200, _}, _headers, body}} ->
        {:ok, body}

      {:ok, {{_, status, _}, _headers, body}} ->
        {:error, {:http_error, status, body}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Verifies Chrome/CDP is functional by creating a tab.
  # Returns {:ok, tab_id} or {:error, reason}.
  defp do_tab_creation_check(host, port, token, timeout \\ 5_000) do
    url = "http://#{host}:#{port}/tab/new"
    headers = [{~c"authorization", to_charlist("Bearer #{token}")}]

    case :httpc.request(:get, {to_charlist(url), headers}, [timeout: timeout], body_format: :binary) do
      {:ok, {{_, 200, _}, _headers, body}} ->
        # Successfully created a tab — Chrome/CDP is functional
        cleanup_created_tab(host, port, body, token)
        {:ok, body}

      {:ok, {{_, status, _}, _headers, body}} ->
        {:error, {:http_error, status, body}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Clean up the tab created during health check to avoid accumulating tabs.
  defp cleanup_created_tab(host, port, body, token) do
    case Jason.decode(body) do
      {:ok, %{"tab_id" => tab_id}} when is_binary(tab_id) and tab_id != "" ->
        close_url = "http://#{host}:#{port}/tab/close?tab_id=#{URI.encode_www_form(tab_id)}"
        headers = [{~c"authorization", to_charlist("Bearer #{token}")}]

        case :httpc.request(:get, {to_charlist(close_url), headers}, [timeout: 3_000],
               body_format: :binary
             ) do
          {:ok, _} ->
            :ok

          {:error, reason} ->
            Logger.warning("[KuriDaemon] Failed to close health-check tab: #{inspect(reason)}")
            :ok
        end

      _ ->
        :ok
    end
  end

  defp schedule_health_check do
    Process.send_after(self(), :health_check, @health_check_interval_ms)
  end

  defp schedule_restart(state) do
    cancel_timer(state.health_timer)
    delay = restart_delay(state.restart_count)

    Logger.info("[KuriDaemon] Scheduling restart in #{delay}ms (attempt #{state.restart_count + 1})")

    timer = Process.send_after(self(), :start_daemon, delay)

    {:noreply,
     %{
       state
       | status: :stopped,
         daemon_ref: nil,
         daemon_pid: nil,
         os_pid: nil,
         chrome_pid: nil,
         health_timer: nil,
         restart_timer: timer,
         restart_count: state.restart_count + 1
     }}
  end

  # Exponential backoff: 5s, 10s, 20s, 40s, capped at 60s
  defp restart_delay(restart_count) do
    delay = @base_restart_delay_ms * :math.pow(2, restart_count) |> round()
    min(delay, @max_restart_delay_ms)
  end

  defp cancel_timer(nil), do: :ok
  defp cancel_timer(ref), do: Process.cancel_timer(ref)

  # Kill the kuri daemon process and its managed Chrome instance.
  defp kill_daemon(%{daemon_pid: nil}), do: :ok

  defp kill_daemon(state) do
    if state.daemon_ref do
      Process.demonitor(state.daemon_ref, [:flush])
    end

    # Kill the managed Chrome process first (if known)
    if state.chrome_pid do
      kill_os_pid(state.chrome_pid, "Chrome")
    else
      # Fallback: try to find Chrome by CDP port
      case find_pid_by_port(state.cdp_port) do
        {:ok, pid} -> kill_os_pid(pid, "Chrome")
        :error -> :ok
      end
    end

    # Kill the kuri process
    killed =
      if state.os_pid do
        kill_os_pid(state.os_pid, "kuri")
        true
      else
        # Fallback: try to find by port
        case find_pid_by_port(state.port) do
          {:ok, os_pid} ->
            kill_os_pid(os_pid, "kuri")
            true

          :error ->
            false
        end
      end

    unless killed do
      # Fallback: kill the Elixir process (which should terminate Exile stream)
      if Process.alive?(state.daemon_pid) do
        Process.exit(state.daemon_pid, :kill)
      end
    end

    :ok
  end

  defp kill_os_pid(pid, name) when is_integer(pid) do
    Logger.info("[KuriDaemon] Sending SIGTERM to #{name} OS pid #{pid}")
    System.cmd("kill", ["-TERM", Integer.to_string(pid)], stderr_to_stdout: true)

    # Fallback to SIGKILL after a short delay if process is still alive
    Task.start(fn ->
      :timer.sleep(3_000)

      case System.cmd("kill", ["-0", Integer.to_string(pid)], stderr_to_stdout: true) do
        {_, 0} ->
          Logger.warning("[KuriDaemon] #{name} pid #{pid} still alive, sending SIGKILL")
          System.cmd("kill", ["-9", Integer.to_string(pid)], stderr_to_stdout: true)

        _ ->
          :ok
      end
    end)

    :ok
  end

  defp kill_os_pid(_, _), do: :ok

  # Find the OS pid of a process listening on the given port
  defp find_pid_by_port(port) do
    case System.cmd("lsof", ["-ti", ":#{port}"], stderr_to_stdout: true) do
      {output, 0} ->
        case output |> String.trim() |> String.split("\n") |> List.first() do
          nil -> :error
          "" -> :error
          pid_str ->
            case Integer.parse(pid_str) do
              {pid, _} -> {:ok, pid}
              :error -> :error
            end
        end

      _ ->
        :error
    end
  end

  defp check_existing_server(host, port, token) do
    url = "http://#{host}:#{port}/health"

    headers =
      if token && token != "" do
        [{~c"authorization", to_charlist("Bearer #{token}")}]
      else
        []
      end

    case :httpc.request(:get, {to_charlist(url), headers}, [timeout: 2_000], body_format: :binary) do
      {:ok, {{_, 200, _}, _headers, body}} ->
        # Try the provided token first; fall back to token file
        # (existing server may have been started with a different token)
        fallback_token = read_token_file()
        tokens = [token, fallback_token] |> Enum.reject(&(&1 in [nil, ""]))

        working =
          Enum.find(tokens, fn t ->
            match?({:ok, _}, do_tab_creation_check(host, port, t, _timeout = 2_000))
          end)

        if working do
          {:ok, body, working}
        else
          Logger.warning(
            "[KuriDaemon] Existing server at #{host}:#{port} cannot create tabs. " <>
              "Treating as unavailable."
          )

          {:error, :no_server}
        end

      {:ok, {{_, status, _}, _headers, _body}} ->
        # Port is in use but not healthy
        {:error, {:port_in_use, status}}

      {:error, _reason} ->
        # No server running on this port
        {:error, :no_server}
    end
  end

  defp read_token_file do
    path = Path.join([System.user_home!(), ".kuri", "api.token"])

    case File.read(path) do
      {:ok, content} -> String.trim(content)
      {:error, _} -> nil
    end
  end
end
