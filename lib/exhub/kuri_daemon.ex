defmodule Exhub.KuriDaemon do
  @moduledoc """
  Supervised daemon that auto-starts the `kuri` HTTP server binary via Exile.

  Kuri is a Zig-based browser automation server that manages a Chrome instance
  via CDP and exposes an HTTP API (tabs, navigate, snapshot, action, etc.).
  This GenServer ensures kuri is running whenever Exhub boots, providing the
  browser backend for `kuri-agent` and the BrowserUse MCP tools.

  ## Configuration (application env)

  | Key                 | Default                         | Description                        |
  |---------------------|---------------------------------|------------------------------------|
  | `:kuri_enabled`     | `true`                          | Enable/disable the daemon          |
  | `:kuri_port`        | `18080`                         | HTTP listen port                   |
  | `:kuri_host`        | `"127.0.0.1"`                   | Bind address                       |
  | `:kuri_headless`    | `true`                          | Run Chrome headless                |
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
  @health_check_interval_ms 30_000
  @restart_delay_ms 5_000
  @startup_timeout_ms 10_000
  @startup_poll_interval_ms 500

  # ── Client API ──────────────────────────────────────────────────────────────

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc "Returns `:healthy`, `:unhealthy`, `:starting`, `:disabled`, or `:stopped`."
  def status do
    GenServer.call(__MODULE__, :status)
  end

  @doc "Returns the base URL of the running kuri server, e.g. `\"http://127.0.0.1:18080\"`."
  def base_url do
    GenServer.call(__MODULE__, :base_url)
  end

  @doc "Performs a synchronous health check against the kuri `/health` endpoint."
  def health do
    GenServer.call(__MODULE__, :health, 10_000)
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
    enabled = Application.get_env(:exhub, :kuri_enabled, true)

    state = %{
      port: port,
      host: host,
      headless: headless,
      status: :stopped,
      binary: nil,
      daemon_ref: nil,
      daemon_pid: nil,
      health_timer: nil,
      restart_timer: nil
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
    result = do_health_check(state.host, state.port)
    {:reply, result, state}
  end

  @impl true
  def handle_info(:start_daemon, state) do
    Logger.info("[KuriDaemon] Starting kuri server on #{state.host}:#{state.port}...")

    env = build_env(state)

    {pid, ref} =
      spawn_monitor(fn ->
        run_kuri(state.binary, env)
      end)

    # Wait for kuri to become healthy
    case wait_for_startup(state.host, state.port) do
      :ok ->
        Logger.info("[KuriDaemon] ✓ kuri server is healthy at http://#{state.host}:#{state.port}")
        timer = schedule_health_check()

        {:noreply,
         %{state | status: :healthy, daemon_ref: ref, daemon_pid: pid, health_timer: timer}}

      :timeout ->
        Logger.warning(
          "[KuriDaemon] kuri started but health check timed out after #{@startup_timeout_ms}ms"
        )

        timer = schedule_health_check()

        {:noreply,
         %{state | status: :unhealthy, daemon_ref: ref, daemon_pid: pid, health_timer: timer}}
    end
  end

  def handle_info({ref, result}, %{daemon_ref: ref} = state) when is_reference(ref) do
    # Daemon process completed (kuri exited)
    Process.demonitor(ref, [:flush])
    Logger.warning("[KuriDaemon] kuri process exited: #{inspect(result)}")

    cancel_timer(state.health_timer)
    timer = Process.send_after(self(), :start_daemon, @restart_delay_ms)

    {:noreply, %{state | status: :stopped, daemon_ref: nil, daemon_pid: nil, health_timer: nil, restart_timer: timer}}
  end

  def handle_info({:DOWN, ref, :process, _pid, reason}, %{daemon_ref: ref} = state) do
    Logger.warning("[KuriDaemon] kuri process crashed: #{inspect(reason)}")

    cancel_timer(state.health_timer)
    timer = Process.send_after(self(), :start_daemon, @restart_delay_ms)

    {:noreply, %{state | status: :stopped, daemon_ref: nil, daemon_pid: nil, health_timer: nil, restart_timer: timer}}
  end

  def handle_info(:health_check, state) do
    new_status =
      case do_health_check(state.host, state.port) do
        {:ok, _} -> :healthy
        _ -> :unhealthy
      end

    if new_status != state.status do
      Logger.info("[KuriDaemon] Health status changed: #{state.status} → #{new_status}")
    end

    timer = schedule_health_check()
    {:noreply, %{state | status: new_status, health_timer: timer}}
  end

  @impl true
  def terminate(_reason, state) do
    cancel_timer(state.health_timer)
    cancel_timer(state.restart_timer)

    if state.daemon_pid do
      Logger.info("[KuriDaemon] Shutting down kuri daemon (pid #{state.daemon_pid})...")
      Process.demonitor(state.daemon_ref, [:flush])

      # Send SIGTERM to the kuri process for graceful shutdown
      case System.cmd("kill", ["-TERM", Integer.to_string(state.daemon_pid)], stderr_to_stdout: true) do
        {_, 0} -> :ok
        _ -> :ok
      end
    end

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

  defp build_env(state) do
    base = [
      {"HOST", state.host},
      {"PORT", Integer.to_string(state.port)},
      {"HEADLESS", to_string(state.headless)}
    ]

    # Filter out nil values from env
    Enum.reject(base, fn {_k, v} -> v == "nil" or v == "" end)
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

  defp wait_for_startup(host, port, elapsed \\ 0) do
    if elapsed >= @startup_timeout_ms do
      :timeout
    else
      case do_health_check(host, port) do
        {:ok, _} ->
          :ok

        _ ->
          Process.sleep(@startup_poll_interval_ms)
          wait_for_startup(host, port, elapsed + @startup_poll_interval_ms)
      end
    end
  end

  defp do_health_check(host, port) do
    url = "http://#{host}:#{port}/health"

    case :httpc.request(:get, {to_charlist(url), []}, [timeout: 5_000], body_format: :binary) do
      {:ok, {{_, 200, _}, _headers, body}} ->
        {:ok, body}

      {:ok, {{_, status, _}, _headers, body}} ->
        {:error, {:http_error, status, body}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp schedule_health_check do
    Process.send_after(self(), :health_check, @health_check_interval_ms)
  end

  defp cancel_timer(nil), do: :ok
  defp cancel_timer(ref), do: Process.cancel_timer(ref)
end
