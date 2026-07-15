defmodule Exhub.GracefulRestart do
  @moduledoc """
  Coordinates graceful restarts for the Exhub BEAM VM.

  Supports three restart strategies:

  1. **Soft restart** (`:init.restart/0`) — Gracefully stops and restarts
     all OTP applications. In-flight HTTP requests finish; new requests are
     delayed until Cowboy comes back up.  Downtime is typically < 5 seconds.

  2. **Hard restart** (`:init.stop/0`) — Terminates the VM entirely.  Useful
     when an external supervisor (e.g. systemd) is responsible for restarting
     the process.

  3. **Socket activation** (systemd) — When the VM is started under systemd
     with `Type=notify` and `Sockets=exhub.socket`, the listening socket is
     passed in via `LISTEN_FDS`.  A soft or hard restart can then be used
     while systemd keeps the socket alive, yielding **zero-downtime** restarts.

  ## Usage

      Exhub.GracefulRestart.schedule_restart(:soft, 2_000)
      # → returns immediately, VM restarts after 2s

  ## systemd socket activation

  Add a `*.socket` unit that binds the port, then reference it in the
  `*.service` unit with `Requires=` and `After=`.  When the service starts
  it will find `LISTEN_FDS=1` in the environment; `cowboy_spec/0` in
  `Exhub.Application` automatically detects this and creates the listener
  from the inherited file descriptor.
  """

  require Logger

  @doc """
  Schedules a VM restart after `delay_ms` milliseconds.

  The calling process is *not* blocked — the restart happens asynchronously
  so the current HTTP/MCP response can be sent before shutdown begins.

  ## Parameters

    * `mode` — `:soft` (default) for `:init.restart/0`, `:hard` for
      `:init.stop/0`.
    * `delay_ms` — Time to wait before triggering the restart.
      Defaults to `3_000`.

  ## Returns

  `{:ok, pid}` of the spawned process.
  """
  @spec schedule_restart(mode :: :soft | :hard, delay_ms :: non_neg_integer()) ::
          {:ok, pid()}
  def schedule_restart(mode \\ :soft, delay_ms \\ 3_000) do
    pid =
      spawn(fn ->
        Logger.info("[GracefulRestart] Scheduling #{mode} restart in #{delay_ms}ms…")
        Process.sleep(delay_ms)

        case mode do
          :soft ->
            Logger.info("[GracefulRestart] Triggering :init.restart/0")
            :init.restart()

          :hard ->
            Logger.info("[GracefulRestart] Triggering :init.stop/0")
            :init.stop()
        end
      end)

    {:ok, pid}
  end

  @doc """
  Returns `true` if the current process was started via systemd socket
  activation (i.e. `LISTEN_FDS` is present and `LISTEN_PID` matches us).
  """
  @spec socket_activation?() :: boolean()
  def socket_activation? do
    with listen_fds when listen_fds != nil <- System.get_env("LISTEN_FDS"),
         listen_pid when listen_pid != nil <- System.get_env("LISTEN_PID"),
         {:ok, fds} <- parse_int(listen_fds),
         {:ok, pid} <- parse_int(listen_pid),
         true <- pid == System.pid() do
      fds >= 1
    else
      _ -> false
    end
  end

  @doc """
  Extracts the inherited socket file descriptor when running under systemd
  socket activation.

  systemd passes descriptors starting at **3** (FD 3, 4, …).  For Exhub we
  expect a single socket, so we return FD 3.

  Returns `{:ok, fd}` or `:error`.
  """
  @spec extract_listen_fd() :: {:ok, non_neg_integer()} | :error
  def extract_listen_fd do
    if socket_activation?() do
      # systemd passes the first FD at index 3
      {:ok, 3}
    else
      :error
    end
  end

  @doc """
  Builds Cowboy listener options that reuse an inherited systemd socket FD.

  This is called from `Exhub.Application.cowboy_spec/0` when socket
  activation is detected.  The returned options can be merged into the
  `Plug.Cowboy.child_spec/1` `options` keyword list.
  """
  @spec socket_options() :: keyword()
  def socket_options do
    case extract_listen_fd() do
      {:ok, fd} ->
        case fd_to_inet_socket(fd) do
          {:ok, socket} ->
            Logger.info("[GracefulRestart] Using inherited systemd socket FD #{fd}")
            [port: 0, net: :inet, transport_options: [socket: socket]]

          :error ->
            Logger.warning(
              "[GracefulRestart] FD #{fd} unusable, falling back to normal port binding"
            )

            []
        end

      :error ->
        []
    end
  end

  # --------------------------------------------------------------------------
  # Helpers
  # --------------------------------------------------------------------------

  defp parse_int(str) do
    case Integer.parse(str) do
      {n, ""} -> {:ok, n}
      _ -> :error
    end
  end

  # Convert a raw file descriptor number into an Erlang `:inet` socket
  # reference that Cowboy can use.
  defp fd_to_inet_socket(fd) when is_integer(fd) and fd >= 0 do
    # Erlang's `:gen_tcp` can adopt an existing FD with the `:fd` option
    # on Linux/macOS.  We use `:inet` (IPv4) as the address family.
    # This returns a socket that Cowboy will treat as already-bound.
    case :gen_tcp.fdopen(fd, [:binary, packet: :raw, active: false]) do
      {:ok, socket} ->
        {:ok, socket}

      {:error, reason} ->
        Logger.warning("[GracefulRestart] Failed to adopt FD #{fd}: #{inspect(reason)}")
        :error
    end
  end
end
