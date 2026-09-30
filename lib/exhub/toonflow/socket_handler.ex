defmodule Exhub.Toonflow.SocketHandler do
  @moduledoc """
  Cowboy websocket handler powering the Toonflow canvas live view (Phase 5).

  One socket per browser page. The client passes `?project=<name>` in the query
  string (or sends `{"action":"subscribe","project":"..."}`) and receives JSON
  frames:

    * `snapshot` — the full `Exhub.Toonflow.Snapshot.build/2` payload (on
      subscribe / `refresh`);
    * `stage` / `shot` / `job` — live progress from `Exhub.Toonflow.Progress`;
    * `jobs` — periodic coarse refresh so a missed broadcast cannot leave the
      UI stale;
    * `pong` / `error`.

  Every reply is a Cowboy *frame* (`{:text, iodata}`) — cowlib's `cow_ws:frame/2`
  has no clause for a bare binary, so `{:reply, [json], state}` crashes the
  connection with a `:function_clause` (see `frames/1`).
  """

  @behaviour :cowboy_websocket
  require Logger

  alias Exhub.Toonflow
  alias Exhub.Toonflow.{Jobs, Progress, Snapshot}

  @tick_ms 5_000

  @impl true
  def init(req, _opts) do
    project = query_param(req, "project")
    {:cowboy_websocket, req, %{project: project}}
  end

  @impl true
  def websocket_init(state) do
    Progress.subscribe(state.project)
    schedule_tick()
    {:reply, snapshot_frames(state.project), state}
  end

  @impl true
  def websocket_handle({:text, payload}, state) do
    case Jason.decode(payload) do
      {:ok, %{"action" => "subscribe", "project" => project}} when is_binary(project) ->
        Progress.unsubscribe(state.project)
        state = %{state | project: Toonflow.blank(project)}
        Progress.subscribe(state.project)
        {:reply, snapshot_frames(state.project), state}

      {:ok, %{"action" => "unsubscribe"}} ->
        Progress.unsubscribe(state.project)
        {:ok, %{state | project: nil}}

      {:ok, %{"action" => "refresh"}} ->
        {:reply, snapshot_frames(state.project), state}

      {:ok, %{"action" => "ping"}} ->
        {:reply, frames(%{"type" => "pong"}), state}

      {:ok, %{"action" => action}} ->
        {:reply, frames(%{"type" => "error", "error" => "unknown action: #{action}"}), state}

      {:error, _} ->
        {:reply, frames(%{"type" => "error", "error" => "invalid json"}), state}
    end
  end

  def websocket_handle(_frame, state), do: {:ok, state}

  @impl true
  def websocket_info({:toonflow_progress, project, event}, %{project: project} = state) do
    {:reply, frames(event), state}
  end

  def websocket_info({:toonflow_progress, _project, _event}, state), do: {:ok, state}

  def websocket_info(:toonflow_tick, state) do
    schedule_tick()
    {:reply, job_frames(state.project), state}
  end

  def websocket_info(_info, state), do: {:ok, state}

  @impl true
  def terminate(reason, _req, _state) do
    Logger.debug("Toonflow socket terminating: #{inspect(reason)}")
    :ok
  end

  # ── internals ────────────────────────────────────────────────────────

  defp snapshot_frames(nil), do: frames(%{"type" => "snapshot", "project" => nil})

  defp snapshot_frames(project) do
    case Snapshot.build(project) do
      {:ok, snapshot} -> frames(Map.put(snapshot, "type", "snapshot"))
      {:error, reason} -> frames(%{"type" => "error", "error" => inspect(reason)})
    end
  end

  defp job_frames(nil), do: frames(%{"type" => "heartbeat"})

  defp job_frames(project) do
    case Jobs.list(project, limit: 5) do
      {:ok, jobs} -> frames(%{"type" => "jobs", "jobs" => jobs})
      _ -> frames(%{"type" => "heartbeat"})
    end
  end

  # A bare binary is not a valid Cowboy frame — wrap the JSON in `{:text, _}`.
  defp frames(map), do: [{:text, encode(map)}]

  defp encode(map) do
    Jason.encode!(map)
  rescue
    _ -> ~s({"type":"error","error":"unencodable frame"})
  end

  defp schedule_tick, do: Process.send_after(self(), :toonflow_tick, @tick_ms)

  defp query_param(req, key) do
    req
    |> :cowboy_req.parse_qs()
    |> List.keyfind(key, 0)
    |> case do
      {^key, value} -> Toonflow.blank(value)
      _ -> nil
    end
  end
end
