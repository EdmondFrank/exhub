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

  The cast tab also sends work over the socket, because the page already holds
  an authenticated connection and the HTTP surface would need its own routes for
  two small operations:

    * `subject_upload` — `{kind, name, data}` with a base64 JPEG; stored as the
      subject's only reference sheet (`Exhub.Toonflow.Assets.import_reference/6`).
      The sheet is then read back into a description by
      `Exhub.Toonflow.Reverse` and answered as a second `reverse` frame, so the
      text later frames are rendered from matches the picture that was just
      uploaded;
    * `subject_reference` — `{kind, name, text, regenerate}`; saves the edited
      description and, unless `regenerate` is `false`, re-renders the sheet. The
      render runs in `Exhub.MCP.ToolTaskSupervisor` so the socket keeps
      streaming progress while an image takes tens of seconds to come back;
    * `subject_reverse` — `{kind, name}`; re-infers the description from the
      current sheet without replacing it;
    * `subject_clear` — `{kind, name}`; deletes the subject's reference rows and
      files and empties its `refs` (the subject itself stays in the cast);
    * `asset_delete` — `{asset_id}`; deletes one 物料 asset, its file, and every
      reference that still points at it.

  Both answer with `{"type":"subject","status":"started"|"ok"|"error",…}`
  followed by a fresh snapshot. Mutating actions honour
  `Exhub.Toonflow.Access.writable?/1` for the peer address.

  Every reply is a Cowboy *frame* (`{:text, iodata}`) — cowlib's `cow_ws:frame/2`
  has no clause for a bare binary, so `{:reply, [json], state}` crashes the
  connection with a `:function_clause` (see `frames/1`).
  """

  @behaviour :cowboy_websocket
  require Logger

  alias Exhub.Toonflow
  alias Exhub.Toonflow.{Access, Assets, Jobs, Progress, Reverse, Snapshot, Upload}

  @tick_ms 5_000

  @impl true
  def init(req, _opts) do
    project = query_param(req, "project")
    {peer_ip, _port} = :cowboy_req.peer(req)
    {:cowboy_websocket, req, %{project: project, peer_ip: peer_ip}}
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

      {:ok, %{"action" => "subject_upload"} = msg} ->
        subject_upload(msg, state)

      {:ok, %{"action" => "subject_reference"} = msg} ->
        subject_reference(msg, state)

      {:ok, %{"action" => "subject_reverse"} = msg} ->
        subject_reverse(msg, state)

      {:ok, %{"action" => "subject_clear"} = msg} ->
        subject_clear(msg, state)

      {:ok, %{"action" => "asset_delete"} = msg} ->
        asset_delete(msg, state)

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

  def websocket_info({:subject_done, context, result}, state) do
    {:reply, subject_frames(result, context) ++ snapshot_frames(state.project), state}
  end

  def websocket_info(_info, state), do: {:ok, state}

  @impl true
  def terminate(reason, _req, _state) do
    Logger.debug("Toonflow socket terminating: #{inspect(reason)}")
    :ok
  end

  # ── internals ────────────────────────────────────────────────────────

  # ── cast-tab edits ───────────────────────────────────────────────────

  # Small enough to run inline: the bytes are already in memory and the write is
  # one file plus two rows. The description is inferred afterwards, off the
  # connection process, because one vision request takes seconds.
  defp subject_upload(msg, state) do
    kind = msg["kind"]
    name = msg["name"]

    result =
      with :ok <- writable(state),
           {:ok, bytes} <- decode_image(msg["data"]),
           {:ok, ext} <- Upload.ext_for(bytes) do
        Assets.import_reference(state.project, kind, name, bytes, ext)
      end

    case result do
      {:ok, payload} ->
        # A failed spawn must not leave the card "反推中…" forever: only promise
        # the follow-up frame when the task actually started.
        reverse? = start_reverse(state.project, kind, name) == :ok

        frames(
          Map.merge(
            %{"type" => "subject", "status" => "ok", "action" => "upload", "reverse" => reverse?},
            payload
          )
        ) ++ snapshot_frames(state.project)

      {:error, _reason} = error ->
        subject_frames(error, {kind, name, "upload"}) ++ snapshot_frames(state.project)
    end
  end

  # Re-read the *current* sheet into a description, leaving the picture alone: a
  # generated sheet or a sheet uploaded before reverse existed can both be
  # retried without going through the file picker again.
  defp subject_reverse(msg, state) do
    kind = msg["kind"]
    name = msg["name"]
    context = {kind, name, "reverse"}

    case writable(state) do
      :ok ->
        case start_reverse(state.project, kind, name) do
          :ok ->
            {:reply,
             frames(%{
               "type" => "subject",
               "status" => "started",
               "action" => "reverse",
               "kind" => kind,
               "name" => name
             }), state}

          {:error, reason} ->
            {:reply, subject_frames({:error, reason}, context), state}
        end

      {:error, reason} ->
        {:reply, subject_frames({:error, reason}, context), state}
    end
  end

  defp subject_clear(msg, state) do
    kind = msg["kind"]
    name = msg["name"]
    context = {kind, name, "clear"}

    result =
      with :ok <- writable(state),
           {:ok, payload} <- Assets.clear_references(state.project, kind, name) do
        {:ok, payload}
      end

    {:reply, subject_frames(result, context) ++ snapshot_frames(state.project), state}
  end

  defp asset_delete(msg, state) do
    asset_id = msg["asset_id"]

    result =
      with :ok <- writable(state),
           {:ok, payload} <- Assets.delete_asset(state.project, asset_id) do
        {:ok, payload}
      end

    {:reply, asset_frames(result, asset_id) ++ snapshot_frames(state.project), state}
  end

  defp asset_frames({:ok, payload}, _asset_id) do
    frames(Map.merge(%{"type" => "assets", "status" => "ok", "action" => "delete"}, payload))
  end

  defp asset_frames({:error, reason}, asset_id) do
    Logger.warning("[Toonflow] asset delete #{inspect(asset_id)}: #{inspect(reason)}")

    frames(%{
      "type" => "assets",
      "status" => "error",
      "action" => "delete",
      "asset_id" => asset_id,
      "error" => reason_text(reason)
    })
  end

  defp start_reverse(project, kind, name) do
    parent = self()
    context = {kind, name, "reverse"}

    case Task.Supervisor.start_child(Exhub.MCP.ToolTaskSupervisor, fn ->
           send(parent, {:subject_done, context, Reverse.infer(project, kind, name)})
         end) do
      {:ok, _pid} -> :ok
      {:error, reason} -> {:error, {:task_failed, reason}}
    end
  end

  # A render takes tens of seconds; run it off the connection process so progress
  # ticks and other frames keep flowing, then answer from `websocket_info/2`.
  defp subject_reference(msg, state) do
    kind = msg["kind"]
    name = msg["name"]
    text = Toonflow.blank(msg["text"])
    regenerate = msg["regenerate"] != false
    action = if regenerate, do: "reference", else: "save"
    context = {kind, name, action}

    case writable(state) do
      :ok ->
        parent = self()
        project = state.project

        Task.Supervisor.start_child(Exhub.MCP.ToolTaskSupervisor, fn ->
          send(
            parent,
            {:subject_done, context, subject_render(project, kind, name, text, regenerate)}
          )
        end)

        {:reply,
         frames(%{
           "type" => "subject",
           "status" => "started",
           "action" => action,
           "kind" => kind,
           "name" => name
         }), state}

      {:error, reason} ->
        {:reply, subject_frames({:error, reason}, context), state}
    end
  end

  defp subject_render(project, kind, name, text, regenerate) do
    action = if regenerate, do: "reference", else: "save"

    with :ok <- save_subject_text(project, kind, name, text),
         {:ok, done} <- regenerate_sheet(project, kind, name, text, regenerate) do
      {:ok,
       %{
         "action" => action,
         "kind" => kind,
         "name" => name,
         "path" => done && done["path"]
       }}
    end
  end

  # A blank description means "keep the stored one", which is how a bare
  # re-render stays possible after the textarea was cleared by accident.
  defp save_subject_text(_project, _kind, _name, nil), do: :ok

  defp save_subject_text(project, kind, name, text) do
    case Assets.update_text(project, kind, name, text) do
      {:ok, _saved} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  defp regenerate_sheet(_project, _kind, _name, _text, false), do: {:ok, nil}

  defp regenerate_sheet(project, kind, name, text, true) do
    Assets.generate_reference(project,
      kind: kind,
      name: name,
      instructions: text,
      replace: true
    )
  end

  defp subject_frames({:ok, payload}, {_kind, _name, action}) when is_map(payload) do
    frames(Map.merge(%{"type" => "subject", "status" => "ok", "action" => action}, payload))
  end

  defp subject_frames({:error, reason}, {kind, name, action}) do
    Logger.warning("[Toonflow] subject #{action} #{kind}/#{name}: #{inspect(reason)}")

    frames(%{
      "type" => "subject",
      "status" => "error",
      "action" => action,
      "kind" => kind,
      "name" => name,
      "error" => reason_text(reason)
    })
  end

  defp writable(%{peer_ip: ip}) do
    if Access.writable?(ip), do: :ok, else: {:error, :forbidden}
  end

  defp decode_image(data) when is_binary(data) do
    case Base.decode64(data) do
      {:ok, bytes} when byte_size(bytes) > 0 ->
        if byte_size(bytes) <= Upload.max_bytes(), do: {:ok, bytes}, else: {:error, :too_large}

      _ ->
        {:error, :empty_image}
    end
  end

  defp decode_image(_data), do: {:error, :empty_image}

  defp reason_text(:forbidden), do: "该连接无权修改项目"
  defp reason_text(:empty_image), do: "没有收到图片数据"
  defp reason_text(:too_large), do: "图片过大"
  defp reason_text(:unsupported_image), do: "只支持 PNG / JPEG / WebP 图片"
  defp reason_text(:subject_not_found), do: "找不到该角色 / 场景 / 道具"
  defp reason_text(:no_reference), do: "这个对象还没有参考图，先上传一张"
  defp reason_text(:reference_missing), do: "参考图文件已不存在，请重新上传"
  defp reason_text(:asset_not_found), do: "素材不存在（可能已被删除）"
  defp reason_text(:missing_asset_id), do: "缺少素材 id"
  defp reason_text(:empty_vision_result), do: "没有识别出可用的描述，换一张更清晰的图"
  defp reason_text({:vision_failed, reason}), do: "反推提示词失败：#{inspect(reason)}"
  defp reason_text({:task_failed, reason}), do: "无法启动任务：#{inspect(reason)}"
  defp reason_text(:missing_description), do: "提示词不能为空"
  defp reason_text(:missing_name), do: "缺少名称"
  defp reason_text(:invalid_reference_kind), do: "未知的类型"
  defp reason_text({:image_failed, reason}), do: "生成失败：#{inspect(reason)}"
  defp reason_text(other), do: inspect(other)

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
