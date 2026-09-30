defmodule Exhub.Toonflow.Jobs do
  @moduledoc """
  Lightweight job ledger over a project's `jobs` table.

  Phase 3 runs media calls synchronously; `run/5` still records a row so
  progress and failures are observable in the workspace database. Fully
  asynchronous submission (and a `toonflow_job_status` tool) is deferred to the
  Phase 4 pipeline.
  """

  alias Exhub.Toonflow
  alias Exhub.Toonflow.{DB, Schema, Store}

  @running "running"
  @success "success"
  @error "error"

  @doc "Record a running job and return its id."
  @spec start(String.t(), String.t(), map(), GenServer.server()) ::
          {:ok, String.t()} | {:error, term()}
  def start(project, type, params, server \\ Store) do
    id = Toonflow.new_id("job")
    now = Toonflow.now_iso()

    case Store.run_project(
           project,
           fn conn ->
             DB.execute(
               conn,
               "INSERT INTO jobs (id, type, status, params_json, result_json, error, created_at, updated_at) " <>
                 "VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
               [id, type, @running, Schema.encode_json(params || %{}), nil, nil, now, now]
             )
           end,
           server
         ) do
      :ok -> {:ok, id}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc "Mark a job successful."
  @spec finish(String.t(), String.t(), term(), GenServer.server()) :: :ok | {:error, term()}
  def finish(project, id, result, server \\ Store),
    do: update(project, id, @success, result, nil, server)

  @doc "Mark a job failed."
  @spec fail(String.t(), String.t(), term(), GenServer.server()) :: :ok | {:error, term()}
  def fail(project, id, error, server \\ Store),
    do: update(project, id, @error, nil, inspect(error), server)

  @doc """
  Run `fun` under a job record.

  The row is created first, then `fun` (which returns `{:ok, result}` or
  `{:error, reason}`) runs, then the outcome is recorded. Job bookkeeping is
  best-effort: if the row cannot be written, `fun` still runs.
  """
  @spec run(
          String.t(),
          String.t(),
          map(),
          (-> {:ok, term()} | {:error, term()}),
          GenServer.server()
        ) ::
          {:ok, term()} | {:error, term()}
  def run(project, type, params, fun, server \\ Store) when is_function(fun, 0) do
    id =
      case start(project, type, params, server) do
        {:ok, id} -> id
        _ -> nil
      end

    result = fun.()
    if id, do: record_outcome(project, id, result, server)
    result
  end

  @doc "List jobs, newest first. `opts`: `:status`, `:type`, `:limit`."
  @spec list(String.t(), keyword(), GenServer.server()) :: {:ok, [map()]} | {:error, term()}
  def list(project, opts \\ [], server \\ Store) do
    status = Toonflow.blank(Keyword.get(opts, :status))
    type = Toonflow.blank(Keyword.get(opts, :type))
    limit = Keyword.get(opts, :limit)
    {where, params} = filters(status, type)

    sql =
      "SELECT id, type, status, params_json, result_json, error, created_at, updated_at FROM jobs" <>
        where <> " ORDER BY created_at DESC"

    Store.run_project(
      project,
      fn conn ->
        case DB.query(conn, sql, params) do
          {:ok, rows} -> {:ok, rows |> Enum.map(&decode/1) |> Toonflow.maybe_limit(limit)}
          {:error, reason} -> {:error, reason}
        end
      end,
      server
    )
  end

  # --- internals ---

  defp update(project, id, status, result, error, server) do
    Store.run_project(
      project,
      fn conn ->
        DB.execute(
          conn,
          "UPDATE jobs SET status = ?, result_json = ?, error = ?, updated_at = ? WHERE id = ?",
          [
            status,
            if(is_nil(result), do: nil, else: Schema.encode_json(result)),
            error,
            Toonflow.now_iso(),
            id
          ]
        )
      end,
      server
    )
  end

  defp record_outcome(project, id, {:ok, result}, server), do: finish(project, id, result, server)

  defp record_outcome(project, id, {:error, reason}, server),
    do: fail(project, id, reason, server)

  defp record_outcome(_project, _id, _other, _server), do: :ok

  defp decode([id, type, status, params_json, result_json, error, created_at, updated_at]) do
    %{
      "id" => id,
      "type" => type,
      "status" => status,
      "params" => Schema.decode_json(params_json),
      "result" => Schema.decode_json(result_json),
      "error" => error,
      "created_at" => created_at,
      "updated_at" => updated_at
    }
  end

  defp filters(nil, nil), do: {"", []}
  defp filters(status, nil), do: {" WHERE status = ?", [status]}
  defp filters(nil, type), do: {" WHERE type = ?", [type]}
  defp filters(status, type), do: {" WHERE status = ? AND type = ?", [status, type]}
end
