defmodule Exhub.Toonflow.Progress do
  @moduledoc """
  Best-effort live-progress fan-out for the Toonflow UI (Phase 5).

  A thin wrapper over a duplicate-key `Registry`
  (`Exhub.Toonflow.Progress.Registry`, supervised in `Application`). Pipeline
  stages publish `stage` / `shot` events here; the Toonflow websocket
  (`Exhub.Toonflow.SocketHandler`) subscribes per project and forwards them to
  the browser.

  Every function is defensive: if the registry is not running (unit tests, a
  partially-started VM) publishing is a no-op. A UI concern must never be able
  to break a pipeline run.
  """

  @registry Exhub.Toonflow.Progress.Registry

  @type event :: map()

  @doc "The registry name (used by the `Application` child spec and tests)."
  @spec registry() :: atom()
  def registry, do: @registry

  @doc "Subscribe the calling process to `project`'s progress events."
  @spec subscribe(String.t() | nil) :: :ok
  def subscribe(project) when is_binary(project) do
    _ = Registry.register(@registry, key(project), nil)
    :ok
  rescue
    _ -> :ok
  end

  def subscribe(_), do: :ok

  @doc "Stop receiving `project`'s progress events."
  @spec unsubscribe(String.t() | nil) :: :ok
  def unsubscribe(project) when is_binary(project) do
    _ = Registry.unregister(@registry, key(project))
    :ok
  rescue
    _ -> :ok
  end

  def unsubscribe(_), do: :ok

  @doc """
  Publish `event` to every process subscribed to `project`.

  Subscribers receive `{:toonflow_progress, project, event}`.
  """
  @spec broadcast(String.t() | nil, event()) :: :ok
  def broadcast(project, event) when is_binary(project) and is_map(event) do
    _ =
      Registry.dispatch(@registry, key(project), fn entries ->
        for {pid, _value} <- entries, do: send(pid, {:toonflow_progress, project, event})
      end)

    :ok
  rescue
    _ -> :ok
  end

  def broadcast(_project, _event), do: :ok

  @doc "Publish a stage lifecycle event (`\"started\"` / `\"ok\"` / `\"skipped\"` / `\"error\"`)."
  @spec stage_event(String.t() | nil, String.t(), String.t(), term()) :: :ok
  def stage_event(project, stage, status, detail \\ nil) do
    broadcast(project, %{
      "type" => "stage",
      "stage" => stage,
      "status" => status,
      "detail" => normalize(detail),
      "at" => now()
    })
  end

  @doc "Publish a per-shot media progress event."
  @spec shot_event(String.t() | nil, String.t(), String.t(), String.t(), term()) :: :ok
  def shot_event(project, stage, shot_id, status, detail \\ nil) do
    broadcast(project, %{
      "type" => "shot",
      "stage" => stage,
      "shot_id" => shot_id,
      "status" => status,
      "detail" => normalize(detail),
      "at" => now()
    })
  end

  @doc "Publish a job lifecycle event."
  @spec job_event(String.t() | nil, String.t(), String.t(), term()) :: :ok
  def job_event(project, job_id, status, detail \\ nil) do
    broadcast(project, %{
      "type" => "job",
      "job_id" => job_id,
      "status" => status,
      "detail" => normalize(detail),
      "at" => now()
    })
  end

  # Reason terms (e.g. `{:image_failed, :boom}`) must survive JSON encoding in
  # the socket, so coerce anything non-JSON-native to its `inspect` form.
  defp normalize(nil), do: nil
  defp normalize(v) when is_binary(v) or is_number(v) or is_boolean(v) or is_atom(v), do: v
  defp normalize(v) when is_list(v), do: Enum.map(v, &normalize/1)

  defp normalize(v) when is_map(v),
    do: Map.new(v, fn {k, val} -> {json_key(k), normalize(val)} end)

  defp normalize(v), do: inspect(v)

  defp json_key(k) when is_binary(k) or is_atom(k) or is_number(k), do: k
  defp json_key(k), do: inspect(k)

  defp key(project), do: {:toonflow_progress, project}

  defp now do
    DateTime.utc_now() |> DateTime.truncate(:second) |> DateTime.to_iso8601()
  end
end
