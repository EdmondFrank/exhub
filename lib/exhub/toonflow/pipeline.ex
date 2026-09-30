defmodule Exhub.Toonflow.Pipeline do
  @moduledoc """
  End-to-end staged orchestration for the Toonflow pipeline (Phase 4).

  Drives the whole novel → events → script → assets → storyboard → images →
  videos → voices → assemble chain through one call, recording the run as a
  `jobs` row (see `Exhub.Toonflow.Jobs`) and returning a per-stage report.

  Stages are idempotent where it matters: `resume/3` sets `:skip_existing`, so
  a stage whose output already exists is skipped and per-shot media stages skip
  shots that already have the corresponding asset.

  Every stage returns `{:ok, entry, ctx}` or `{:error, reason}`; by default the
  run stops at the first failure (`continue_on_error: true` to press on). The
  result is `{:ok, %{"stages" => [...], "completed" => n, "errors" => [...]}}`,
  or on failure `{:error, %{"stage" => name, "reason" => reason, ...}}`.
  """

  alias Exhub.Toonflow

  alias Exhub.Toonflow.{
    Assemble,
    Assets,
    Events,
    Jobs,
    Media,
    Novel,
    Progress,
    Script,
    Store,
    Storyboard,
    Video,
    Voice
  }

  @stages ~w(novel events script assets storyboard images videos voices assemble)

  @doc "The ordered pipeline stage names."
  @spec stages() :: [String.t()]
  def stages, do: @stages

  @doc """
  Run the pipeline (or a subset). `opts`:

    * `:stages` — subset of stage names (list or comma string); default all.
    * `:from` — start at this stage (implies all stages from there on).
    * `:path` / `:text` / `:title` — novel ingest source.
    * `:chapter_id` — scope events/script to one chapter.
    * `:instructions` / `:recall` / `:recall_top_k` — script & storyboard.
    * `:shots_limit` — cap the shots processed by the media stages.
    * `:mix_audio` / `:subtitles` — assembly options.
    * `:continue_on_error` — keep going after a stage fails (default false).
    * `:skip_existing` — skip stages whose output already exists (see `resume/3`).
  """
  @spec run(String.t(), keyword(), GenServer.server()) :: {:ok, map()} | {:error, map()}
  def run(project, opts \\ [], server \\ Store) do
    run_with(project, Keyword.put_new(opts, :skip_existing, false), server)
  end

  @doc "Like `run/3` but skips stages whose output already exists (idempotent resume)."
  @spec resume(String.t(), keyword(), GenServer.server()) :: {:ok, map()} | {:error, map()}
  def resume(project, opts \\ [], server \\ Store) do
    run_with(project, Keyword.put(opts, :skip_existing, true), server)
  end

  @doc """
  Inspect which stages are already satisfied, without running anything.

  Returns `{:ok, %{"stages" => [%{"stage" =>, "status" =>}], "next" => name | nil}}`
  where status is `"done"`, `"partial"` or `"ready"`.
  """
  @spec plan(String.t(), keyword(), GenServer.server()) :: {:ok, map()} | {:error, term()}
  def plan(project, opts \\ [], server \\ Store) do
    with {:ok, _} <- Store.get_project(project, server) do
      entries =
        @stages
        |> Enum.map(fn stage ->
          %{"stage" => stage, "status" => stage_status(stage, project, opts, server)}
        end)

      next =
        entries
        |> Enum.find(&(&1["status"] != "done"))
        |> case do
          nil -> nil
          entry -> entry["stage"]
        end

      {:ok, %{"stages" => entries, "next" => next}}
    end
  end

  # ── run ──────────────────────────────────────────────────────────────

  defp run_with(project, opts, server) do
    selected = selected_stages(opts)
    skip = Keyword.get(opts, :skip_existing, false)

    params = %{"stages" => selected, "skip_existing" => skip}

    Jobs.run(
      project,
      "pipeline",
      params,
      fn -> execute(project, selected, opts, server) end,
      server
    )
  end

  defp execute(project, selected, opts, server) do
    report = %{"stages" => [], "completed" => 0, "errors" => [], "skip_existing" => skip?(opts)}

    case run_stages(project, selected, opts, server, %{}, report) do
      {:ok, report} ->
        {:ok, report}

      {:error, entry, report} ->
        {:error,
         report
         |> Map.put("stage", entry["stage"])
         |> Map.put("reason", entry["reason"])}
    end
  end

  defp run_stages(_project, [], _opts, _server, _ctx, report), do: {:ok, report}

  defp run_stages(project, [stage | rest], opts, server, ctx, report) do
    Progress.stage_event(project, stage, "started")

    case run_stage(stage, project, opts, server, ctx) do
      {:ok, entry, ctx1} ->
        Progress.stage_event(project, stage, entry["status"], entry["detail"] || entry["reason"])

        report = %{
          report
          | "stages" => report["stages"] ++ [entry],
            "completed" => report["completed"] + 1
        }

        run_stages(project, rest, opts, server, ctx1, report)

      {:error, reason} ->
        Progress.stage_event(project, stage, "error", reason)
        entry = %{"stage" => stage, "status" => "error", "reason" => reason}

        report = %{
          report
          | "stages" => report["stages"] ++ [entry],
            "errors" => report["errors"] ++ [entry]
        }

        if Keyword.get(opts, :continue_on_error, false) do
          run_stages(project, rest, opts, server, ctx, report)
        else
          {:error, entry, report}
        end
    end
  end

  # ── stages ───────────────────────────────────────────────────────────

  defp run_stage("novel", project, opts, server, ctx) do
    cond do
      skip?(opts) and has_chapters?(project, server) ->
        {:ok, skipped("novel", "chapters already exist"), ctx}

      true ->
        case Novel.add_novel(
               project,
               compact(
                 path: kget(opts, :path),
                 text: kget(opts, :text),
                 title: kget(opts, :title)
               ),
               server
             ) do
          {:ok, novel} ->
            entry =
              ok("novel", %{"novel_id" => novel["novel_id"], "chapters" => novel["chapter_count"]})

            {:ok, entry, ctx}

          {:error, :missing_source} ->
            if has_chapters?(project, server) do
              {:ok, skipped("novel", "no source; using existing chapters"), ctx}
            else
              {:error, :missing_source}
            end

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  defp run_stage("events", project, opts, server, ctx) do
    chapter_id = kget(opts, :chapter_id)

    cond do
      skip?(opts) and has_events?(project, chapter_id, server) ->
        {:ok, skipped("events", "events already extracted"), ctx}

      true ->
        case Events.extract_events(project, compact(chapter_id: chapter_id), server) do
          {:ok, summary} ->
            entry =
              ok("events", Map.take(summary, ["chapters_extracted", "event_count", "errors"]))

            {:ok, entry, ctx}

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  defp run_stage("script", project, opts, server, ctx) do
    cond do
      skip?(opts) and has_script?(project, server) ->
        {:ok, skipped("script", "script already exists"), ctx}

      true ->
        case Script.generate_script(project, script_opts(opts), server) do
          {:ok, script} ->
            entry =
              ok("script", %{"script_id" => script["script_id"], "version" => script["version"]})

            {:ok, entry, Map.put(ctx, "script_id", script["script_id"])}

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  defp run_stage("assets", project, opts, server, ctx) do
    script_id = script_id(project, opts, server, ctx)

    cond do
      skip?(opts) and has_characters?(project, server) ->
        {:ok, skipped("assets", "assets already extracted"), ctx}

      true ->
        case Assets.extract_assets(project, compact(script_id: script_id), server) do
          {:ok, summary} -> {:ok, ok("assets", drop_raw(summary)), ctx}
          {:error, reason} -> {:error, reason}
        end
    end
  end

  defp run_stage("storyboard", project, opts, server, ctx) do
    script_id = script_id(project, opts, server, ctx)

    cond do
      skip?(opts) and has_shots?(project, script_id, server) ->
        {:ok, skipped("storyboard", "storyboard already exists"),
         Map.put(ctx, "script_id", script_id)}

      true ->
        case Storyboard.generate_storyboard(project, storyboard_opts(opts, script_id), server) do
          {:ok, summary} ->
            entry = ok("storyboard", Map.take(summary, ["script_id", "shot_count"]))
            {:ok, entry, Map.put(ctx, "script_id", script_id)}

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  defp run_stage(stage, project, opts, server, ctx) when stage in ~w(images videos voices) do
    script_id = script_id(project, opts, server, ctx)

    with {:ok, shots} <- Storyboard.list_shots(project, compact(script_id: script_id), server) do
      shots = Toonflow.maybe_limit(shots, kget(opts, :shots_limit))
      {generated, skipped_n, errors} = run_media(stage, project, shots, opts, server)

      detail = %{"shots" => length(shots), "generated" => generated, "skipped" => skipped_n}
      detail = if errors == [], do: detail, else: Map.put(detail, "errors", errors)

      entry =
        if generated == 0 and skipped_n > 0 and errors == [] do
          %{
            "stage" => stage,
            "status" => "skipped",
            "reason" => "assets already exist",
            "detail" => detail
          }
        else
          ok(stage, detail)
        end

      {:ok, entry, ctx}
    end
  end

  defp run_stage("assemble", project, opts, server, ctx) do
    script_id = script_id(project, opts, server, ctx)

    case Assemble.assemble(project, assemble_opts(opts, script_id), server) do
      {:ok, built} ->
        entry = ok("assemble", Map.take(built, ["video", "subtitles", "duration"]))
        {:ok, entry, ctx}

      {:error, reason} ->
        {:error, reason}
    end
  end

  # ── media helpers ────────────────────────────────────────────────────

  defp run_media(stage, project, shots, opts, server) do
    Enum.reduce(shots, {0, 0, []}, fn shot, {generated, skipped_n, errors} ->
      shot_id = shot["id"]

      cond do
        skip?(opts) and has_asset?(project, shot_id, asset_kind(stage), server) ->
          {generated, skipped_n + 1, errors}

        true ->
          case generate_media(stage, project, shot_id, opts, server) do
            {:ok, _asset} ->
              {generated + 1, skipped_n, errors}

            {:error, reason} ->
              {generated, skipped_n, [%{"shot_id" => shot_id, "reason" => reason} | errors]}
          end
      end
    end)
    |> then(fn {g, s, e} -> {g, s, Enum.reverse(e)} end)
  end

  defp generate_media("images", project, shot_id, opts, server),
    do: Media.generate_image(project, media_opts(opts, shot_id), server)

  defp generate_media("videos", project, shot_id, opts, server),
    do: Video.generate_video(project, media_opts(opts, shot_id), server)

  defp generate_media("voices", project, shot_id, opts, server),
    do: Voice.generate_voice(project, media_opts(opts, shot_id), server)

  defp media_opts(opts, shot_id) do
    compact(
      shot_id: shot_id,
      model: kget(opts, :model),
      task: kget(opts, :task),
      duration_seconds: kget(opts, :duration_seconds),
      size: kget(opts, :size),
      speaker: kget(opts, :speaker),
      voice: kget(opts, :voice),
      prompt_audio_url: kget(opts, :prompt_audio_url),
      prompt_text: kget(opts, :prompt_text)
    )
  end

  # ── plan ─────────────────────────────────────────────────────────────

  defp stage_status("novel", project, opts, server) do
    cond do
      has_chapters?(project, server) -> "done"
      kget(opts, :path) || kget(opts, :text) -> "ready"
      true -> "blocked"
    end
  end

  defp stage_status("events", project, opts, server) do
    if has_events?(project, kget(opts, :chapter_id), server), do: "done", else: "ready"
  end

  defp stage_status("script", project, _opts, server) do
    if has_script?(project, server), do: "done", else: "ready"
  end

  defp stage_status("assets", project, _opts, server) do
    if has_characters?(project, server), do: "done", else: "ready"
  end

  defp stage_status("storyboard", project, _opts, server) do
    if has_shots?(project, latest_script_id(project, server), server), do: "done", else: "ready"
  end

  defp stage_status(stage, project, _opts, server) when stage in ~w(images videos voices) do
    shots = shots_or_empty(project, latest_script_id(project, server), server)
    kind = asset_kind(stage)

    with_count =
      Enum.count(shots, fn shot -> has_asset?(project, shot["id"], kind, server) end)

    cond do
      shots == [] -> "ready"
      with_count == length(shots) -> "done"
      with_count == 0 -> "ready"
      true -> "partial"
    end
  end

  defp stage_status("assemble", _project, _opts, _server), do: "ready"

  # ── checks ───────────────────────────────────────────────────────────

  defp has_chapters?(project, server) do
    match?({:ok, [_ | _]}, Novel.list_chapters(project, [limit: 1], server))
  end

  defp has_events?(project, chapter_id, server) do
    match?({:ok, [_ | _]}, Events.list_events(project, compact(chapter_id: chapter_id), server))
  end

  defp has_script?(project, server) do
    match?({:ok, _}, Script.get_script(project, [], server))
  end

  defp has_characters?(project, server) do
    match?({:ok, [_ | _]}, Assets.list_characters(project, [limit: 1], server))
  end

  defp has_shots?(project, script_id, server),
    do: shots_or_empty(project, script_id, server) != []

  defp has_asset?(project, shot_id, kind, server) do
    match?(
      {:ok, asset} when not is_nil(asset),
      Media.latest_asset(project, shot_id, kind, server)
    )
  end

  defp shots_or_empty(project, script_id, server) do
    case Storyboard.list_shots(project, compact(script_id: script_id), server) do
      {:ok, shots} -> shots
      _ -> []
    end
  end

  defp script_id(project, opts, server, ctx) do
    kget(opts, :script_id) || ctx["script_id"] || latest_script_id(project, server)
  end

  defp latest_script_id(project, server) do
    case Script.get_script(project, [], server) do
      {:ok, script} -> script["id"] || script["script_id"]
      _ -> nil
    end
  end

  # ── option shaping ───────────────────────────────────────────────────

  defp selected_stages(opts) do
    requested =
      case kget(opts, :stages) do
        nil ->
          nil

        value when is_binary(value) ->
          value |> String.split(",", trim: true) |> Enum.map(&String.trim/1)

        value when is_list(value) ->
          Enum.map(value, &to_string/1)
      end

    from = kget(opts, :from)

    cond do
      requested -> Enum.filter(requested, &(&1 in @stages))
      from -> Enum.drop_while(@stages, &(&1 != to_string(from)))
      true -> @stages
    end
  end

  defp script_opts(opts) do
    compact(
      chapter_id: kget(opts, :chapter_id),
      instructions: kget(opts, :instructions),
      recall: kget(opts, :recall),
      recall_top_k: kget(opts, :recall_top_k)
    )
  end

  defp storyboard_opts(opts, script_id) do
    compact(
      script_id: script_id,
      instructions: kget(opts, :instructions),
      recall: kget(opts, :recall),
      recall_top_k: kget(opts, :recall_top_k)
    )
  end

  defp assemble_opts(opts, script_id) do
    compact(
      script_id: script_id,
      mix_audio: kget(opts, :mix_audio),
      subtitles: kget(opts, :subtitles),
      name: kget(opts, :name)
    )
  end

  defp asset_kind("images"), do: "image"
  defp asset_kind("videos"), do: "video"
  defp asset_kind("voices"), do: "audio"

  # ── small helpers ────────────────────────────────────────────────────

  defp skip?(opts), do: Keyword.get(opts, :skip_existing, false)

  defp kget(opts, key), do: Toonflow.blank(Keyword.get(opts, key))

  defp ok(stage, detail), do: %{"stage" => stage, "status" => "ok", "detail" => detail}
  defp skipped(stage, reason), do: %{"stage" => stage, "status" => "skipped", "reason" => reason}

  defp compact(kw), do: Enum.reject(kw, fn {_k, v} -> is_nil(v) end)

  defp drop_raw(summary) when is_map(summary), do: Map.drop(summary, ["raw"])
  defp drop_raw(summary), do: summary
end
