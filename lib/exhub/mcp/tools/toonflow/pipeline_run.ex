defmodule Exhub.MCP.Tools.Toonflow.PipelineRun do
  @moduledoc """
  MCP Tool: `toonflow_pipeline_run` — drive the whole Toonflow pipeline.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Pipeline

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_pipeline_run"

  @impl true
  def description do
    """
    Run the Toonflow pipeline end to end — novel → events → script → assets →
    storyboard → images → videos → voices → assemble — recording the run as a
    job and returning a per-stage report.

    Use `stages` (a comma list or array) or `from` to run a subset. `resume`
    (default true) skips stages whose output already exists and skips shots that
    already have the corresponding asset, so re-runs are cheap and idempotent;
    set `resume` to false to force regeneration. By default the run stops at the
    first failing stage (`continue_on_error` to press on).
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")

    field(:stages, {:list, :string},
      description: "Subset of stages to run (default: all), e.g. [\"script\",\"storyboard\"]."
    )

    field(:from, :string, description: "Run all stages starting at this one.")
    field(:resume, :boolean, description: "Skip stages/assets that already exist (default true).")
    field(:path, :string, description: "Novel source file path (for the novel stage).")
    field(:text, :string, description: "Novel source text (for the novel stage).")
    field(:title, :string, description: "Novel title (for the novel stage).")
    field(:chapter_id, :string, description: "Scope events/script to a single chapter.")
    field(:instructions, :string, description: "Extra writing/directing guidance.")

    field(:recall, :boolean,
      description: "Append semantically-recalled memory to script/storyboard."
    )

    field(:shots_limit, :integer, description: "Cap the shots processed by the media stages.")
    field(:mix_audio, :boolean, description: "Mix per-shot voiceover during assembly.")
    field(:subtitles, :boolean, description: "Soft-mux subtitles during assembly.")
    field(:continue_on_error, :boolean, description: "Keep going after a stage fails.")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> put(:stages, Map.get(params, :stages))
      |> Helpers.put_opt(:from, Helpers.opt(params, :from))
      |> put(:resume, Map.get(params, :resume))
      |> Helpers.put_opt(:path, Helpers.opt(params, :path))
      |> Helpers.put_opt(:text, Helpers.opt(params, :text))
      |> Helpers.put_opt(:title, Helpers.opt(params, :title))
      |> Helpers.put_opt(:chapter_id, Helpers.opt(params, :chapter_id))
      |> Helpers.put_opt(:instructions, Helpers.opt(params, :instructions))
      |> put(:recall, Map.get(params, :recall))
      |> put(:shots_limit, Map.get(params, :shots_limit))
      |> put(:mix_audio, Map.get(params, :mix_audio))
      |> put(:subtitles, Map.get(params, :subtitles))
      |> put(:continue_on_error, Map.get(params, :continue_on_error))

    result =
      if Keyword.get(opts, :resume, true) == false do
        Pipeline.run(project, Keyword.delete(opts, :resume))
      else
        Pipeline.resume(project, Keyword.delete(opts, :resume))
      end

    case result do
      {:ok, report} ->
        resp = Response.tool() |> Response.structured(Map.put(report, "success", true))
        {:reply, resp, frame}

      {:error, report} when is_map(report) ->
        resp = Response.tool() |> Response.structured(Map.put(report, "success", false))
        {:reply, resp, frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end

  defp put(opts, _key, nil), do: opts
  defp put(opts, key, value), do: Keyword.put(opts, key, value)
end
