defmodule Exhub.MCP.Tools.Toonflow.Helpers do
  @moduledoc false
  # Shared helpers for the Toonflow MCP tools.

  alias Anubis.Server.Response

  @doc "Read a param, normalizing blank strings to the default."
  def opt(params, key, default \\ nil) do
    case Map.get(params, key, default) do
      value when is_binary(value) -> if String.trim(value) == "", do: default, else: value
      value -> value
    end
  end

  @doc "Add a non-blank option to a keyword list."
  def put_opt(opts, _key, nil), do: opts
  def put_opt(opts, _key, ""), do: opts
  def put_opt(opts, key, value), do: Keyword.put(opts, key, value)

  @doc "Render an error response with a human-readable message."
  def error(frame, reason),
    do: {:reply, Response.tool() |> Response.error(message(reason)), frame}

  @doc "Map a Toonflow error term to a human-readable message."
  def message(reason) do
    case reason do
      :not_found ->
        "Project not found."

      :no_chapters ->
        "No chapters found — ingest a novel first (toonflow_add_novel)."

      :no_script ->
        "No script found — generate one first (toonflow_generate_script)."

      :empty_novel ->
        "The source contains no usable text."

      :missing_source ->
        "Provide either `path` or `text`."

      :missing_script_id ->
        "`script_id` is required."

      :missing_content ->
        "`content` is required."

      :missing_text ->
        "`text` is required."

      :missing_query ->
        "`query` is required."

      {:chat_failed, reason} ->
        "Agent chat failed: #{inspect(reason)}"

      :shot_not_found ->
        "Shot not found — generate a storyboard first (toonflow_generate_storyboard)."

      :missing_prompt ->
        "Provide a `prompt`, or a `shot_id` whose shot has a prompt."

      :missing_api_key ->
        "Gitee AI API key not configured (set :giteeai_api_key)."

      :registry_unavailable ->
        "Toonflow registry is unavailable (SQLite not open)."

      {:read_failed, r} ->
        "Could not read the source file: #{inspect(r)}"

      {:doc_extract, r} ->
        "Document extraction failed: #{inspect(r)}"

      {:parse_failed, r} ->
        "Could not parse the model's JSON response: #{inspect(r)}"

      {:image_failed, r} ->
        "Image generation failed: #{inspect(r)}"

      :missing_first_frame ->
        "`fl2va` needs a first frame: generate a frame image for the shot " <>
          "(toonflow_generate_image) or pass `first_frame`."

      :no_video_clips ->
        "No video clips found — generate clips first (toonflow_generate_video)."

      :no_audio_url ->
        "Speech succeeded but no audio URL was returned."

      {:missing_clips, idxs} ->
        "Some shots have no generated clip (shot idx: #{inspect(idxs)}); " <>
          "run toonflow_generate_video for them first."

      {:missing_voice, idx} ->
        "Mix audio requested but shot idx #{inspect(idx)} has no voice clip."

      {:missing_tool, kind, exec} ->
        "Required tool #{kind} not found: #{inspect(exec)}."

      {:video_failed, r} ->
        "Video generation failed: #{inspect(r)}"

      {:voice_failed, r} ->
        "Voice generation failed: #{inspect(r)}"

      {:no_video_url, r} ->
        "Video task succeeded but returned no file URL: #{inspect(r)}"

      {:no_audio_url, r} ->
        "Speech task succeeded but returned no audio URL: #{inspect(r)}"

      {:ffmpeg_failed, code, out} ->
        "FFmpeg failed (exit #{inspect(code)}): #{out}"

      {:probe_failed, code, out} ->
        "FFprobe failed (exit #{inspect(code)}): #{out}"

      {:poll_timeout, task_id, attempts} ->
        "Timed out after #{attempts} polls waiting for task #{task_id}."

      {:no_task_id, decoded} ->
        "Upstream returned no task_id: #{inspect(decoded)}"

      {:http, status, body} ->
        "Upstream HTTP #{status}: #{inspect(body)}"

      {:download, status} ->
        "Failed to download the generated asset (HTTP #{status})."

      other ->
        "Toonflow error: #{inspect(other)}"
    end
  end
end
