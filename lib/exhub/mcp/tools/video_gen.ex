defmodule Exhub.MCP.Tools.VideoGen do
  @moduledoc """
  MCP Tool for generating videos with the MiniMax-H3 model on MoArk's
  Serverless API (Gitee AI).

  Supports two MoArk interfaces:

    * `t2va` — 文生视频 / text-to-video
    * `fl2va` — 首尾帧生视频 / first-last-frame-to-video

  Video generation on MoArk is asynchronous: the task is submitted to
  `POST /v1/async/videos/generations`, which returns a `task_id`, and the
  result is then polled from `GET /v1/task/{task_id}` until it succeeds.

  See `docs/moark-minimax-h3-api.md` for the full API reference.
  """

  alias Anubis.Server.Response

  use Anubis.Server.Component, type: :tool

  @submit_url "https://api.moark.com/v1/async/videos/generations"
  @task_url "https://api.moark.com/v1/task"

  @default_model "MiniMax-H3"
  @valid_models ~w(MiniMax-H3)

  @t2va "t2va"
  @fl2va "fl2va"
  @valid_tasks ~w(t2va fl2va)

  @valid_aspect_ratios ~w(auto adaptive 9:16 1:1 4:3 3:4 16:9)

  @min_duration 4
  @max_duration 15
  @min_steps 5
  @max_steps 50

  @default_duration 6
  @default_steps 20
  @default_aspect_ratio "16:9"

  # Polling budget stays under the server's 600_000 ms request timeout.
  @poll_interval_ms 10_000
  @max_poll_attempts 55
  @submit_timeout_ms 60_000
  @poll_timeout_ms 15_000

  def name, do: "video_gen"

  @impl true
  def description do
    """
    Generate a video from text (text-to-video) or first/last frame images
    (first-last-frame-to-video) using the MiniMax-H3 model on MoArk / Gitee AI.

    Video generation is asynchronous: this tool submits the task and then polls
    until the video is ready, returning its URL. Display it with markdown:
    `[video](URL)` or an HTML5 `<video>` tag.

    **Modes (`task`):**
    - `t2va` (default) — text-to-video. `prompt` is the core input.
    - `fl2va` — first-last-frame-to-video. Requires `first_frame` (image URL);
      `last_frame` is optional.

    **Parameters:**
    - `model`: only `MiniMax-H3` is supported.
    - `duration_seconds`: 4–15 (default 6).
    - `num_steps`: inference steps, 5–50 (default 20).
    - `aspect_ratio`: `auto`, `adaptive`, `9:16`, `1:1`, `4:3`, `3:4`, `16:9` (default 16:9).
    - `seed`: integer seed for reproducible generation.
    - `first_frame` / `last_frame`: image source for `fl2va` — a URL, a base64
      `data:` URI, or an absolute / `~` local file path. Local files are encoded
      as data URIs; anything over 2 MB is downscaled to 1280 px first.
    - `wait`: wait for the result (default true). Set `false` to submit and
      return the `task_id` immediately.
    - `task_id`: poll an existing task instead of submitting a new one — useful
      if a previous call timed out.

    Generation can take minutes; if a synchronous call times out, re-run with
    `task_id` (or submit with `wait: false`).
    """
  end

  schema do
    field(:prompt, {:required, :string},
      description: "Text description of the video to generate. Be specific and detailed."
    )

    field(:model, :string, description: "Video model. Only `MiniMax-H3` is supported (default).")

    field(:task, :string,
      description:
        "Generation mode: `t2va` (text-to-video, default) or `fl2va` (first/last-frame-to-video)."
    )

    field(:first_frame, :string,
      description:
        "First-frame image: a URL, a base64 `data:` URI, or an absolute / `~` local " <>
          "file path. Required for `task` = \"fl2va\"."
    )

    field(:last_frame, :string,
      description:
        "Last-frame image: a URL, a base64 `data:` URI, or an absolute / `~` local " <>
          "file path. Optional, only used with `task` = \"fl2va\"."
    )

    field(:duration_seconds, :integer,
      description: "Video duration in seconds, 4–15. Default: 6."
    )

    field(:num_steps, :integer,
      description:
        "Number of inference steps, 5–50. Higher = better quality but slower. Default: 20."
    )

    field(:aspect_ratio, :string,
      description:
        "Video aspect ratio. One of: auto, adaptive, 9:16, 1:1, 4:3, 3:4, 16:9. Default: 16:9."
    )

    field(:seed, :integer,
      description: "Random seed for reproducible generation (integer). Optional."
    )

    field(:wait, :boolean,
      description:
        "Whether to wait for the result (default: true). When false, returns the task_id immediately."
    )

    field(:task_id, :string,
      description: "Existing task id to poll instead of submitting a new task."
    )
  end

  @impl true
  def execute(params, frame) do
    api_key = Application.get_env(:exhub, :giteeai_api_key, "")

    cond do
      api_key == "" ->
        error(
          frame,
          "Gitee AI API key not configured. Run: mix scr.insert dev giteeai_api_key \"your-key\""
        )

      is_binary(Map.get(params, :task_id)) and Map.get(params, :task_id) != "" ->
        poll_and_reply(Map.get(params, :task_id), context_from(params), api_key, frame)

      true ->
        with {:ok, resolved} <- resolve_frames(params),
             {:ok, validated} <- validate_params(resolved) do
          submit_and_reply(build_body(validated), resolved, validated, api_key, frame)
        else
          {:error, reason} -> error(frame, reason)
        end
    end
  end

  # ---------------------------------------------------------------------------
  # Public, pure helpers (unit-tested)
  # ---------------------------------------------------------------------------

  @doc """
  Validates and normalizes `video_gen` params.

  Returns `{:ok, validated}` with keys `:model`, `:task`, `:prompt`,
  `:duration_seconds`, `:num_steps`, `:aspect_ratio`, `:first_frame`,
  `:last_frame`, `:seed`, or `{:error, message}`.
  """
  @spec validate_params(map()) :: {:ok, map()} | {:error, String.t()}
  def validate_params(params) when is_map(params) do
    model = normalize_string(Map.get(params, :model), @default_model)
    task = normalize_string(Map.get(params, :task), @t2va)
    prompt = Map.get(params, :prompt)
    duration = Map.get(params, :duration_seconds) || @default_duration
    steps = Map.get(params, :num_steps) || @default_steps
    aspect = normalize_string(Map.get(params, :aspect_ratio), @default_aspect_ratio)
    first_frame = Map.get(params, :first_frame)
    last_frame = Map.get(params, :last_frame)
    seed = Map.get(params, :seed)

    cond do
      not is_binary(prompt) or String.trim(prompt) == "" ->
        {:error, "`prompt` is required"}

      model not in @valid_models ->
        {:error, "Invalid model: #{model}. Valid models: #{Enum.join(@valid_models, ", ")}"}

      task not in @valid_tasks ->
        {:error,
         "Invalid task: #{task}. Valid tasks: #{Enum.join(@valid_tasks, ", ")} " <>
           "(t2va = text-to-video, fl2va = first/last-frame-to-video)"}

      task == @fl2va and not present?(first_frame) ->
        {:error,
         "`first_frame` is required when `task` is \"fl2va\" — pass an image URL, " <>
           "a base64 data URI, or a local file path"}

      not (is_integer(duration) and duration >= @min_duration and duration <= @max_duration) ->
        {:error,
         "Invalid duration_seconds: #{inspect(duration)}. " <>
           "Must be an integer between #{@min_duration} and #{@max_duration}."}

      not (is_integer(steps) and steps >= @min_steps and steps <= @max_steps) ->
        {:error,
         "Invalid num_steps: #{inspect(steps)}. " <>
           "Must be an integer between #{@min_steps} and #{@max_steps}."}

      aspect not in @valid_aspect_ratios ->
        {:error,
         "Invalid aspect_ratio: #{aspect}. Valid values: #{Enum.join(@valid_aspect_ratios, ", ")}"}

      not is_nil(seed) and not is_integer(seed) ->
        {:error, "Invalid seed: #{inspect(seed)}. Must be an integer."}

      true ->
        {:ok,
         %{
           model: model,
           task: task,
           prompt: prompt,
           duration_seconds: duration,
           num_steps: steps,
           aspect_ratio: aspect,
           first_frame: first_frame,
           last_frame: last_frame,
           seed: seed
         }}
    end
  end

  @doc """
  Resolves `first_frame` / `last_frame` into values the MoArk API can fetch.

  URLs and base64 `data:` URIs pass through unchanged; local file paths
  (absolute or `~` shorthand) are read and encoded as data URIs by
  `Exhub.MCP.ImageSource`. Frames that are absent are left untouched.

  Returns `{:ok, params}` with the frames resolved, or `{:error, message}` when
  a frame cannot be read.
  """
  @spec resolve_frames(map()) :: {:ok, map()} | {:error, String.t()}
  def resolve_frames(params) when is_map(params) do
    with {:ok, first} <- resolve_frame(params, :first_frame),
         {:ok, last} <- resolve_frame(params, :last_frame) do
      {:ok, params |> put_frame(:first_frame, first) |> put_frame(:last_frame, last)}
    end
  end

  @doc """
  Builds the submit request body from validated params.

  `first_frame` / `last_frame` are only included for the `fl2va` task; `seed`
  is included only when set.
  """
  @spec build_body(map()) :: map()
  def build_body(validated) do
    body = %{
      "model" => validated.model,
      "task" => validated.task,
      "prompt" => validated.prompt,
      "duration_seconds" => validated.duration_seconds,
      "num_steps" => validated.num_steps,
      "aspect_ratio" => validated.aspect_ratio
    }

    body =
      if validated.task == @fl2va do
        body
        |> Map.put("first_frame", validated.first_frame)
        |> maybe_put("last_frame", validated.last_frame)
      else
        body
      end

    maybe_put(body, "seed", validated.seed)
  end

  @doc """
  Classifies a decoded `/v1/task/{id}` payload.

  Returns `{:ok, result}` on success, `{:error, message}` when the task failed
  or was cancelled, and `:pending` while it is still queued or running.
  """
  @spec interpret_poll(map()) :: {:ok, map()} | {:error, String.t()} | :pending
  def interpret_poll(result) when is_map(result) do
    status = Map.get(result, "status")

    cond do
      Map.get(result, "error") ->
        {:error, "#{result["error"]}: #{Map.get(result, "message", "Unknown error")}"}

      status == "success" ->
        {:ok, result}

      status in ["failure", "failed", "cancelled"] ->
        {:error, "Task ended with status: #{status}#{error_detail(result)}"}

      true ->
        :pending
    end
  end

  # ---------------------------------------------------------------------------
  # Private helpers
  # ---------------------------------------------------------------------------

  defp submit_and_reply(body_map, params, validated, api_key, frame) do
    case submit_task(body_map, api_key) do
      {:ok, task_id} ->
        if Map.get(params, :wait, true) == false do
          reply =
            Jason.encode!(%{
              "task_id" => task_id,
              "status" => "submitted",
              "model" => validated.model,
              "task" => validated.task,
              "video_url" => nil
            })

          {:reply, Response.tool() |> Response.text(reply), frame}
        else
          poll_and_reply(task_id, validated, api_key, frame)
        end

      {:error, reason} ->
        error(frame, reason)
    end
  end

  defp submit_task(body_map, api_key) do
    headers = [
      {"Content-Type", "application/json"},
      {"Authorization", "Bearer #{api_key}"},
      {"X-Failover-Enabled", "true"}
    ]

    case HTTPoison.post(
           @submit_url,
           Jason.encode!(body_map),
           headers,
           [recv_timeout: @submit_timeout_ms, timeout: @submit_timeout_ms] ++
             Exhub.TLSCompat.httpoison_opts(@submit_url)
         ) do
      {:ok, %HTTPoison.Response{status_code: status, body: resp_body}} when status in 200..299 ->
        case Jason.decode(resp_body) do
          {:ok, %{"task_id" => task_id}} when is_binary(task_id) ->
            {:ok, task_id}

          {:ok, decoded} ->
            {:error, "No task_id in response: #{inspect(decoded)}"}

          {:error, reason} ->
            {:error, "Failed to decode submit response: #{inspect(reason)}"}
        end

      {:ok, %HTTPoison.Response{status_code: status, body: resp_body}} ->
        {:error, "MoArk video API error (HTTP #{status}): #{resp_body}"}

      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, "Submit request failed: #{inspect(reason)}"}
    end
  end

  defp poll_and_reply(task_id, context, api_key, frame) do
    case wait_for_task(task_id, api_key, 0) do
      {:ok, result} ->
        output = Map.get(result, "output", %{})
        file_url = if is_map(output), do: Map.get(output, "file_url")

        if present?(file_url) do
          reply =
            %{
              "video_url" => file_url,
              "task_id" => task_id,
              "status" => Map.get(result, "status", "success"),
              "model" => context.model,
              "task" => context.task,
              "duration_seconds" => context.duration_seconds,
              "aspect_ratio" => context.aspect_ratio,
              "prompt" => context.prompt,
              "usage_info" => Map.get(result, "usage_info")
            }
            |> reject_nil()
            |> Jason.encode!()

          {:reply, Response.tool() |> Response.text(reply), frame}
        else
          error(frame, "Task succeeded but no video URL was returned: #{inspect(result)}")
        end

      {:error, reason} ->
        error(frame, reason)
    end
  end

  defp wait_for_task(_task_id, _api_key, attempt) when attempt >= @max_poll_attempts do
    {:error,
     "Timed out after #{div(@max_poll_attempts * @poll_interval_ms, 1000)}s waiting for the " <>
       "video task. Re-run `video_gen` with `task_id` to keep polling, or submit with `wait: false`."}
  end

  defp wait_for_task(task_id, api_key, attempt) do
    :timer.sleep(@poll_interval_ms)
    url = "#{@task_url}/#{task_id}"
    headers = [{"Authorization", "Bearer #{api_key}"}]

    case HTTPoison.get(
           url,
           headers,
           [recv_timeout: @poll_timeout_ms, timeout: @poll_timeout_ms] ++
             Exhub.TLSCompat.httpoison_opts(url)
         ) do
      {:ok, %HTTPoison.Response{status_code: 200, body: resp_body}} ->
        case Jason.decode(resp_body) do
          {:ok, result} ->
            case interpret_poll(result) do
              {:ok, _} = ok -> ok
              {:error, _} = err -> err
              :pending -> wait_for_task(task_id, api_key, attempt + 1)
            end

          {:error, reason} ->
            {:error, "Failed to decode poll response: #{inspect(reason)}"}
        end

      {:ok, %HTTPoison.Response{status_code: status, body: body}} ->
        {:error, "Poll HTTP #{status}: #{body}"}

      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, "Poll request failed: #{inspect(reason)}"}
    end
  end

  defp context_from(params) do
    %{
      model: normalize_string(Map.get(params, :model), @default_model),
      task: normalize_string(Map.get(params, :task), @t2va),
      duration_seconds: Map.get(params, :duration_seconds) || @default_duration,
      aspect_ratio: normalize_string(Map.get(params, :aspect_ratio), @default_aspect_ratio),
      prompt: Map.get(params, :prompt)
    }
  end

  defp error(frame, message) do
    resp = Response.tool() |> Response.error(message)
    {:reply, resp, frame}
  end

  defp normalize_string(value, default) when is_binary(value) do
    case String.trim(value) do
      "" -> default
      trimmed -> trimmed
    end
  end

  defp normalize_string(_value, default), do: default

  defp present?(value) when is_binary(value), do: String.trim(value) != ""
  defp present?(_value), do: false

  defp resolve_frame(params, key) do
    case Map.get(params, key) do
      value when is_binary(value) ->
        if present?(value), do: Exhub.MCP.ImageSource.resolve(value), else: {:ok, value}

      _absent ->
        {:ok, nil}
    end
  end

  defp put_frame(params, _key, nil), do: params
  defp put_frame(params, key, value), do: Map.put(params, key, value)

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, _key, ""), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)

  defp reject_nil(map) do
    map
    |> Enum.reject(fn {_key, value} -> is_nil(value) end)
    |> Map.new()
  end

  defp error_detail(result) do
    case Map.get(result, "message") do
      msg when is_binary(msg) and msg != "" -> " (#{msg})"
      _ -> ""
    end
  end
end
