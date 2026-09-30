defmodule Exhub.MCP.Tools.Speak do
  @moduledoc """
  MCP Tool for text-to-speech synthesis via Gitee AI / moark.com.

  Two backends are available, chosen with the `provider` parameter:

    * **`sync`** (default) — the OpenAI-compatible
      `POST https://ai.gitee.com/v1/audio/speech` endpoint, which returns the
      audio bytes directly. Driven by `Exhub.TTS.Sync`; default model
      `CosyVoice2`. Voice-cloning models (`IndexTTS-2`, `GLM-TTS`) take a
      `prompt_audio_url` reference clip.
    * **`async`** — the legacy MoArk async `Qwen3-TTS` flow: the text is
      submitted to `POST /v1/async/audio/speech`, which returns a `task_id`, and
      the generated audio URL is then polled from `GET /v1/task/{task_id}`.
      The model caps each request at 150 characters, so longer text is split
      into sentence-aligned segments and (when a local `output` path is given)
      the segments are concatenated into a single WAV file. Two modes are
      supported per segment: **voice design** (preset `speaker` + optional
      `instruction`) and **zero-shot voice clone** (`ref_audio` + `ref_text`).

  See `docs/modules/speak.md` for the tool reference.
  """

  alias Anubis.Server.Response
  alias Exhub.TTS.Sync

  use Anubis.Server.Component, type: :tool

  @submit_url "https://api.moark.com/v1/async/audio/speech"
  @task_url "https://api.moark.com/v1/task"

  @default_provider "sync"
  @valid_providers ~w(sync async)

  # Async (MoArk) speech backend.
  @async_default_model "Qwen3-TTS"
  @async_valid_models ~w(Qwen3-TTS)
  @async_default_speaker "Vivian"

  @default_output_format "mp3"

  # The model rejects inputs longer than this many characters.
  @max_input_chars 150

  # Preferred break points when splitting long text, most natural first.
  @sentence_breaks ["。", "！", "？", "；", "，", "、", "：", ".", "!", "?", ";", ",", ":"]

  # Polling budget stays under the server's 600_000 ms request timeout.
  @poll_interval_ms 5_000
  @max_poll_attempts 110
  @submit_timeout_ms 60_000
  @poll_timeout_ms 15_000
  @download_timeout_ms 120_000

  # The MoArk upstream occasionally returns a transient internal error; retry a
  # segment a few times before giving up.
  @max_submit_retries 4
  @retry_delay_ms 3_000
  @transient_markers ["unexpected error", "unparseable", "upstream returned", "server log"]

  def name, do: "speak"

  @impl true
  def description do
    """
    Synthesize speech from text via Gitee AI / moark.com.

    **Providers (chosen with `provider`):**

    - `sync` (default) — OpenAI-compatible synchronous TTS. The audio bytes are
      returned directly (no task/poll). Default model `CosyVoice2`
      (Chinese/English). Other models: `ChatTTS`, `Step-Audio-TTS-3B`, and the
      clone-only `IndexTTS-2` / `GLM-TTS` (which require `prompt_audio_url`).
      `CosyVoice3` and `Qwen3-TTS` are NOT available here.
    - `async` — legacy MoArk async `Qwen3-TTS`: submit, then poll until ready.
      Text longer than 150 chars is split into sentence-aligned segments and,
      when `output` is set, concatenated into one WAV file. Modes: **voice
      design** (`speaker` + optional `instruction`) or **zero-shot clone**
      (`ref_audio` + `ref_text`).

    **Parameters:**
    - `text`: the text to synthesize (required).
    - `provider`: `sync` (default) or `async`.
    - `voice`: preset voice for sync mode. Default: `alloy`.
    - `prompt_audio_url`: reference audio URL for the sync clone models
      (`IndexTTS-2`, `GLM-TTS`).
    - `prompt_text`: transcript of `prompt_audio_url` (optional).
    - `output`: absolute / `~` path to save the audio locally. The extension is
      corrected to the detected container (e.g. `.wav` for CosyVoice2). When
      omitted, no file is written.
    - `model`: TTS model. Defaults: `CosyVoice2` (sync) / `Qwen3-TTS` (async).

    **Async-only parameters:**
    - `speaker`: preset voice for voice-design mode. Default: `Vivian`.
    - `language`: language hint, e.g. `Chinese`, `English` (optional).
    - `instruction`: natural-language voice description (optional).
    - `ref_audio` / `ref_text`: reference audio **URL** + its transcript for
      zero-shot cloning.
    - `output_format`: only `mp3` is accepted by the API. Default `mp3`.
    - `wait`: wait for the result (default true); `false` submits and returns
      the `task_id`(s) immediately.
    - `task_id`: poll an existing task instead of submitting a new one.

    Returns JSON with `saved_path`, `status`, `model` and `format` (sync); or
    `audio_url`/`audio_urls`, `task_id`/`task_ids`, `segments`, `speaker`,
    `output_format` and `usage_info` (async).
    """
  end

  schema do
    field(:text, {:required, :string}, description: "Text to synthesize into speech.")

    field(:provider, :string,
      description: "TTS backend: `sync` (default) or `async`. See the description."
    )

    field(:model, :string,
      description: "Speech synthesis model. Defaults: `CosyVoice2` (sync) / `Qwen3-TTS` (async)."
    )

    field(:voice, :string, description: "Preset voice for sync mode. Default: alloy.")

    field(:prompt_audio_url, :string,
      description:
        "Reference audio URL for sync clone models (IndexTTS-2, GLM-TTS). Required by those models."
    )

    field(:prompt_text, :string,
      description: "Transcript of `prompt_audio_url` (sync clone mode). Optional."
    )

    field(:output, :string,
      description:
        "Absolute path or ~ shorthand to save the generated audio file. The extension is corrected to the detected container. Optional."
    )

    field(:speaker, :string,
      description: "Async only: preset voice for voice-design mode. Default: Vivian."
    )

    field(:language, :string, description: "Async only: language hint, e.g. Chinese, English.")

    field(:instruction, :string,
      description:
        "Async only: natural-language description of the desired voice (voice-design mode)."
    )

    field(:ref_audio, :string,
      description:
        "Async only: reference audio URL for zero-shot voice cloning (http/https). Requires `ref_text`."
    )

    field(:ref_text, :string,
      description: "Async only: transcript of `ref_audio`; required when `ref_audio` is set."
    )

    field(:output_format, :string,
      description: "Async only: audio output format. Only `mp3` is accepted by the API."
    )

    field(:wait, :boolean,
      description:
        "Async only: wait for the result (default: true). When false, returns the task_id immediately."
    )

    field(:task_id, :string,
      description: "Async only: existing task id to poll instead of submitting a new task."
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
        with {:ok, output_path} <- resolve_output(Map.get(params, :output)) do
          poll_and_reply(
            Map.get(params, :task_id),
            context_from(params),
            output_path,
            api_key,
            frame
          )
        end

      true ->
        with {:ok, validated} <- validate_params(params),
             {:ok, output_path} <- resolve_output(Map.get(params, :output)) do
          dispatch(validated, Map.get(params, :wait, true), output_path, api_key, frame)
        else
          {:error, reason} -> error(frame, reason)
        end
    end
  end

  # Routes to the sync (default) or async backend.
  defp dispatch(%{provider: "sync"} = validated, _wait, output_path, api_key, frame),
    do: run_sync(validated, output_path, api_key, frame)

  defp dispatch(%{provider: "async"} = validated, wait, output_path, api_key, frame),
    do: submit_and_reply(validated, wait, output_path, api_key, frame)

  # --- sync backend (Gitee AI /v1/audio/speech) ---

  defp run_sync(validated, output_path, api_key, frame) do
    opts = [
      api_key: api_key,
      model: validated.model,
      voice: validated.voice,
      prompt_audio_url: validated.prompt_audio_url,
      prompt_text: validated.prompt_text
    ]

    case Sync.synthesize(validated.text, opts) do
      {:ok, audio} ->
        case save_sync(audio, output_path) do
          {:ok, saved_path} ->
            reply =
              %{
                "status" => "success",
                "provider" => "sync",
                "model" => audio.model,
                "voice" => audio.voice,
                "format" => audio.format,
                "content_type" => audio.content_type,
                "bytes" => byte_size(audio.body),
                "saved_path" => saved_path
              }
              |> reject_nil()
              |> Jason.encode!()

            {:reply, Response.tool() |> Response.text(reply), frame}

          {:error, reason} ->
            error(frame, reason)
        end

      {:error, reason} ->
        error(frame, sync_error(reason))
    end
  end

  defp save_sync(_audio, nil), do: {:ok, nil}

  defp save_sync(audio, path) do
    final = Sync.with_format(path, audio.format)

    with :ok <- File.mkdir_p(Path.dirname(final)),
         :ok <- File.write(final, audio.body) do
      {:ok, final}
    else
      {:error, reason} when is_atom(reason) ->
        {:error, "Failed to write #{final}: #{inspect(reason)}"}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp sync_error(:missing_api_key), do: "Gitee AI API key not configured"

  defp sync_error({:http, status, message}),
    do: "Gitee AI speech API error (HTTP #{status}): #{message}"

  defp sync_error(reason), do: "Speech request failed: #{inspect(reason)}"

  # ---------------------------------------------------------------------------
  # Public, pure helpers (unit-tested)
  # ---------------------------------------------------------------------------

  @doc """
  Validates and normalizes `speak` params.

  The `provider` (`sync` default, or `async`) selects the backend and which
  fields apply. Returns `{:ok, validated}` or `{:error, message}`.
  """
  @spec validate_params(map()) :: {:ok, map()} | {:error, String.t()}
  def validate_params(params) when is_map(params) do
    case normalize_string(Map.get(params, :provider), @default_provider) do
      "sync" ->
        validate_sync(params)

      "async" ->
        validate_async(params)

      other ->
        {:error,
         "Invalid provider: #{other}. Valid providers: #{Enum.join(@valid_providers, ", ")}"}
    end
  end

  defp validate_sync(params) do
    model = normalize_string(Map.get(params, :model), Sync.default_model())
    text = Map.get(params, :text)
    voice = normalize_string(Map.get(params, :voice), Sync.default_voice())
    prompt_audio_url = Map.get(params, :prompt_audio_url)
    prompt_text = Map.get(params, :prompt_text)

    cond do
      not is_binary(text) or String.trim(text) == "" ->
        {:error, "`text` is required"}

      not Sync.model?(model) ->
        {:error,
         "Invalid model: #{model}. Valid sync models: #{Enum.join(Sync.valid_models(), ", ")}"}

      present?(prompt_audio_url) and not url?(prompt_audio_url) ->
        {:error, "`prompt_audio_url` must be an http(s) URL: #{inspect(prompt_audio_url)}"}

      Sync.clone_model?(model) and not present?(prompt_audio_url) ->
        {:error, "Model #{model} requires `prompt_audio_url` (voice cloning)"}

      true ->
        {:ok,
         %{
           provider: "sync",
           model: model,
           text: text,
           voice: voice,
           prompt_audio_url: prompt_audio_url,
           prompt_text: prompt_text
         }}
    end
  end

  defp validate_async(params) do
    model = normalize_string(Map.get(params, :model), @async_default_model)
    text = Map.get(params, :text)
    speaker = normalize_string(Map.get(params, :speaker), @async_default_speaker)
    ref_audio = Map.get(params, :ref_audio)
    ref_text = Map.get(params, :ref_text)
    output_format = normalize_string(Map.get(params, :output_format), @default_output_format)

    cond do
      not is_binary(text) or String.trim(text) == "" ->
        {:error, "`text` is required"}

      model not in @async_valid_models ->
        {:error,
         "Invalid model: #{model}. Valid async models: #{Enum.join(@async_valid_models, ", ")}"}

      present?(ref_audio) and not url?(ref_audio) ->
        {:error,
         "`ref_audio` must be an http(s) URL (local files are not supported): #{inspect(ref_audio)}"}

      present?(ref_audio) and not present?(ref_text) ->
        {:error,
         "`ref_text` is required when `ref_audio` is provided (transcript of the reference audio)"}

      true ->
        {:ok,
         %{
           provider: "async",
           model: model,
           text: text,
           speaker: speaker,
           language: Map.get(params, :language),
           instruction: Map.get(params, :instruction),
           ref_audio: ref_audio,
           ref_text: ref_text,
           output_format: output_format
         }}
    end
  end

  @doc """
  Builds the request body from validated params.

  `sync` → the OpenAI-compatible speech body (see `Exhub.TTS.Sync.build_body/2`);
  `async` → the MoArk `inputs` list shape.
  """
  @spec build_body(map()) :: map()
  def build_body(%{provider: "sync"} = validated) do
    Sync.build_body(validated.text,
      model: validated.model,
      voice: validated.voice,
      prompt_audio_url: validated.prompt_audio_url,
      prompt_text: validated.prompt_text
    )
  end

  def build_body(%{provider: "async"} = validated), do: async_body(validated)

  defp async_body(validated) do
    item =
      %{"prompt" => validated.text}
      |> maybe_put("language", validated.language)
      |> put_mode(validated)

    %{
      "inputs" => [item],
      "model" => validated.model,
      "output_format" => validated.output_format
    }
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

  @doc """
  Extracts every audio URL from a task's `output` payload.

  Handles both the Qwen3-TTS shape (`output.result[].audio_urls[].url`) and the
  generic `output.file_url` shape. Returns a de-duplicated list of URL strings
  (possibly empty).
  """
  @spec extract_audio_urls(map() | nil) :: [String.t()]
  def extract_audio_urls(output) when is_map(output) do
    direct =
      [output["file_url"], output["audio_url"], output["url"]]
      |> Enum.filter(&non_empty_string?/1)

    nested =
      output
      |> Map.get("result", [])
      |> List.wrap()
      |> Enum.flat_map(&collect_urls/1)

    (direct ++ nested) |> Enum.uniq()
  end

  def extract_audio_urls(_output), do: []

  @doc """
  Splits `text` into segments no longer than the model's input limit (150 chars).

  Breaks on the most natural boundary that fits, preferring newlines, then
  sentence/clause punctuation, then spaces; falls back to a hard cut when the
  text has no break point. Never splits a multibyte grapheme, and preserves
  order (rejoining the segments reproduces the input). Whitespace-only segments
  are dropped.
  """
  @spec chunk_text(String.t()) :: [String.t()]
  def chunk_text(text) when is_binary(text) do
    text
    |> String.graphemes()
    |> do_chunk([])
    |> Enum.reject(&(String.trim(&1) == ""))
  end

  @doc """
  Concatenates a list of RIFF/WAVE PCM binaries into a single WAV binary.

  The `fmt ` chunk of the first input is reused and the `data` chunks are
  combined. Returns `{:ok, binary}` or `{:error, reason}`.
  """
  @spec concat_wav([binary()]) :: {:ok, binary()} | {:error, String.t()}
  def concat_wav([_ | _] = wavs) do
    with {:ok, parts} <- parse_all(wavs) do
      [{fmt, _} | _] = parts
      data = parts |> Enum.flat_map(fn {_fmt, datas} -> datas end) |> IO.iodata_to_binary()
      {:ok, build_wav(fmt, data)}
    end
  end

  def concat_wav(_), do: {:error, "no audio to concatenate"}

  # ---------------------------------------------------------------------------
  # Private helpers
  # ---------------------------------------------------------------------------

  defp submit_and_reply(validated, wait, output_path, api_key, frame) do
    chunks = chunk_text(validated.text)

    cond do
      chunks == [] ->
        error(frame, "`text` is required")

      wait == false ->
        submit_only(chunks, validated, api_key, frame)

      true ->
        run_chunks(chunks, validated, output_path, api_key, frame)
    end
  end

  defp submit_only(chunks, validated, api_key, frame) do
    case submit_all(chunks, validated, api_key) do
      {:ok, task_ids} ->
        reply =
          Jason.encode!(%{
            "task_id" => List.first(task_ids),
            "task_ids" => task_ids,
            "segments" => length(task_ids),
            "status" => "submitted",
            "model" => validated.model,
            "speaker" => validated.speaker,
            "output_format" => validated.output_format,
            "audio_url" => nil
          })

        {:reply, Response.tool() |> Response.text(reply), frame}

      {:error, reason} ->
        error(frame, reason)
    end
  end

  defp run_chunks(chunks, validated, output_path, api_key, frame) do
    case submit_and_wait_all(chunks, validated, api_key) do
      {:ok, results} ->
        urls = results |> Enum.flat_map(&extract_audio_urls(&1.output)) |> Enum.uniq()

        if urls == [] do
          error(frame, "Task succeeded but no audio URL was returned")
        else
          case save_audio(urls, output_path) do
            {:ok, saved_path} ->
              reply =
                %{
                  "audio_url" => List.first(urls),
                  "audio_urls" => urls,
                  "saved_path" => saved_path,
                  "segments" => length(chunks),
                  "task_id" => results |> List.first() |> Map.get(:task_id),
                  "task_ids" => Enum.map(results, & &1.task_id),
                  "status" => "success",
                  "model" => validated.model,
                  "speaker" => validated.speaker,
                  "output_format" => validated.output_format,
                  "usage_info" => results |> List.last() |> Map.get(:usage_info)
                }
                |> reject_nil()
                |> Jason.encode!()

              {:reply, Response.tool() |> Response.text(reply), frame}

            {:error, reason} ->
              error(frame, reason)
          end
        end

      {:error, reason} ->
        error(frame, reason)
    end
  end

  defp submit_all(chunks, validated, api_key) do
    Enum.reduce_while(chunks, {:ok, []}, fn chunk, {:ok, acc} ->
      case submit_task(build_body(%{validated | text: chunk}), api_key) do
        {:ok, task_id} -> {:cont, {:ok, acc ++ [task_id]}}
        {:error, _} = err -> {:halt, err}
      end
    end)
  end

  defp submit_and_wait_all(chunks, validated, api_key) do
    Enum.reduce_while(chunks, {:ok, []}, fn chunk, {:ok, acc} ->
      case submit_and_wait(build_body(%{validated | text: chunk}), api_key, @max_submit_retries) do
        {:ok, item} -> {:cont, {:ok, acc ++ [item]}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  # Submits a segment and waits for it, retrying transient upstream failures.
  defp submit_and_wait(body, api_key, attempts) do
    case submit_task(body, api_key) do
      {:ok, task_id} ->
        case wait_for_task(task_id, api_key, 0) do
          {:ok, result} -> {:ok, task_result(task_id, result)}
          {:error, reason} -> retry_transient(body, api_key, attempts, reason)
        end

      {:error, reason} ->
        retry_transient(body, api_key, attempts, reason)
    end
  end

  defp retry_transient(body, api_key, attempts, reason) when attempts > 1 do
    if transient?(reason) do
      :timer.sleep(@retry_delay_ms)
      submit_and_wait(body, api_key, attempts - 1)
    else
      {:error, reason}
    end
  end

  defp retry_transient(_body, _api_key, _attempts, reason), do: {:error, reason}

  defp task_result(task_id, result) do
    %{
      task_id: task_id,
      output: Map.get(result, "output", %{}),
      usage_info: Map.get(result, "usage_info")
    }
  end

  defp transient?(reason) when is_binary(reason) do
    downcased = String.downcase(reason)
    Enum.any?(@transient_markers, &String.contains?(downcased, &1))
  end

  defp transient?(_reason), do: false

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
        {:error, "MoArk speech API error (HTTP #{status}): #{resp_body}"}

      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, "Submit request failed: #{inspect(reason)}"}
    end
  end

  # Single-task polling path (`task_id` param), kept for resuming a timed-out call.
  defp poll_and_reply(task_id, context, output_path, api_key, frame) do
    case wait_for_task(task_id, api_key, 0) do
      {:ok, result} ->
        output = Map.get(result, "output", %{})
        urls = extract_audio_urls(output)

        if urls == [] do
          error(frame, "Task succeeded but no audio URL was returned: #{inspect(result)}")
        else
          case save_audio(urls, output_path) do
            {:ok, saved_path} ->
              reply =
                %{
                  "audio_url" => List.first(urls),
                  "audio_urls" => urls,
                  "saved_path" => saved_path,
                  "segments" => 1,
                  "task_id" => task_id,
                  "status" => Map.get(result, "status", "success"),
                  "model" => context.model,
                  "speaker" => context.speaker,
                  "output_format" => context.output_format,
                  "usage_info" => Map.get(result, "usage_info")
                }
                |> reject_nil()
                |> Jason.encode!()

              {:reply, Response.tool() |> Response.text(reply), frame}

            {:error, reason} ->
              error(frame, reason)
          end
        end

      {:error, reason} ->
        error(frame, reason)
    end
  end

  defp wait_for_task(_task_id, _api_key, attempt) when attempt >= @max_poll_attempts do
    {:error,
     "Timed out after #{div(@max_poll_attempts * @poll_interval_ms, 1000)}s waiting for the " <>
       "speech task. Re-run `speak` with `task_id` to keep polling, or submit with `wait: false`."}
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

  defp save_audio(_urls, nil), do: {:ok, nil}

  defp save_audio(urls, path) do
    with {:ok, bodies} <- download_all(urls),
         {:ok, binary} <- assemble(bodies),
         :ok <- File.write(path, binary) do
      {:ok, path}
    else
      {:error, reason} when is_atom(reason) ->
        {:error, "Failed to write #{path}: #{inspect(reason)}"}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp download_all(urls) do
    Enum.reduce_while(urls, {:ok, []}, fn url, {:ok, acc} ->
      case download(url) do
        {:ok, body} -> {:cont, {:ok, [body | acc]}}
        {:error, _} = err -> {:halt, err}
      end
    end)
    |> case do
      {:ok, bodies} -> {:ok, Enum.reverse(bodies)}
      err -> err
    end
  end

  defp assemble([body]), do: {:ok, body}

  defp assemble(bodies) do
    case concat_wav(bodies) do
      {:ok, binary} -> {:ok, binary}
      {:error, reason} -> {:error, "Failed to concatenate audio segments: #{reason}"}
    end
  end

  defp download(file_url) do
    opts =
      [recv_timeout: @download_timeout_ms, timeout: @download_timeout_ms] ++
        Exhub.TLSCompat.httpoison_opts(file_url)

    case HTTPoison.get(file_url, [], opts) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        {:ok, body}

      {:ok, %HTTPoison.Response{status_code: status}} ->
        {:error, "Failed to download audio (HTTP #{status})"}

      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, "Failed to download audio: #{inspect(reason)}"}
    end
  end

  defp resolve_output(nil), do: {:ok, nil}
  defp resolve_output(""), do: {:ok, nil}

  defp resolve_output(path) when is_binary(path) do
    if String.starts_with?(path, ["/", "~"]) do
      Exhub.MCP.Desktop.Helpers.validate_absolute_path(path)
    else
      {:error,
       "Relative paths are not supported for `output`: '#{path}'. Use an absolute path or ~ shorthand."}
    end
  end

  defp resolve_output(_path), do: {:error, "Invalid `output` path."}

  defp context_from(params) do
    %{
      model: normalize_string(Map.get(params, :model), @async_default_model),
      speaker: normalize_string(Map.get(params, :speaker), @async_default_speaker),
      output_format: normalize_string(Map.get(params, :output_format), @default_output_format)
    }
  end

  defp do_chunk([], acc), do: Enum.reverse(acc)

  defp do_chunk(graphemes, acc) do
    if length(graphemes) <= @max_input_chars do
      Enum.reverse([Enum.join(graphemes) | acc])
    else
      cut = cut_index(graphemes)
      do_chunk(Enum.drop(graphemes, cut), [graphemes |> Enum.take(cut) |> Enum.join() | acc])
    end
  end

  defp cut_index(graphemes) do
    window = Enum.take(graphemes, @max_input_chars)

    last_index(window, ["\n"]) ||
      last_index(window, @sentence_breaks) ||
      last_index(window, [" ", "\t"]) ||
      @max_input_chars
  end

  defp last_index(window, targets) do
    window
    |> Enum.with_index(1)
    |> Enum.reduce(nil, fn {grapheme, index}, acc ->
      if grapheme in targets, do: index, else: acc
    end)
  end

  defp parse_all(wavs) do
    wavs
    |> Enum.reduce_while({:ok, []}, fn wav, {:ok, acc} ->
      case parse_wav(wav) do
        {:ok, part} -> {:cont, {:ok, [part | acc]}}
        {:error, _} = err -> {:halt, err}
      end
    end)
    |> case do
      {:ok, parts} -> {:ok, Enum.reverse(parts)}
      err -> err
    end
  end

  defp parse_wav(<<"RIFF", _size::little-32, "WAVE", rest::binary>>) do
    chunks = parse_chunks(rest)
    fmt = Enum.find_value(chunks, fn {id, data} -> if id == "fmt ", do: data end)
    datas = for {"data", data} <- chunks, do: data

    if fmt && datas != [] do
      {:ok, {fmt, datas}}
    else
      {:error, "invalid WAV: missing fmt/data chunk"}
    end
  end

  defp parse_wav(_), do: {:error, "invalid WAV: missing RIFF/WAVE header"}

  defp parse_chunks(bin, acc \\ [])

  defp parse_chunks(<<id::binary-size(4), size::little-32, rest::binary>>, acc)
       when byte_size(rest) >= size do
    data = binary_part(rest, 0, size)
    offset = size + rem(size, 2)

    tail =
      if byte_size(rest) >= offset,
        do: binary_part(rest, offset, byte_size(rest) - offset),
        else: <<>>

    parse_chunks(tail, [{id, data} | acc])
  end

  defp parse_chunks(_bin, acc), do: Enum.reverse(acc)

  defp build_wav(fmt, data) do
    body = "WAVE" <> chunk("fmt ", fmt) <> chunk("data", data)
    "RIFF" <> <<byte_size(body)::little-32>> <> body
  end

  defp chunk(id, data) do
    pad = if rem(byte_size(data), 2) == 1, do: <<0>>, else: <<>>
    id <> <<byte_size(data)::little-32>> <> data <> pad
  end

  defp collect_urls(%{} = segment) do
    segment
    |> Map.get("audio_urls", [])
    |> List.wrap()
    |> Enum.flat_map(fn
      %{"url" => url} when is_binary(url) and url != "" -> [url]
      url when is_binary(url) and url != "" -> [url]
      _ -> []
    end)
  end

  defp collect_urls(url) when is_binary(url) and url != "", do: [url]
  defp collect_urls(_), do: []

  defp non_empty_string?(value), do: is_binary(value) and value != ""

  defp put_mode(item, validated) do
    if present?(validated.ref_audio) do
      item
      |> Map.put("prompt_text", validated.ref_text)
      |> Map.put("prompt_audio_url", validated.ref_audio)
    else
      item
      |> Map.put("speaker", validated.speaker)
      |> maybe_put("instruction", validated.instruction)
    end
  end

  defp normalize_string(value, default) when is_binary(value) do
    case String.trim(value) do
      "" -> default
      trimmed -> trimmed
    end
  end

  defp normalize_string(_value, default), do: default

  defp url?(value) when is_binary(value),
    do: String.starts_with?(value, ["http://", "https://"])

  defp url?(_value), do: false

  defp present?(value) when is_binary(value), do: String.trim(value) != ""
  defp present?(_value), do: false

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, _key, ""), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)

  defp reject_nil(map) do
    map
    |> Enum.reject(fn {_key, value} -> is_nil(value) end)
    |> Map.new()
  end

  defp error_detail(result) do
    detail = Map.get(result, "message") || get_in(result, ["output", "error"])

    case detail do
      msg when is_binary(msg) and msg != "" -> " (#{String.trim(msg)})"
      _ -> ""
    end
  end

  defp error(frame, message) do
    resp = Response.tool() |> Response.error(message)
    {:reply, resp, frame}
  end
end
