defmodule Exhub.MCP.Tools.Look do
  @moduledoc """
  MCP Tool for image understanding using Gitee AI vision models.

  Accepts local file paths or remote URLs, sends images to vision models
  (DeepSeek V4.1 Flash, Qwen3.8 Omni, Kimi, etc.) and returns
  extracted/analyzed information.

  `mode: "text"` is a transcription preset tuned for terminal / code / UI
  screenshots: it asks for the raw text grid only (no Markdown, no box art),
  disables the model's reasoning pass and allows a larger output budget.

  Three knobs make it usable as a *screen reader* for canvas-rendered
  terminals (`webterm` drives them):

    * `max_lines`   — transcribe only the last N visible lines (cheaper, and
      avoids the model's output-token ceiling on a full screen);
    * `only_new`    — pass the previous transcription and get back only the
      lines that changed (polling a terminal without re-reading all of it);
    * `expect`      — substrings that must appear; the reply gets an
      `[expect: ...]` verdict appended so the caller can self-check instead of
      silently trusting the OCR.

  Terminal screenshots should NOT be routed through `doc_extract` (the async
  document-OCR API): document layout reconstruction is the wrong shape for
  monospaced full-line text and the OCR pipeline fails or times out on them.
  """

  alias Anubis.Server.Response

  use Anubis.Server.Component, type: :tool

  @api_url "https://api.moark.com/v1/chat/completions"

  @default_model "deepseek-v4.1-flash"
  @max_tokens 4096
  @text_max_tokens 8192

  @supported_image_exts ~w(.png .jpg .jpeg .gif .webp .bmp)

  @valid_models ~w(
    deepseek-v4.1-flash
    kimi-k2.6
    kimi-k2.5
    qwen3.5-122b-a10b
    qwen3.8-omni-flash
    minimax-m3.1-flash
  )

  @default_mode "analyze"
  @text_mode "text"

  @mode_aliases %{
    "ocr" => @text_mode,
    "transcribe" => @text_mode,
    "transcription" => @text_mode
  }

  @text_prompt """
  Transcribe every character of text in this screenshot exactly as it appears.

  - Preserve line breaks, indentation and spacing; keep one visual line per line.
  - Do not correct typos, complete truncated commands, translate or summarize.
  - Do not reformat as Markdown, add code fences, headings or comments.
  - Do not draw box borders or characters that are not visible in the image.
  - Output only the transcribed text, with no preamble or commentary.
  """

  @truncation_warning "\n\n[truncated: the model hit its output-token limit — capture a smaller region or ask for less]"

  def name, do: "look"

  @impl true
  def description do
    """
    Analyze images using AI vision models via Gitee AI.

    Accepts local file paths or remote URLs. Use this to extract text,
    describe contents, answer questions about images, or analyze visual data.

    **Modes:**
    - `analyze` (default) — answer `prompt` about the image.
    - `text` — exact text transcription, tuned for terminal / code / log /
      UI screenshots. `prompt` becomes optional (extra instructions are
      appended to a strict transcription prompt). Disables model reasoning
      for lower latency and returns raw text without Markdown or box art.

    **Screen-reading controls (mode `text`):**
    - `max_lines` — only the last N visible lines (use ~30-40 for a terminal).
    - `only_new` — previous transcription; returns only changed lines. Ideal
      when polling a terminal that is still producing output.
    - `expect` — newline-separated substrings that must appear in the result;
      the reply ends with `[expect: ok]` or `[expect: MISSING ...]`.

    For plain text extraction prefer `mode: "text"` — it is materially faster
    and cleaner than asking for a transcription through `analyze`. Do not use
    `doc_extract` for terminal screenshots: its document-OCR pipeline fails on
    them.

    **Supported models:**
    - `deepseek-v4.1-flash` (default) — DeepSeek V4.1 Flash vision model
    - `kimi-k2.6` — Moonshot AI vision model
    - `kimi-k2.5` — Moonshot AI vision model
    - `qwen3.5-122b-a10b` — Alibaba Qwen3.5 vision model
    - `qwen3.8-omni-flash` — Alibaba Qwen3.8 native omni-modal model (1M context)
    - `minimax-m3.1-flash` — MiniMax M3.1 Flash multimodal model (avoid for long
      transcriptions; it can return an empty string)

    **Supported formats:** PNG, JPG, JPEG, GIF, WebP, BMP

    Returns the model's analysis as text (or JSON if response_format="json").
    """
  end

  schema do
    field(:image, {:required, :string},
      description:
        "Local file path: Absolute path or ~ shorthand to the file (e.g. /path/to/img.png) or remote URL: (https://...)"
    )

    field(:prompt, :string,
      description:
        "What to extract or analyze from the image. Required unless mode is \"text\" (then it is optional and appended to the transcription prompt)"
    )

    field(:mode, :string,
      description:
        "\"analyze\" (default) or \"text\" (exact transcription, best for terminal/code screenshots; aliases: ocr, transcribe)"
    )

    field(:model, :string, description: "Vision model to use. Default: deepseek-v4.1-flash")

    field(:response_format, :string, description: "Output format: 'text' (default) or 'json'")

    field(:thinking, :boolean,
      description:
        "Enable the model's reasoning pass. Defaults to true in analyze mode and false in text mode"
    )

    field(:max_lines, :integer,
      description:
        "mode \"text\" only: transcribe at most the last N visible lines (default: everything). Use ~30-40 for a terminal screen"
    )

    field(:only_new, :string,
      description:
        "mode \"text\" only: the previous transcription. Lines that already appear in it verbatim are dropped, so a polling caller only sees new output"
    )

    field(:expect, :string,
      description:
        "mode \"text\" only: newline-separated substrings that must appear in the result. The reply ends with [expect: ok] or [expect: MISSING ...]"
    )
  end

  @impl true
  def execute(params, frame) do
    image = Map.get(params, :image)
    prompt = Map.get(params, :prompt)
    model = (Map.get(params, :model, @default_model) || @default_model) |> String.downcase()
    response_format = Map.get(params, :response_format, "text") || "text"
    thinking = Map.get(params, :thinking)

    with :ok <- validate_image(image),
         {:ok, mode} <- normalize_mode(Map.get(params, :mode)),
         :ok <- validate_prompt(mode, prompt),
         :ok <- validate_model(model) do
      opts = %{
        prompt: prompt,
        model: model,
        response_format: response_format,
        mode: mode,
        thinking: thinking,
        max_lines: Map.get(params, :max_lines),
        only_new: Map.get(params, :only_new),
        expect: Map.get(params, :expect)
      }

      case analyze(image, opts) do
        {:ok, text} -> {:reply, Response.tool() |> Response.text(text), frame}
        {:error, reason} -> error_reply(reason, frame)
      end
    else
      {:error, reason} -> error_reply(reason, frame)
    end
  end

  @doc """
  Runs one vision request and returns the (post-processed) text.

  Shared with `Exhub.MCP.Tools.WebTerm`, which feeds terminal screenshots in
  and treats the result as the screen buffer.

  Options: `:prompt`, `:mode`, `:model`, `:response_format`, `:thinking`,
  `:max_lines`, `:only_new`, `:expect`.
  """
  @spec analyze(String.t(), map() | keyword()) :: {:ok, String.t()} | {:error, String.t()}
  def analyze(image, opts) do
    mode = get_opt(opts, :mode, @default_mode)
    prompt = get_opt(opts, :prompt, nil)
    api_key = Application.get_env(:exhub, :giteeai_api_key, "")

    cond do
      api_key == "" ->
        {:error,
         "Gitee AI API key not configured. Run: mix scr.insert dev giteeai_api_key \"your-key\""}

      not is_binary(image) or blank?(image) ->
        {:error, "`image` is required — provide a local path or URL"}

      mode == @default_mode and blank?(prompt) ->
        {:error, "`prompt` is required — describe what to extract or analyze"}

      true ->
        do_analyze(image, compose_prompt(mode, prompt, opts), opts, api_key)
    end
  end

  # --- Validation ---

  defp validate_image(image) when is_binary(image) do
    if blank?(image),
      do: {:error, "`image` is required — provide a local path or URL"},
      else: :ok
  end

  defp validate_image(_), do: {:error, "`image` is required — provide a local path or URL"}

  defp validate_prompt(@text_mode, _prompt), do: :ok

  defp validate_prompt(_mode, prompt) do
    if blank?(prompt),
      do: {:error, "`prompt` is required — describe what to extract or analyze"},
      else: :ok
  end

  defp validate_model(model) do
    if model in @valid_models do
      :ok
    else
      {:error, "Invalid model: #{model}. Valid models: #{Enum.join(@valid_models, ", ")}"}
    end
  end

  defp blank?(value), do: is_nil(value) or String.trim(to_string(value)) == ""

  # --- Mode / prompt handling ---

  @doc false
  def normalize_mode(nil), do: {:ok, @default_mode}

  def normalize_mode(mode) when is_binary(mode) do
    mode = mode |> String.trim() |> String.downcase()
    mode = Map.get(@mode_aliases, mode, mode)

    if mode in [@default_mode, @text_mode],
      do: {:ok, mode},
      else: {:error, "Invalid mode: #{mode}. Valid modes: #{@default_mode}, #{@text_mode}"}
  end

  def normalize_mode(mode) do
    {:error, "Invalid mode: #{inspect(mode)}. Valid modes: #{@default_mode}, #{@text_mode}"}
  end

  @doc false
  def compose_prompt(@text_mode, prompt) do
    if blank?(prompt) do
      @text_prompt
    else
      @text_prompt <>
        "\nAdditional instruction (apply it while keeping the transcription exact): " <>
        String.trim(prompt) <> "\n"
    end
  end

  def compose_prompt(_mode, prompt), do: prompt

  @doc false
  def compose_prompt(@text_mode, prompt, opts) do
    compose_prompt(@text_mode, prompt)
    |> maybe_add_max_lines(get_opt(opts, :max_lines, nil))
    |> maybe_add_only_new(get_opt(opts, :only_new, nil))
  end

  def compose_prompt(mode, prompt, _opts), do: compose_prompt(mode, prompt)

  defp maybe_add_max_lines(prompt, nil), do: prompt

  defp maybe_add_max_lines(prompt, n) when is_integer(n) and n > 0 do
    prompt <>
      "\nOutput at most the last #{n} visible lines of the terminal — drop everything " <>
      "earlier, keep the surviving lines in their original order, do not summarise them.\n"
  end

  defp maybe_add_max_lines(prompt, _), do: prompt

  defp maybe_add_only_new(prompt, known) when is_binary(known) do
    if blank?(known) do
      prompt
    else
      prompt <>
        "\nA previous transcription is given below. Output ONLY lines that are new or changed " <>
        "compared with it. If the screen shows nothing new, output an empty response.\n" <>
        "--- BEGIN KNOWN TEXT ---\n" <> String.trim(known) <> "\n--- END KNOWN TEXT ---\n"
    end
  end

  defp maybe_add_only_new(prompt, _), do: prompt

  # --- Private helpers ---

  defp do_analyze(image, prompt, opts, api_key) do
    with {:ok, image_content} <- prepare_image_content(image) do
      messages = build_messages(prompt, image_content)

      body =
        build_request_body(
          get_opt(opts, :model, @default_model),
          messages,
          get_opt(opts, :response_format, "text"),
          opts
        )

      headers = [
        {"Content-Type", "application/json"},
        {"Authorization", "Bearer #{api_key}"}
      ]

      case HTTPoison.post(
             @api_url,
             Jason.encode!(body),
             headers,
             [recv_timeout: 120_000, timeout: 120_000] ++ Exhub.TLSCompat.httpoison_opts(@api_url)
           ) do
        {:ok, %HTTPoison.Response{status_code: 200, body: resp_body}} ->
          with {:ok, text, finish_reason} <-
                 extract_text(resp_body, get_opt(opts, :mode, @default_mode)) do
            {:ok, text |> postprocess(opts) |> maybe_warn_truncated(finish_reason)}
          end

        {:ok, %HTTPoison.Response{status_code: status, body: resp_body}} ->
          {:error, "Gitee AI API error (HTTP #{status}): #{resp_body}"}

        {:error, %HTTPoison.Error{reason: reason}} ->
          {:error, "HTTP request failed: #{inspect(reason)}"}
      end
    end
  end

  defp error_reply(reason, frame) do
    resp = Response.tool() |> Response.error(reason)
    {:reply, resp, frame}
  end

  defp prepare_image_content(image) do
    cond do
      String.starts_with?(image, ["http://", "https://"]) ->
        {:ok, %{type: "image_url", image_url: %{url: image}}}

      String.starts_with?(image, ["/", "~"]) ->
        # Absolute or tilde-based path - validate and expand
        case Exhub.MCP.Desktop.Helpers.validate_absolute_path(image) do
          {:ok, expanded_path} ->
            if File.exists?(expanded_path) do
              ext = Path.extname(expanded_path) |> String.downcase()

              if ext in @supported_image_exts do
                case File.read(expanded_path) do
                  {:ok, binary} ->
                    b64 = Base.encode64(binary)
                    mime = ext_to_mime(ext)
                    data_uri = "data:#{mime};base64,#{b64}"
                    {:ok, %{type: "image_url", image_url: %{url: data_uri}}}

                  {:error, reason} ->
                    {:error, "Cannot read file #{expanded_path}: #{inspect(reason)}"}
                end
              else
                {:error,
                 "Unsupported image format: #{ext}. Supported: #{Enum.join(@supported_image_exts, ", ")}"}
              end
            else
              {:error, "File not found: #{expanded_path}"}
            end

          {:error, reason} ->
            {:error, reason}
        end

      true ->
        {:error,
         "Relative paths are not supported: '#{image}'. Use an absolute path (e.g. /path/to/image.png), ~ shorthand (e.g. ~/path/to/image.png), or a URL (https://...)."}
    end
  end

  defp ext_to_mime(".png"), do: "image/png"
  defp ext_to_mime(".jpg"), do: "image/jpeg"
  defp ext_to_mime(".jpeg"), do: "image/jpeg"
  defp ext_to_mime(".gif"), do: "image/gif"
  defp ext_to_mime(".webp"), do: "image/webp"
  defp ext_to_mime(".bmp"), do: "image/bmp"
  defp ext_to_mime(_), do: "application/octet-stream"

  defp build_messages(prompt, image_content) do
    [
      %{
        role: "user",
        content: [
          %{type: "text", text: prompt},
          image_content
        ]
      }
    ]
  end

  @doc false
  def build_request_body(model, messages, response_format, opts) do
    mode = get_opt(opts, :mode, @default_mode)
    thinking = get_opt(opts, :thinking, nil)

    %{
      model: model,
      messages: messages,
      max_tokens: max_tokens_for(mode)
    }
    |> maybe_put_thinking(thinking_enabled?(mode, thinking))
    |> maybe_put_temperature(mode)
    |> maybe_put_response_format(response_format)
  end

  @doc false
  def thinking_enabled?(@text_mode, nil), do: false
  def thinking_enabled?(_mode, nil), do: true
  def thinking_enabled?(_mode, value) when is_boolean(value), do: value
  def thinking_enabled?(mode, _value), do: thinking_enabled?(mode, nil)

  defp get_opt(opts, key, default) when is_list(opts), do: Keyword.get(opts, key, default)
  defp get_opt(opts, key, default), do: Map.get(opts, key, default)

  defp max_tokens_for(@text_mode), do: @text_max_tokens
  defp max_tokens_for(_mode), do: @max_tokens

  defp maybe_put_thinking(body, true), do: body
  defp maybe_put_thinking(body, false), do: Map.put(body, :thinking, %{type: "disabled"})

  defp maybe_put_temperature(body, @text_mode), do: Map.put(body, :temperature, 0)
  defp maybe_put_temperature(body, _mode), do: body

  defp maybe_put_response_format(body, "json"),
    do: Map.put(body, :response_format, %{type: "json_object"})

  defp maybe_put_response_format(body, _response_format), do: body

  @doc false
  def strip_code_fences(text) when is_binary(text) do
    case Regex.run(~r/\A```[^\n]*\n(.*?)\n```\z/s, String.trim(text)) do
      [_, inner] -> inner
      _ -> text
    end
  end

  @doc """
  Applies the screen-reading post-processing to a raw transcription.

  Order: drop lines already known (`:only_new`) -> keep the last `:max_lines`
  -> append the `:expect` verdict.
  """
  @spec postprocess(String.t(), map() | keyword()) :: String.t()
  def postprocess(text, opts) when is_binary(text) do
    text
    |> drop_known_lines(get_opt(opts, :only_new, nil))
    |> keep_last_lines(get_opt(opts, :max_lines, nil))
    |> append_expect_status(get_opt(opts, :expect, nil))
  end

  defp drop_known_lines(text, known) when is_binary(known) do
    if blank?(known) do
      text
    else
      known_set =
        known
        |> String.split("\n")
        |> Enum.map(&String.trim/1)
        |> Enum.reject(&(&1 == ""))
        |> MapSet.new()

      text
      |> String.split("\n")
      |> Enum.reject(fn line ->
        trimmed = String.trim(line)
        trimmed != "" and MapSet.member?(known_set, trimmed)
      end)
      |> Enum.join("\n")
    end
  end

  defp drop_known_lines(text, _), do: text

  defp keep_last_lines(text, n) when is_integer(n) and n > 0 do
    text
    |> String.split("\n")
    |> Enum.take(-n)
    |> Enum.join("\n")
  end

  defp keep_last_lines(text, _), do: text

  defp append_expect_status(text, expect) when is_binary(expect) do
    wanted = expect |> String.split("\n") |> Enum.map(&String.trim/1) |> Enum.reject(&(&1 == ""))

    case wanted do
      [] ->
        text

      list ->
        missing = Enum.reject(list, &String.contains?(text, &1))

        case missing do
          [] -> text <> "\n[expect: ok #{length(list)}]"
          _ -> text <> "\n[expect: MISSING " <> Enum.join(missing, " | ") <> "]"
        end
    end
  end

  defp append_expect_status(text, _), do: text

  defp extract_text(resp_body, mode) do
    case Jason.decode(resp_body) do
      {:ok, %{"choices" => [choice | _]}} ->
        content = get_in(choice, ["message", "content"])

        if is_binary(content) do
          text = if mode == @text_mode, do: strip_code_fences(content), else: content
          {:ok, text, choice["finish_reason"]}
        else
          {:error, "Unexpected API response: #{inspect(choice)}"}
        end

      {:ok, decoded} ->
        {:error, "Unexpected API response: #{inspect(decoded)}"}

      {:error, reason} ->
        {:error, "Failed to decode API response: #{inspect(reason)}"}
    end
  end

  defp maybe_warn_truncated(text, "length"), do: text <> @truncation_warning
  defp maybe_warn_truncated(text, _finish_reason), do: text
end
