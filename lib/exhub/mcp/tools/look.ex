defmodule Exhub.MCP.Tools.Look do
  @moduledoc """
  MCP Tool for image understanding using Gitee AI vision models.

  Accepts local file paths or remote URLs, sends images to vision models
  (Qwen-VL, GLM-4V, etc.) and returns extracted/analyzed information.
  """

  alias Anubis.Server.Response

  use Anubis.Server.Component, type: :tool

  @api_url "https://ai.gitee.com/v1/chat/completions"

  @default_model "glm-5v-turbo"
  @max_tokens 4096

  @supported_image_exts ~w(.png .jpg .jpeg .gif .webp .bmp)

  @valid_models ~w(
    glm-5v-turbo
    Qwen2.5-VL-72B-Instruct
    Qwen2-VL-72B-Instruct
    GLM-4V
    deepseek-vl
  )

  def name, do: "look"

  @impl true
  def description do
    """
    Analyze images using AI vision models via Gitee AI.

    Accepts local file paths or remote URLs. Use this to extract text,
    describe contents, answer questions about images, or analyze visual data.

    **Supported models:**
    - `glm-5v-turbo` (default) — Zhipu AI efficient vision model
    - `Qwen2.5-VL-72B-Instruct` — Alibaba's flagship vision model
    - `Qwen2-VL-72B-Instruct` — Previous generation Qwen
    - `GLM-4V` — Zhipu AI vision model
    - `deepseek-vl` — DeepSeek vision

    **Supported formats:** PNG, JPG, JPEG, GIF, WebP, BMP

    Returns the model's analysis as text (or JSON if response_format="json").
    """
  end

  schema do
    field(:image, {:required, :string},
      description: "Local file path (e.g. /path/to/img.png) or remote URL (https://...)"
    )

    field(:prompt, {:required, :string},
      description: "What to extract or analyze from the image"
    )

    field(:model, :string,
      description: "Vision model to use. Default: glm-5v-turbo"
    )

    field(:response_format, :string,
      description: "Output format: 'text' (default) or 'json'"
    )
  end

  @impl true
  def execute(params, frame) do
    image = Map.get(params, :image)
    prompt = Map.get(params, :prompt)
    model = Map.get(params, :model, @default_model) || @default_model
    response_format = Map.get(params, :response_format, "text") || "text"

    cond do
      is_nil(image) or image == "" ->
        resp = Response.tool() |> Response.error("`image` is required — provide a local path or URL")
        {:reply, resp, frame}

      is_nil(prompt) or prompt == "" ->
        resp = Response.tool() |> Response.error("`prompt` is required — describe what to extract or analyze")
        {:reply, resp, frame}

      model not in @valid_models ->
        resp = Response.tool() |> Response.error("Invalid model: #{model}. Valid models: #{Enum.join(@valid_models, ", ")}")
        {:reply, resp, frame}

      true ->
        api_key = Application.get_env(:exhub, :giteeai_api_key, "")

        if api_key == "" do
          resp = Response.tool() |> Response.error("Gitee AI API key not configured. Run: mix scr.insert dev giteeai_api_key \"your-key\"")
          {:reply, resp, frame}
        else
          do_analyze(image, prompt, model, response_format, api_key, frame)
        end
    end
  end

  # --- Private helpers ---

  defp do_analyze(image, prompt, model, response_format, api_key, frame) do
    with {:ok, image_content} <- prepare_image_content(image) do
      messages = build_messages(prompt, image_content)
      body = build_request_body(model, messages, response_format)

      headers = [
        {"Content-Type", "application/json"},
        {"Authorization", "Bearer #{api_key}"}
      ]

      case HTTPoison.post(@api_url, Jason.encode!(body), headers, recv_timeout: 120_000, timeout: 120_000) do
        {:ok, %HTTPoison.Response{status_code: 200, body: resp_body}} ->
          handle_success(resp_body, frame)

        {:ok, %HTTPoison.Response{status_code: status, body: resp_body}} ->
          resp = Response.tool() |> Response.error("Gitee AI API error (HTTP #{status}): #{resp_body}")
          {:reply, resp, frame}

        {:error, %HTTPoison.Error{reason: reason}} ->
          resp = Response.tool() |> Response.error("HTTP request failed: #{inspect(reason)}")
          {:reply, resp, frame}
      end
    else
      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end

  defp prepare_image_content(image) do
    cond do
      String.starts_with?(image, ["http://", "https://"]) ->
        {:ok, %{type: "image_url", image_url: %{url: image}}}

      File.exists?(image) ->
        ext = Path.extname(image) |> String.downcase()

        if ext in @supported_image_exts do
          case File.read(image) do
            {:ok, binary} ->
              b64 = Base.encode64(binary)
              mime = ext_to_mime(ext)
              data_uri = "data:#{mime};base64,#{b64}"
              {:ok, %{type: "image_url", image_url: %{url: data_uri}}}

            {:error, reason} ->
              {:error, "Cannot read file #{image}: #{inspect(reason)}"}
          end
        else
          {:error, "Unsupported image format: #{ext}. Supported: #{Enum.join(@supported_image_exts, ", ")}"}
        end

      true ->
        {:error, "File not found: #{image}"}
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

  defp build_request_body(model, messages, "json") do
    %{
      model: model,
      messages: messages,
      max_tokens: @max_tokens,
      response_format: %{type: "json_object"}
    }
  end

  defp build_request_body(model, messages, _response_format) do
    %{
      model: model,
      messages: messages,
      max_tokens: @max_tokens
    }
  end

  defp handle_success(resp_body, frame) do
    case Jason.decode(resp_body) do
      {:ok, %{"choices" => [%{"message" => %{"content" => content}} | _]}} ->
        resp = Response.tool() |> Response.text(content)
        {:reply, resp, frame}

      {:ok, decoded} ->
        resp = Response.tool() |> Response.error("Unexpected API response: #{inspect(decoded)}")
        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to decode API response: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end
end
