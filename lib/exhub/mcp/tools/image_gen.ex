defmodule Exhub.MCP.Tools.ImageGen do
  @moduledoc """
  MCP Tool for generating images from text descriptions using Gitee AI.

  Supports the full set of text-to-image models available on Gitee AI Serverless API,
  sourced from https://ai.gitee.com/serverless-api (图像生成与处理 category).
  """

  alias Anubis.Server.Response

  use Anubis.Server.Component, type: :tool

  @api_url "https://ai.gitee.com/v1/images/generations"

  # Text-to-image generation models available on Gitee AI Serverless API
  # (excludes editing/upscaling/segmentation/background-removal models)
  @valid_models ~w(
    Qwen-Image
    Qwen-Image-2512
    Kolors
    GLM-Image
    FLUX.1-schnell
    FLUX.1-dev
    FLUX.1-Krea-dev
    FLUX.2-dev
    FLUX.2-klein-9B
    FLUX.2-klein-4B
    HunyuanDiT-v1.2
    stable-diffusion-xl-base-1.0
    stable-diffusion-3.5-large-turbo
    stable-diffusion-3-medium
    CogView4-6B
    HiDream-I1-Full
    z-image-turbo
    Z-Image
    LongCat-Image
  )

  @valid_sizes ~w(
    256x256
    512x512
    1024x1024
    1024x576
    576x1024
    1024x768
    768x1024
    1024x640
    640x1024
    2048x2048
  )

  # Which extra_body params each model supports
  @supported_params %{
    "Qwen-Image"                       => [:negative_prompt, :num_inference_steps],
    "Qwen-Image-2512"                  => [:negative_prompt, :num_inference_steps],
    "Kolors"                           => [:num_inference_steps, :guidance_scale],
    "GLM-Image"                        => [:negative_prompt, :num_inference_steps, :guidance_scale],
    "FLUX.1-schnell"                   => [:num_inference_steps, :guidance_scale],
    "FLUX.1-dev"                       => [:negative_prompt, :num_inference_steps, :guidance_scale],
    "FLUX.1-Krea-dev"                  => [:negative_prompt, :num_inference_steps, :guidance_scale],
    "FLUX.2-dev"                       => [:negative_prompt, :num_inference_steps, :guidance_scale],
    "FLUX.2-klein-9B"                  => [:num_inference_steps, :guidance_scale],
    "FLUX.2-klein-4B"                  => [:num_inference_steps, :guidance_scale],
    "HunyuanDiT-v1.2"                 => [:negative_prompt, :num_inference_steps, :guidance_scale],
    "stable-diffusion-xl-base-1.0"    => [:negative_prompt, :num_inference_steps, :guidance_scale],
    "stable-diffusion-3.5-large-turbo"=> [:negative_prompt, :num_inference_steps, :guidance_scale],
    "stable-diffusion-3-medium"        => [:negative_prompt, :num_inference_steps, :guidance_scale],
    "CogView4-6B"                      => [:negative_prompt, :num_inference_steps, :guidance_scale],
    "HiDream-I1-Full"                  => [:negative_prompt, :num_inference_steps, :guidance_scale],
    "z-image-turbo"                    => [:num_inference_steps, :guidance_scale],
    "Z-Image"                          => [:negative_prompt, :num_inference_steps, :guidance_scale],
    "LongCat-Image"                    => [:negative_prompt, :num_inference_steps, :guidance_scale]
  }

  @model_defaults %{
    "Qwen-Image"                        => %{num_inference_steps: 30},
    "Qwen-Image-2512"                   => %{num_inference_steps: 30},
    "Kolors"                            => %{num_inference_steps: 25, guidance_scale: 7.5},
    "GLM-Image"                         => %{num_inference_steps: 30, guidance_scale: 1.5},
    "FLUX.1-schnell"                    => %{num_inference_steps: 4,  guidance_scale: 0.0},
    "FLUX.1-dev"                        => %{num_inference_steps: 28, guidance_scale: 3.5},
    "FLUX.1-Krea-dev"                   => %{num_inference_steps: 28, guidance_scale: 3.5},
    "FLUX.2-dev"                        => %{num_inference_steps: 20, guidance_scale: 7.5},
    "FLUX.2-klein-9B"                   => %{num_inference_steps: 8,  guidance_scale: 3.5},
    "FLUX.2-klein-4B"                   => %{num_inference_steps: 8,  guidance_scale: 3.5},
    "HunyuanDiT-v1.2"                  => %{num_inference_steps: 25, guidance_scale: 5.0},
    "stable-diffusion-xl-base-1.0"     => %{num_inference_steps: 30, guidance_scale: 7.5},
    "stable-diffusion-3.5-large-turbo" => %{num_inference_steps: 8,  guidance_scale: 1.0},
    "stable-diffusion-3-medium"         => %{num_inference_steps: 28, guidance_scale: 7.0},
    "CogView4-6B"                       => %{num_inference_steps: 50, guidance_scale: 7.5},
    "HiDream-I1-Full"                   => %{num_inference_steps: 50, guidance_scale: 7.0},
    "z-image-turbo"                     => %{num_inference_steps: 8,  guidance_scale: 3.5},
    "Z-Image"                           => %{num_inference_steps: 28, guidance_scale: 5.0},
    "LongCat-Image"                     => %{num_inference_steps: 28, guidance_scale: 5.0}
  }

  @default_negative_prompt "低分辨率，低画质，肢体畸形，手指畸形，画面过饱和，蜡像感，人脸无细节，过度光滑，画面具有AI感。构图混乱。文字模糊，扭曲。"

  def name, do: "image_gen"

  @impl true
  def description do
    """
    Generate high-quality images from text descriptions using Gitee AI image generation API.

    Supported models (text-to-image):
    - `Qwen-Image` (default) — Alibaba 20B MMDiT, excellent text rendering in Chinese & English
    - `Qwen-Image-2512` — Latest Qwen-Image with improved realism and text rendering
    - `Kolors` — Kuaishou model, strong Chinese semantic understanding
    - `GLM-Image` — Zhipu AI multimodal image generation
    - `FLUX.1-schnell` — Black Forest Labs, ultra-fast (4 steps), Apache-2.0
    - `FLUX.1-dev` — Black Forest Labs, high quality, flexible fine-tuning
    - `FLUX.1-Krea-dev` — FLUX.1 variant optimized for image generation & editing
    - `FLUX.2-dev` — Next-gen BFL model, strong text understanding and detail
    - `FLUX.2-klein-9B` — BFL 9B efficient model, fast inference
    - `FLUX.2-klein-4B` — BFL 4B lightweight, very fast
    - `HunyuanDiT-v1.2` — Tencent distilled model, fast high-quality generation
    - `stable-diffusion-xl-base-1.0` — Stability AI SDXL, industry-leading creative generation
    - `stable-diffusion-3.5-large-turbo` — SD 3.5 Turbo, high detail, fast
    - `stable-diffusion-3-medium` — SD 3 Medium, improved prompt understanding
    - `CogView4-6B` — Tsinghua/Zhipu, bilingual (Chinese & English), multi-object scenes
    - `HiDream-I1-Full` — HiDream 17B open-source, industry-leading quality
    - `z-image-turbo` — Alibaba Z-Image Turbo, 8-step efficient generation (6B, Apache-2.0)
    - `Z-Image` — Alibaba Z-Image full model, strong controllability and style coverage
    - `LongCat-Image` — Meituan LongCat, bilingual, excellent Chinese text rendering

    Returns the generated image URL. Display it with markdown: `![Generated Image](URL)`

    **Quality tuning tips:**
    - If quality is low: increase `num_inference_steps` (e.g. 25 → 35)
    - If image ignores prompt details: increase `guidance_scale` (e.g. 7.5 → 15)
    - If image is oversaturated or distorted: decrease `guidance_scale`
    - For FLUX.1-schnell: keep steps at 1-4, guidance_scale at 0 (distilled model)
    """
  end

  schema do
    field(:prompt, {:required, :string},
      description: "Text description of the image to generate. Be specific and detailed."
    )

    field(:model, :string,
      description:
        "Model to use. One of: Qwen-Image (default), Qwen-Image-2512, Kolors, GLM-Image, FLUX.1-schnell, FLUX.1-dev, FLUX.1-Krea-dev, FLUX.2-dev, FLUX.2-klein-9B, FLUX.2-klein-4B, HunyuanDiT-v1.2, stable-diffusion-xl-base-1.0, stable-diffusion-3.5-large-turbo, stable-diffusion-3-medium, CogView4-6B, HiDream-I1-Full, z-image-turbo, Z-Image, LongCat-Image"
    )

    field(:size, :string,
      description:
        "Output image size. One of: 256x256, 512x512, 1024x1024 (default), 1024x576, 576x1024, 1024x768, 768x1024, 1024x640, 640x1024, 2048x2048"
    )

    field(:negative_prompt, :string,
      description:
        "Elements to avoid in the generated image. Not supported by Kolors. Defaults to a standard quality-improvement negative prompt."
    )

    field(:guidance_scale, :number,
      description:
        "How closely the model follows the prompt (float). Not supported by Qwen-Image, Qwen-Image-2512. Model defaults: Kolors=7.5, GLM-Image=1.5, FLUX.1-schnell=0.0, FLUX.1-dev=3.5, FLUX.2-dev=7.5, FLUX.2-klein=3.5, HunyuanDiT-v1.2=5.0, SD-XL=7.5, SD-3.5-turbo=1.0, SD-3-medium=7.0, CogView4=7.5, HiDream=7.0, z-image-turbo=3.5, Z-Image=5.0, LongCat=5.0"
    )

    field(:num_inference_steps, :integer,
      description:
        "Number of denoising steps (integer). Higher = better quality but slower. Model defaults: Qwen-Image=30, Kolors=25, GLM-Image=30, FLUX.1-schnell=4, FLUX.1-dev=28, FLUX.2-dev=20, FLUX.2-klein=8, HunyuanDiT-v1.2=25, SD-XL=30, SD-3.5-turbo=8, SD-3-medium=28, CogView4=50, HiDream=50, z-image-turbo=8, Z-Image=28, LongCat=28"
    )
  end

  @impl true
  def execute(params, frame) do
    prompt = Map.get(params, :prompt)
    model = Map.get(params, :model, "Qwen-Image") || "Qwen-Image"
    size = Map.get(params, :size, "1024x1024") || "1024x1024"

    cond do
      is_nil(prompt) or prompt == "" ->
        resp = Response.tool() |> Response.error("`prompt` is required")
        {:reply, resp, frame}

      model not in @valid_models ->
        resp =
          Response.tool()
          |> Response.error(
            "Invalid model: #{model}. Valid models: #{Enum.join(@valid_models, ", ")}"
          )

        {:reply, resp, frame}

      size not in @valid_sizes ->
        resp =
          Response.tool()
          |> Response.error(
            "Invalid size: #{size}. Valid sizes: #{Enum.join(@valid_sizes, ", ")}"
          )

        {:reply, resp, frame}

      true ->
        api_key = Application.get_env(:exhub, :giteeai_api_key, "")

        if api_key == "" do
          resp =
            Response.tool()
            |> Response.error(
              "Gitee AI API key not configured. Run: mix scr.insert dev giteeai_api_key \"your-key\""
            )

          {:reply, resp, frame}
        else
          do_generate(prompt, model, size, params, api_key, frame)
        end
    end
  end

  # ---------------------------------------------------------------------------
  # Private helpers
  # ---------------------------------------------------------------------------

  defp do_generate(prompt, model, size, params, api_key, frame) do
    extra_body = build_extra_body(model, params)

    body =
      Jason.encode!(%{
        "prompt" => prompt,
        "model" => model,
        "size" => size,
        "response_format" => "url",
        "extra_body" => extra_body
      })

    headers = [
      {"Content-Type", "application/json"},
      {"Authorization", "Bearer #{api_key}"}
    ]

    case HTTPoison.post(@api_url, body, headers, recv_timeout: 120_000, timeout: 120_000) do
      {:ok, %HTTPoison.Response{status_code: 200, body: resp_body}} ->
        handle_success(resp_body, model, size, prompt, extra_body, frame)

      {:ok, %HTTPoison.Response{status_code: status, body: resp_body}} ->
        resp =
          Response.tool()
          |> Response.error("Gitee AI API error (HTTP #{status}): #{resp_body}")

        {:reply, resp, frame}

      {:error, %HTTPoison.Error{reason: reason}} ->
        resp =
          Response.tool()
          |> Response.error("HTTP request failed: #{inspect(reason)}")

        {:reply, resp, frame}
    end
  end

  defp handle_success(resp_body, model, size, prompt, extra_body, frame) do
    case Jason.decode(resp_body) do
      {:ok, %{"data" => [first | _]}} when is_map(first) ->
        {type, value} =
          cond do
            is_binary(Map.get(first, "url")) and Map.get(first, "url") != "" ->
              {"url", Map.get(first, "url")}

            is_binary(Map.get(first, "b64_json")) ->
              {"b64_json", Map.get(first, "b64_json")}

            true ->
              {"unknown", nil}
          end

        if is_nil(value) do
          resp =
            Response.tool()
            |> Response.error("API returned no image data in response: #{inspect(first)}")

          {:reply, resp, frame}
        else
          result =
            Jason.encode!(%{
              "image_type" => type,
              "image_url" => if(type == "url", do: value, else: nil),
              "image_b64" => if(type == "b64_json", do: value, else: nil),
              "model" => model,
              "size" => size,
              "prompt" => prompt,
              "params" => extra_body
            })

          resp = Response.tool() |> Response.text(result)
          {:reply, resp, frame}
        end

      {:ok, decoded} ->
        resp =
          Response.tool()
          |> Response.error("Unexpected API response: #{inspect(decoded)}")

        {:reply, resp, frame}

      {:error, reason} ->
        resp =
          Response.tool()
          |> Response.error("Failed to decode API response: #{inspect(reason)}")

        {:reply, resp, frame}
    end
  end

  defp build_extra_body(model, params) do
    allowed = Map.get(@supported_params, model, [])
    defaults = Map.get(@model_defaults, model, %{})

    base = %{}

    base
    |> maybe_put_param(:negative_prompt, allowed, params, @default_negative_prompt)
    |> maybe_put_param(:guidance_scale, allowed, params, Map.get(defaults, :guidance_scale))
    |> maybe_put_param(
      :num_inference_steps,
      allowed,
      params,
      Map.get(defaults, :num_inference_steps)
    )
  end

  defp maybe_put_param(acc, _key, _allowed, _params, nil), do: acc

  defp maybe_put_param(acc, key, allowed, params, default) do
    if key in allowed do
      value = Map.get(params, key, default)

      if is_nil(value),
        do: acc,
        else: Map.put(acc, to_string(key), value)
    else
      acc
    end
  end
end
