defmodule Exhub.MCP.Tools.ImageGen do
  @moduledoc """
  MCP Tool for generating images from text descriptions using Gitee AI.

  Supports the full set of text-to-image models available on Gitee AI Serverless API,
  sourced from https://moark.com/serverless-api (图像生成与处理 category).
  """

  alias Anubis.Server.Response

  use Anubis.Server.Component, type: :tool

  @api_url "https://api.moark.com/v1/images/generations"

  # Text-to-image generation models available on Gitee AI Serverless API
  # (excludes editing/upscaling/segmentation/background-removal models)
  @valid_models ~w(
    gpt-image-2
    qwen-image-2.0
    qwen-image-2.0-pro
    Qwen-Image
    Qwen-Image-2512
    Qwen-Image-Layered
    wan2.7-image
    wan2.7-image-pro
    Kolors
    GLM-Image
    flux-1-schnell
    FLUX.1-dev
    FLUX_1-Krea-dev
    FLUX.1-Kontext-dev
    FLUX.2-dev
    FLUX.2-klein-9B
    FLUX.2-klein-4B
    stable-diffusion-xl-base-1.0
    stable-diffusion-3.5-large-turbo
    stable-diffusion-3-medium
    CogView4_6B
    HiDream-I1-Full
    z-image-turbo
    Z-Image
    LongCat-Image
  )

  # WAN models support extra params (n, seed, prompt_extend, 1K/2K/4K sizes)
  @wan_models ~w(wan2.7-image wan2.7-image-pro)

  # Sizes accepted by WAN models (in addition to the standard pixel sizes)
  @wan_sizes ~w(1K 2K 4K)

  # Quality levels accepted by gpt-image-2
  @gpt_image_qualities ~w(low medium high auto)

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
    "qwen-image-2.0" => [:negative_prompt, :num_inference_steps],
    "qwen-image-2.0-pro" => [:negative_prompt, :num_inference_steps],
    "Qwen-Image" => [:negative_prompt, :num_inference_steps],
    "Qwen-Image-2512" => [:negative_prompt, :num_inference_steps],
    "Qwen-Image-Layered" => [:negative_prompt, :num_inference_steps],
    "wan2.7-image" => [:negative_prompt, :num_inference_steps, :guidance_scale],
    "wan2.7-image-pro" => [:negative_prompt, :num_inference_steps, :guidance_scale],
    "Kolors" => [:num_inference_steps, :guidance_scale],
    "GLM-Image" => [:negative_prompt, :num_inference_steps, :guidance_scale],
    "flux-1-schnell" => [:num_inference_steps, :guidance_scale],
    "FLUX.1-dev" => [:negative_prompt, :num_inference_steps, :guidance_scale],
    "FLUX_1-Krea-dev" => [:negative_prompt, :num_inference_steps, :guidance_scale],
    "FLUX.1-Kontext-dev" => [:negative_prompt, :num_inference_steps, :guidance_scale],
    "FLUX.2-dev" => [:negative_prompt, :num_inference_steps, :guidance_scale],
    "FLUX.2-klein-9B" => [:num_inference_steps, :guidance_scale],
    "FLUX.2-klein-4B" => [:num_inference_steps, :guidance_scale],
    "stable-diffusion-xl-base-1.0" => [:negative_prompt, :num_inference_steps, :guidance_scale],
    "stable-diffusion-3.5-large-turbo" => [
      :negative_prompt,
      :num_inference_steps,
      :guidance_scale
    ],
    "stable-diffusion-3-medium" => [:negative_prompt, :num_inference_steps, :guidance_scale],
    "CogView4_6B" => [:negative_prompt, :num_inference_steps, :guidance_scale],
    "HiDream-I1-Full" => [:negative_prompt, :num_inference_steps, :guidance_scale],
    "z-image-turbo" => [:num_inference_steps, :guidance_scale],
    "Z-Image" => [:negative_prompt, :num_inference_steps, :guidance_scale],
    "LongCat-Image" => [:negative_prompt, :num_inference_steps, :guidance_scale]
  }

  @model_defaults %{
    "qwen-image-2.0" => %{num_inference_steps: 30},
    "qwen-image-2.0-pro" => %{num_inference_steps: 30},
    "Qwen-Image" => %{num_inference_steps: 30},
    "Qwen-Image-2512" => %{num_inference_steps: 30},
    "Qwen-Image-Layered" => %{num_inference_steps: 30},
    "wan2.7-image" => %{num_inference_steps: 30, guidance_scale: 7.5},
    "wan2.7-image-pro" => %{num_inference_steps: 30, guidance_scale: 7.5},
    "Kolors" => %{num_inference_steps: 25, guidance_scale: 7.5},
    "GLM-Image" => %{num_inference_steps: 30, guidance_scale: 1.5},
    "flux-1-schnell" => %{num_inference_steps: 4, guidance_scale: 0.0},
    "FLUX.1-dev" => %{num_inference_steps: 28, guidance_scale: 3.5},
    "FLUX_1-Krea-dev" => %{num_inference_steps: 28, guidance_scale: 3.5},
    "FLUX.1-Kontext-dev" => %{num_inference_steps: 28, guidance_scale: 3.5},
    "FLUX.2-dev" => %{num_inference_steps: 20, guidance_scale: 7.5},
    "FLUX.2-klein-9B" => %{num_inference_steps: 8, guidance_scale: 3.5},
    "FLUX.2-klein-4B" => %{num_inference_steps: 8, guidance_scale: 3.5},
    "stable-diffusion-xl-base-1.0" => %{num_inference_steps: 30, guidance_scale: 7.5},
    "stable-diffusion-3.5-large-turbo" => %{num_inference_steps: 8, guidance_scale: 1.0},
    "stable-diffusion-3-medium" => %{num_inference_steps: 28, guidance_scale: 7.0},
    "CogView4_6B" => %{num_inference_steps: 50, guidance_scale: 7.5},
    "HiDream-I1-Full" => %{num_inference_steps: 50, guidance_scale: 7.0},
    "z-image-turbo" => %{num_inference_steps: 8, guidance_scale: 3.5},
    "Z-Image" => %{num_inference_steps: 28, guidance_scale: 5.0},
    "LongCat-Image" => %{num_inference_steps: 28, guidance_scale: 5.0}
  }

  @default_negative_prompt "低分辨率，低画质，肢体畸形，手指畸形，画面过饱和，蜡像感，人脸无细节，过度光滑，画面具有AI感。构图混乱。文字模糊，扭曲。"

  def name, do: "image_gen"

  @impl true
  def description do
    """
    Generate high-quality images from text descriptions using Gitee AI image generation API.

    Supported models (text-to-image):
    - `gpt-image-2` — OpenAI's latest image model (via Moark), supports `quality` (low/medium/high/auto) and `n` (1-10)
    - `qwen-image-2.0` (default) — Alibaba latest Qwen-Image 2.0, superior quality and text rendering
    - `qwen-image-2.0-pro` — Qwen-Image 2.0 Pro, enhanced quality and detail
    - `Qwen-Image` — Alibaba 20B MMDiT, excellent text rendering in Chinese & English
    - `Qwen-Image-2512` — Latest Qwen-Image with improved realism and text rendering
    - `Qwen-Image-Layered` — Qwen-Image with layered generation for compositing
    - `wan2.7-image` — Alibaba Wan 2.7 image generation model
    - `wan2.7-image-pro` — Alibaba Wan 2.7 image Pro, enhanced quality
    - `Kolors` — Kuaishou model, strong Chinese semantic understanding
    - `GLM-Image` — Zhipu AI multimodal image generation
    - `flux-1-schnell` — Black Forest Labs, ultra-fast (4 steps), Apache-2.0
    - `FLUX.1-dev` — Black Forest Labs, high quality, flexible fine-tuning
    - `FLUX_1-Krea-dev` — FLUX.1 variant optimized for image generation & editing
    - `FLUX.1-Kontext-dev` — FLUX.1 variant with contextual understanding
    - `FLUX.2-dev` — Next-gen BFL model, strong text understanding and detail
    - `FLUX.2-klein-9B` — BFL 9B efficient model, fast inference
    - `FLUX.2-klein-4B` — BFL 4B lightweight, very fast
    - `stable-diffusion-xl-base-1.0` — Stability AI SDXL, industry-leading creative generation
    - `stable-diffusion-3.5-large-turbo` — SD 3.5 Turbo, high detail, fast
    - `stable-diffusion-3-medium` — SD 3 Medium, improved prompt understanding
    - `CogView4_6B` — Tsinghua/Zhipu, bilingual (Chinese & English), multi-object scenes
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

    **WAN model params** (`wan2.7-image`, `wan2.7-image-pro`):
    - `n`: number of images to generate (1-4, default: 1)
    - `seed`: integer seed for reproducible results (default: 0)
    - `prompt_extend`: auto-enhance the prompt (boolean, default: true)
    - `size`: supports `1K`, `2K`, `4K` aliases; pixel sizes (e.g. `1024x1024`) are auto-converted to the nearest alias

    **gpt-image-2 params**:
    - `quality`: one of `low`, `medium` (default), `high`, `auto`
    - `n`: number of images to generate (default: 1)
    - `size` is not supported and is ignored
    """
  end

  schema do
    field(:prompt, {:required, :string},
      description: "Text description of the image to generate. Be specific and detailed."
    )

    field(:model, :string,
      description:
        "Model to use. One of: gpt-image-2, qwen-image-2.0 (default), qwen-image-2.0-pro, Qwen-Image, Qwen-Image-2512, Qwen-Image-Layered, wan2.7-image, wan2.7-image-pro, Kolors, GLM-Image, flux-1-schnell, FLUX.1-dev, FLUX_1-Krea-dev, FLUX.1-Kontext-dev, FLUX.2-dev, FLUX.2-klein-9B, FLUX.2-klein-4B, stable-diffusion-xl-base-1.0, stable-diffusion-3.5-large-turbo, stable-diffusion-3-medium, CogView4_6B, HiDream-I1-Full, z-image-turbo, Z-Image, LongCat-Image"
    )

    field(:size, :string,
      description:
        "Output image size. One of: 256x256, 512x512, 1024x1024 (default), 1024x576, 576x1024, 1024x768, 768x1024, 1024x640, 640x1024, 2048x2048. Ignored by gpt-image-2 (the model picks the output size)."
    )

    field(:quality, :string,
      description:
        "Output quality for gpt-image-2. One of: low, medium (default), high, auto. Ignored by other models."
    )

    field(:negative_prompt, :string,
      description:
        "Elements to avoid in the generated image. Not supported by Kolors. Defaults to a standard quality-improvement negative prompt."
    )

    field(:guidance_scale, :number,
      description:
        "How closely the model follows the prompt (float). Not supported by qwen-image-2.0, qwen-image-2.0-pro, Qwen-Image, Qwen-Image-2512, Qwen-Image-Layered. Model defaults: wan2.7=7.5, Kolors=7.5, GLM-Image=1.5, flux-1-schnell=0.0, FLUX.1-dev=3.5, FLUX.2-dev=7.5, FLUX.2-klein=3.5, SD-XL=7.5, SD-3.5-turbo=1.0, SD-3-medium=7.0, CogView4=7.5, HiDream=7.0, z-image-turbo=3.5, Z-Image=5.0, LongCat=5.0"
    )

    field(:num_inference_steps, :integer,
      description:
        "Number of denoising steps (integer). Higher = better quality but slower. Model defaults: qwen-image-2.0=30, qwen-image-2.0-pro=30, Qwen-Image=30, Qwen-Image-Layered=30, wan2.7=30, Kolors=25, GLM-Image=30, flux-1-schnell=4, FLUX.1-dev=28, FLUX.2-dev=20, FLUX.2-klein=8, SD-XL=30, SD-3.5-turbo=8, SD-3-medium=28, CogView4=50, HiDream=50, z-image-turbo=8, Z-Image=28, LongCat=28"
    )

    field(:n, :integer,
      description:
        "Number of images to generate (integer, 1-4). Default: 1. Only supported by some models."
    )

    field(:seed, :integer,
      description:
        "Random seed for reproducible generation (integer). WAN models default to 0. Supported by wan2.7-image, wan2.7-image-pro, and other models."
    )

    field(:prompt_extend, :boolean,
      description:
        "Whether to auto-enhance the prompt using AI (boolean). WAN models default to true. Supported by wan2.7-image, wan2.7-image-pro, and other models."
    )
  end

  @impl true
  def execute(params, frame) do
    prompt = Map.get(params, :prompt)
    model = Map.get(params, :model, "qwen-image-2.0") || "qwen-image-2.0"

    size =
      if model in @wan_models do
        Map.get(params, :size, "2K") || "2K"
      else
        Map.get(params, :size, "1024x1024") || "1024x1024"
      end

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

      size not in @valid_sizes and
        model != "gpt-image-2" and
          not (model in @wan_models and size in @wan_sizes) ->
        resp =
          Response.tool()
          |> Response.error(
            "Invalid size: #{size}. Valid sizes: #{Enum.join(@valid_sizes, ", ")}#{if(model in @wan_models, do: " (WAN models also support: #{Enum.join(@wan_sizes, ", ")})", else: "")}"
          )

        {:reply, resp, frame}

      model == "gpt-image-2" and
        not is_nil(Map.get(params, :quality)) and
          Map.get(params, :quality) not in @gpt_image_qualities ->
        resp =
          Response.tool()
          |> Response.error(
            "Invalid quality: #{Map.get(params, :quality)}. Valid qualities: #{Enum.join(@gpt_image_qualities, ", ")}"
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

  # gpt-image-2 uses OpenAI-style top-level params and does not support
  # size / extra_body / negative_prompt / guidance_scale / num_inference_steps.
  # response_format is intentionally omitted so the provider returns either a
  # url or b64_json (both are handled by handle_success/6).
  defp do_generate(prompt, "gpt-image-2", _size, params, api_key, frame) do
    body_map =
      %{
        "prompt" => prompt,
        "model" => "gpt-image-2"
      }
      |> then(fn bm ->
        n = Map.get(params, :n, 1)

        if is_integer(n) and n >= 1,
          do: Map.put(bm, "n", n),
          else: bm
      end)
      |> then(fn bm ->
        quality = Map.get(params, :quality, "medium")

        if is_binary(quality) and quality != "",
          do: Map.put(bm, "quality", quality),
          else: bm
      end)

    body = Jason.encode!(body_map)

    headers = [
      {"Content-Type", "application/json"},
      {"Authorization", "Bearer #{api_key}"}
    ]

    case HTTPoison.post(@api_url, body, headers, recv_timeout: 120_000, timeout: 120_000) do
      {:ok, %HTTPoison.Response{status_code: 200, body: resp_body}} ->
        handle_success(resp_body, "gpt-image-2", nil, prompt, %{}, frame)

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

  defp do_generate(prompt, model, size, params, api_key, frame) do
    extra_body = build_extra_body(model, params)
    size = normalize_size(size, model)

    body_map = %{
      "prompt" => prompt,
      "model" => model,
      "size" => size,
      "response_format" => "url",
      "extra_body" => extra_body
    }

    # WAN models support additional top-level params
    body_map =
      if model in @wan_models do
        n = Map.get(params, :n, 1)
        seed = Map.get(params, :seed, 0)
        prompt_extend = Map.get(params, :prompt_extend, true)
        negative_prompt = Map.get(params, :negative_prompt, @default_negative_prompt)

        body_map
        |> Map.put("n", n)
        |> then(fn bm ->
          if is_integer(seed), do: Map.put(bm, "seed", seed), else: bm
        end)
        |> then(fn bm ->
          if prompt_extend, do: Map.put(bm, "prompt_extend", true), else: bm
        end)
        |> then(fn bm ->
          if is_binary(negative_prompt) and negative_prompt != "",
            do: Map.put(bm, "negative_prompt", negative_prompt),
            else: bm
        end)
      else
        body_map
      end

    body = Jason.encode!(body_map)

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

    # WAN models send negative_prompt at the top level; keep it out of extra_body
    allowed = if model in @wan_models, do: allowed -- [:negative_prompt], else: allowed

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

  # qwen-image models use "*" as size separator (e.g. "1024*1024")
  defp normalize_size(size, "qwen-image-2.0"), do: String.replace(size, "x", "*")
  defp normalize_size(size, "qwen-image-2.0-pro"), do: String.replace(size, "x", "*")

  # WAN models accept 1K/2K/4K aliases; convert pixel sizes to the nearest alias
  defp normalize_size(size, model) when model in @wan_models do
    if size in @wan_sizes do
      size
    else
      pixel_size_to_wan(size)
    end
  end

  defp normalize_size(size, _model), do: size

  # Map a "WxH" pixel size to the nearest WAN K alias based on the max dimension
  defp pixel_size_to_wan(size) do
    max_dim =
      size
      |> String.split(["x", "*"])
      |> Enum.map(&String.to_integer/1)
      |> Enum.max()

    cond do
      max_dim <= 1024 -> "1K"
      max_dim <= 2048 -> "2K"
      true -> "4K"
    end
  end
end
