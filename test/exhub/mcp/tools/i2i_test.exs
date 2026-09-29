defmodule Exhub.MCP.Tools.I2ITest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.I2I

  describe "validate_params/1" do
    test "applies defaults" do
      assert {:ok, validated} =
               I2I.validate_params(%{image_path: "/tmp/a.png", prompt: "restyle"})

      assert validated.model == "qwen-image-2.0"
      assert validated.size == "1024x1024"
      assert validated.reference_image_paths == []
      assert validated.negative_prompt == nil
      assert validated.guidance_scale == nil
      assert validated.num_inference_steps == nil
      assert validated.seed == nil
    end

    test "requires image_path" do
      assert {:error, message} = I2I.validate_params(%{prompt: "x"})
      assert message =~ "`image_path` is required"
    end

    test "requires prompt" do
      assert {:error, message} = I2I.validate_params(%{image_path: "/tmp/a.png"})
      assert message =~ "`prompt` is required"

      assert {:error, message} = I2I.validate_params(%{image_path: "/tmp/a.png", prompt: "   "})
      assert message =~ "`prompt` is required"
    end

    test "rejects an unknown model" do
      assert {:error, message} =
               I2I.validate_params(%{image_path: "/tmp/a.png", prompt: "x", model: "Nope"})

      assert message =~ "Invalid model: Nope"
    end

    test "blank model and size fall back to defaults" do
      assert {:ok, validated} =
               I2I.validate_params(%{image_path: "/tmp/a.png", prompt: "x", model: " ", size: ""})

      assert validated.model == "qwen-image-2.0"
      assert validated.size == "1024x1024"
    end

    test "trims image_path and filters reference list" do
      assert {:ok, validated} =
               I2I.validate_params(%{
                 image_path: "  /tmp/a.png  ",
                 prompt: "x",
                 reference_image_paths: ["/tmp/b.png", "", "  ", "/tmp/a.png", "/tmp/c.png"]
               })

      assert validated.image_path == "/tmp/a.png"
      assert validated.reference_image_paths == ["/tmp/b.png", "/tmp/c.png"]
    end

    test "ignores a non-list reference_image_paths" do
      assert {:ok, validated} =
               I2I.validate_params(%{
                 image_path: "/tmp/a.png",
                 prompt: "x",
                 reference_image_paths: "all"
               })

      assert validated.reference_image_paths == []
    end
  end

  describe "resolve_sources/1" do
    test "passes URLs and data URIs through unchanged" do
      {:ok, validated} =
        I2I.validate_params(%{
          image_path: "https://example.com/a.png",
          prompt: "x",
          reference_image_paths: ["data:image/png;base64,AAAA"]
        })

      assert {:ok, [url, data]} = I2I.resolve_sources(validated)
      assert url == "https://example.com/a.png"
      assert data == "data:image/png;base64,AAAA"
    end

    test "encodes a local file as a data URI" do
      path = tmp_png()
      {:ok, validated} = I2I.validate_params(%{image_path: path, prompt: "x"})

      assert {:ok, [data]} = I2I.resolve_sources(validated)
      assert String.starts_with?(data, "data:image/png;base64,")
    end

    test "returns an error when a source cannot be resolved" do
      {:ok, validated} = I2I.validate_params(%{image_path: "relative.png", prompt: "x"})

      assert {:error, message} = I2I.resolve_sources(validated)
      assert message =~ "Failed to resolve image source"
    end
  end

  describe "build_generations_body/2" do
    test "includes prompt, model, normalized size, images and extra_body defaults" do
      {:ok, validated} = I2I.validate_params(%{image_path: "/tmp/a.png", prompt: "restyle"})
      body = I2I.build_generations_body(validated, ["data:image/png;base64,AAAA"])

      assert body["prompt"] == "restyle"
      assert body["model"] == "qwen-image-2.0"
      assert body["size"] == "1024*1024"
      assert body["response_format"] == "url"
      assert body["images"] == ["data:image/png;base64,AAAA"]
      assert body["extra_body"]["num_inference_steps"] == 30
      assert is_binary(body["extra_body"]["negative_prompt"])
      refute Map.has_key?(body, "seed")
    end

    test "omits size for qwen-image-2.0-pro (the generations endpoint rejects it)" do
      {:ok, validated} =
        I2I.validate_params(%{
          image_path: "/tmp/a.png",
          prompt: "restyle",
          model: "qwen-image-2.0-pro"
        })

      body = I2I.build_generations_body(validated, ["u"])
      refute Map.has_key?(body, "size")
    end

    test "adds seed only when provided" do
      {:ok, validated} =
        I2I.validate_params(%{image_path: "/tmp/a.png", prompt: "x", seed: 42})

      assert I2I.build_generations_body(validated, ["u"])["seed"] == 42
    end

    test "honours an explicit negative_prompt" do
      {:ok, validated} =
        I2I.validate_params(%{image_path: "/tmp/a.png", prompt: "x", negative_prompt: "blurry"})

      body = I2I.build_generations_body(validated, ["u"])
      assert body["extra_body"]["negative_prompt"] == "blurry"
    end
  end

  describe "build_edit_fields/2" do
    test "includes the primary image as a multipart file part" do
      {:ok, validated} =
        I2I.validate_params(%{
          image_path: "/tmp/a.png",
          prompt: "x",
          model: "FLUX.1-Kontext-dev"
        })

      fields = I2I.build_edit_fields(validated, {"a.png", <<1, 2, 3>>, "image/png"})

      assert {"model", "FLUX.1-Kontext-dev"} in fields
      assert {"prompt", "x"} in fields
      assert {"response_format", "url"} in fields
      assert {"image", {"a.png", <<1, 2, 3>>, "image/png"}} in fields
      refute Enum.any?(fields, fn {key, _value} -> key == "seed" end)
    end

    test "adds seed as a string field when provided" do
      {:ok, validated} =
        I2I.validate_params(%{
          image_path: "/tmp/a.png",
          prompt: "x",
          model: "FLUX.1-Kontext-dev",
          seed: 7
        })

      fields = I2I.build_edit_fields(validated, {"a.png", <<1>>, "image/png"})
      assert {"seed", "7"} in fields
    end
  end

  describe "data_uri_file_part/1" do
    test "parses a base64 data URI" do
      data_uri = "data:image/png;base64," <> Base.encode64(<<137, 80, 78, 71>>)

      assert {:ok, {"image.png", bytes, "image/png"}} = I2I.data_uri_file_part(data_uri)
      assert bytes == <<137, 80, 78, 71>>
    end

    test "rejects a non data URI" do
      assert {:error, _message} = I2I.data_uri_file_part("https://example.com/a.png")
    end
  end

  defp tmp_png do
    path = Path.join(System.tmp_dir!(), "i2i_test_#{System.unique_integer([:positive])}.png")
    File.write!(path, <<137, 80, 78, 71, 13, 10, 26, 10>>)
    on_exit(fn -> File.rm(path) end)
    path
  end
end
