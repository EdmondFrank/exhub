defmodule Exhub.MCP.Tools.VideoGenTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.VideoGen

  describe "validate_params/1" do
    test "applies defaults for a text-to-video request" do
      assert {:ok, validated} = VideoGen.validate_params(%{prompt: "a savanna at sunset"})

      assert validated.model == "MiniMax-H3"
      assert validated.task == "t2va"
      assert validated.duration_seconds == 6
      assert validated.num_steps == 20
      assert validated.aspect_ratio == "16:9"
      assert validated.first_frame == nil
      assert validated.last_frame == nil
      assert validated.seed == nil
    end

    test "requires a prompt" do
      assert {:error, message} = VideoGen.validate_params(%{})
      assert message =~ "`prompt` is required"

      assert {:error, message} = VideoGen.validate_params(%{prompt: "   "})
      assert message =~ "`prompt` is required"
    end

    test "blank model/task/aspect_ratio fall back to defaults" do
      assert {:ok, validated} =
               VideoGen.validate_params(%{
                 prompt: "x",
                 model: "  ",
                 task: "",
                 aspect_ratio: " "
               })

      assert validated.model == "MiniMax-H3"
      assert validated.task == "t2va"
      assert validated.aspect_ratio == "16:9"
    end

    test "rejects an unknown model" do
      assert {:error, message} = VideoGen.validate_params(%{prompt: "x", model: "MiniMax-H2"})
      assert message =~ "Invalid model: MiniMax-H2"
    end

    test "rejects an unknown task" do
      assert {:error, message} = VideoGen.validate_params(%{prompt: "x", task: "i2v"})
      assert message =~ "Invalid task: i2v"
    end

    test "requires first_frame for fl2va" do
      assert {:error, message} = VideoGen.validate_params(%{prompt: "x", task: "fl2va"})
      assert message =~ "`first_frame`"
    end

    test "accepts fl2va with first_frame and optional last_frame" do
      assert {:ok, validated} =
               VideoGen.validate_params(%{
                 prompt: "x",
                 task: "fl2va",
                 first_frame: "https://example.com/first.png"
               })

      assert validated.first_frame == "https://example.com/first.png"
      assert validated.last_frame == nil

      assert {:ok, validated} =
               VideoGen.validate_params(%{
                 prompt: "x",
                 task: "fl2va",
                 first_frame: "https://example.com/first.png",
                 last_frame: "https://example.com/last.png"
               })

      assert validated.last_frame == "https://example.com/last.png"
    end

    test "range-checks duration_seconds and num_steps" do
      assert {:error, message} = VideoGen.validate_params(%{prompt: "x", duration_seconds: 3})
      assert message =~ "Invalid duration_seconds: 3"

      assert {:error, message} = VideoGen.validate_params(%{prompt: "x", duration_seconds: 16})
      assert message =~ "Invalid duration_seconds: 16"

      assert {:error, message} = VideoGen.validate_params(%{prompt: "x", num_steps: 4})
      assert message =~ "Invalid num_steps: 4"

      assert {:error, message} = VideoGen.validate_params(%{prompt: "x", num_steps: 51})
      assert message =~ "Invalid num_steps: 51"

      assert {:ok, validated} =
               VideoGen.validate_params(%{prompt: "x", duration_seconds: 15, num_steps: 50})

      assert validated.duration_seconds == 15
      assert validated.num_steps == 50
    end

    test "rejects an invalid aspect_ratio" do
      assert {:error, message} = VideoGen.validate_params(%{prompt: "x", aspect_ratio: "21:9"})
      assert message =~ "Invalid aspect_ratio: 21:9"
    end

    test "rejects a non-integer seed" do
      assert {:error, message} = VideoGen.validate_params(%{prompt: "x", seed: "abc"})
      assert message =~ "Invalid seed"
    end
  end

  describe "build_body/1" do
    test "t2va body omits frame fields and seed by default" do
      {:ok, validated} = VideoGen.validate_params(%{prompt: "a savanna"})
      body = VideoGen.build_body(validated)

      assert body["model"] == "MiniMax-H3"
      assert body["task"] == "t2va"
      assert body["prompt"] == "a savanna"
      assert body["duration_seconds"] == 6
      assert body["num_steps"] == 20
      assert body["aspect_ratio"] == "16:9"
      refute Map.has_key?(body, "first_frame")
      refute Map.has_key?(body, "last_frame")
      refute Map.has_key?(body, "seed")
    end

    test "t2va body includes seed when set" do
      {:ok, validated} = VideoGen.validate_params(%{prompt: "x", seed: 42})
      body = VideoGen.build_body(validated)

      assert body["seed"] == 42
      refute Map.has_key?(body, "first_frame")
    end

    test "fl2va body includes first_frame and optional last_frame" do
      {:ok, validated} =
        VideoGen.validate_params(%{prompt: "x", task: "fl2va", first_frame: "https://f.png"})

      body = VideoGen.build_body(validated)

      assert body["task"] == "fl2va"
      assert body["first_frame"] == "https://f.png"
      refute Map.has_key?(body, "last_frame")

      {:ok, validated} =
        VideoGen.validate_params(%{
          prompt: "x",
          task: "fl2va",
          first_frame: "https://f.png",
          last_frame: "https://l.png"
        })

      body = VideoGen.build_body(validated)
      assert body["last_frame"] == "https://l.png"
    end
  end

  describe "interpret_poll/1" do
    test "returns ok on success" do
      result = %{"status" => "success", "output" => %{"file_url" => "https://v.mp4"}}
      assert VideoGen.interpret_poll(result) == {:ok, result}
    end

    test "returns pending for waiting and in_progress" do
      assert VideoGen.interpret_poll(%{"status" => "waiting"}) == :pending
      assert VideoGen.interpret_poll(%{"status" => "in_progress"}) == :pending
    end

    test "returns an error for terminal failure statuses" do
      for status <- ~w(failure failed cancelled) do
        assert {:error, message} = VideoGen.interpret_poll(%{"status" => status})
        assert message =~ "Task ended with status: #{status}"
      end
    end

    test "surfaces an error payload" do
      assert {:error, message} =
               VideoGen.interpret_poll(%{"error" => "InvalidRequest", "message" => "bad task"})

      assert message == "InvalidRequest: bad task"
    end

    test "appends the message to a terminal failure" do
      assert {:error, message} =
               VideoGen.interpret_poll(%{"status" => "failure", "message" => "oom"})

      assert message =~ "Task ended with status: failure (oom)"
    end
  end

  describe "resolve_frames/1" do
    test "passes URLs and data URIs through unchanged" do
      params = %{
        task: "fl2va",
        first_frame: "https://example.com/first.png",
        last_frame: "data:image/png;base64,AAAA"
      }

      assert {:ok, resolved} = VideoGen.resolve_frames(params)
      assert resolved.first_frame == "https://example.com/first.png"
      assert resolved.last_frame == "data:image/png;base64,AAAA"
    end

    test "encodes a local file path as a data URI" do
      path = write_tmp_png()

      assert {:ok, resolved} = VideoGen.resolve_frames(%{task: "fl2va", first_frame: path})
      assert resolved.first_frame == "data:image/png;base64," <> Base.encode64(png_bytes())
    end

    test "keeps other params and leaves absent frames untouched" do
      assert {:ok, resolved} = VideoGen.resolve_frames(%{prompt: "x", task: "t2va"})

      assert resolved.prompt == "x"
      refute Map.has_key?(resolved, :first_frame)
      refute Map.has_key?(resolved, :last_frame)
    end

    test "reports an unreadable frame" do
      assert {:error, message} = VideoGen.resolve_frames(%{first_frame: "images/a.png"})
      assert message =~ "Relative paths are not supported"
    end

    test "a resolved path flows through validate_params/1 and build_body/1" do
      path = write_tmp_png()

      {:ok, resolved} =
        VideoGen.resolve_frames(%{prompt: "x", task: "fl2va", first_frame: path})

      assert {:ok, validated} = VideoGen.validate_params(resolved)

      assert VideoGen.build_body(validated)["first_frame"] ==
               "data:image/png;base64," <> Base.encode64(png_bytes())
    end
  end

  # Minimal 1x1 transparent PNG.
  defp png_bytes do
    <<137, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, 73, 72, 68, 82, 0, 0, 0, 1, 0, 0, 0, 1, 8, 6,
      0, 0, 0, 31, 21, 196, 137, 0, 0, 0, 10, 73, 68, 65, 84, 120, 156, 99, 0, 1, 0, 0, 5, 0, 1,
      13, 10, 45, 180, 0, 0, 0, 0, 73, 69, 78, 68, 174, 66, 96, 130>>
  end

  defp write_tmp_png do
    path =
      Path.join(
        System.tmp_dir!(),
        "exhub_video_gen_test_#{System.unique_integer([:positive])}.png"
      )

    File.write!(path, png_bytes())
    path
  end
end
