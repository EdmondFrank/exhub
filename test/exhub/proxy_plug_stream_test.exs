defmodule Exhub.ProxyPlugStreamTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureLog

  alias Exhub.ProxyPlug

  @done_marker "<mask>DONE</mask>"
  @openai_done "data: [DONE]\n\n"
  @anthropic_stop "event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n"

  setup do
    Application.put_env(:exhub, :giteeai_early_done_markers, [@done_marker])

    on_exit(fn ->
      Application.delete_env(:exhub, :giteeai_early_done_markers)
    end)

    :ok
  end

  defp chunks_out(chunks, model \\ "test-model") do
    chunks
    |> Stream.map(&{:chunk, &1})
    |> ProxyPlug.wrap_stream(model, "openai", "")
    |> Enum.to_list()
    |> Enum.map(fn {:chunk, bin} -> bin end)
  end

  describe "truncate_to_frame_boundary/1" do
    test "keeps complete SSE frames only" do
      assert ProxyPlug.truncate_to_frame_boundary("data: a\n\ndata: b\n\n") ==
               "data: a\n\ndata: b\n\n"

      assert ProxyPlug.truncate_to_frame_boundary("data: a\n\ndata: b") == "data: a\n\n"
      assert ProxyPlug.truncate_to_frame_boundary("data: a") == ""
    end
  end

  describe "wrap_stream passthrough" do
    test "forwards chunks unchanged when no marker is present" do
      data = [
        "data: {\"choices\":[{\"delta\":{\"content\":\"hello\"}}]}\n\n",
        "data: {\"choices\":[{\"delta\":{\"content\":\" world\"}}]}\n\n",
        "data: [DONE]\n\n"
      ]

      assert chunks_out(data) == data
    end

    test "does not drop the tail when the stream ends naturally" do
      # Each chunk is shorter than the marker so the scanner holds back bytes;
      # flush_stream_tail must still deliver them at stream end.
      data = [
        "data: {\"choices\":[{\"delta\":{\"content\":\"hello\"}}]}\n\n",
        "data: {\"choices\":[{\"delta\":{\"content\":\" world\"}}]}\n\n"
      ]

      assert Enum.join(chunks_out(data)) == Enum.join(data)
    end

    test "passes through when markers are disabled" do
      Application.put_env(:exhub, :giteeai_early_done_markers, [])

      data = [
        "data: {\"choices\":[{\"delta\":{\"content\":\"#{@done_marker}\"}}]}\n\n",
        "data: [DONE]\n\n"
      ]

      assert chunks_out(data) == data
    end
  end

  describe "early termination on <mask>DONE</mask>" do
    test "drops the marker frame and emits the OpenAI [DONE] terminator" do
      prior = "data: {\"choices\":[{\"delta\":{\"content\":\"hello\"}}]}\n\n"
      marker_frame = "data: {\"choices\":[{\"delta\":{\"content\":\"#{@done_marker}\"}}]}\n\n"
      trailing = "data: {\"choices\":[{\"delta\":{\"content\":\"ignored\"}}]}\n\n"

      assert chunks_out([prior, marker_frame, trailing]) == [prior, @openai_done]
    end

    test "detects a marker split across upstream chunks" do
      part1 = "data: {\"choices\":[{\"delta\":{\"content\":\"done<mask>"
      part2 = "DONE</mask>\"}}]}\n\n"
      trailing = "data: {\"choices\":[{\"delta\":{\"content\":\"ignored\"}}]}\n\n"

      assert chunks_out([part1, part2, trailing]) == [@openai_done]
    end

    test "emits nothing before the marker when it appears in the first frame" do
      marker_frame = "data: {\"choices\":[{\"delta\":{\"content\":\"#{@done_marker}\"}}]}\n\n"

      assert chunks_out([marker_frame]) == [@openai_done]
    end

    test "uses the Anthropic message_stop terminator for event-style streams" do
      prior =
        "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"hi\"}}\n\n"

      marker_frame =
        "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"#{@done_marker}\"}}\n\n"

      assert chunks_out([prior, marker_frame]) == [prior, @anthropic_stop]
    end

    test "keeps the complete frames preceding the marker (framing preserved)" do
      prior = "data: {\"choices\":[{\"delta\":{\"reasoning_content\":\"thinking\"}}]}\n\n"

      marker_frame =
        "data: {\"choices\":[{\"delta\":{\"reasoning_content\":\"more#{@done_marker}\"}}]}\n\n"

      assert chunks_out([prior, marker_frame]) == [prior, @openai_done]
    end

    test "logs an info line when the early-done marker is detected" do
      prior = "data: {\"choices\":[{\"delta\":{\"content\":\"hello\"}}]}\n\n"
      marker_frame = "data: {\"choices\":[{\"delta\":{\"content\":\"#{@done_marker}\"}}]}\n\n"

      log =
        capture_log(fn ->
          assert chunks_out([prior, marker_frame]) == [prior, @openai_done]
        end)

      assert log =~ "Early-done marker"
      assert log =~ @done_marker
    end

    test "logs an info line when the stream ends without the marker" do
      data = ["data: {\"choices\":[{\"delta\":{\"content\":\"hello\"}}]}\n\n", "partial frame"]

      log =
        capture_log(fn ->
          assert Enum.join(chunks_out(data)) == Enum.join(data)
        end)

      assert log =~ "Early-done marker never fired"
    end
  end

  describe "inject_early_done_prompt/1" do
    test "prepends a system prompt for giteeai models when markers are enabled" do
      body = %{
        "model" => "deepseek-v3",
        "messages" => [%{"role" => "user", "content" => "hi"}]
      }

      result = ProxyPlug.inject_early_done_prompt(body)

      assert [%{"role" => "system", "content" => prompt}, %{"role" => "user"}] =
               result["messages"]

      assert prompt =~ @done_marker
    end

    test "merges into an existing leading system message" do
      body = %{
        "model" => "deepseek-v3",
        "messages" => [
          %{"role" => "system", "content" => "You are helpful"},
          %{"role" => "user", "content" => "hi"}
        ]
      }

      result = ProxyPlug.inject_early_done_prompt(body)

      assert [%{"role" => "system", "content" => content}, %{"role" => "user"}] =
               result["messages"]

      assert content =~ "You are helpful"
      assert content =~ @done_marker
    end

    test "leaves the body untouched when markers are disabled" do
      Application.put_env(:exhub, :giteeai_early_done_markers, [])

      body = %{
        "model" => "deepseek-v3",
        "messages" => [%{"role" => "user", "content" => "hi"}]
      }

      assert ProxyPlug.inject_early_done_prompt(body) == body
    end

    test "leaves non-giteeai models untouched" do
      Application.put_env(:exhub, :giteeai_early_done_markers, [@done_marker])

      for model <- ["minimax-m2.7", "unknown-model"] do
        body = %{"model" => model, "messages" => [%{"role" => "user", "content" => "hi"}]}
        assert ProxyPlug.inject_early_done_prompt(body) == body
      end
    end

    test "leaves bodies without messages untouched" do
      body = %{"model" => "deepseek-v3"}
      assert ProxyPlug.inject_early_done_prompt(body) == body
    end
  end
end
