defmodule Exhub.MCP.Tools.LookTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Look

  describe "normalize_mode/1" do
    test "defaults to analyze" do
      assert Look.normalize_mode(nil) == {:ok, "analyze"}
    end

    test "accepts analyze and text, case-insensitively" do
      assert Look.normalize_mode("analyze") == {:ok, "analyze"}
      assert Look.normalize_mode("TEXT") == {:ok, "text"}
      assert Look.normalize_mode(" text ") == {:ok, "text"}
    end

    test "maps transcription aliases onto text" do
      assert Look.normalize_mode("ocr") == {:ok, "text"}
      assert Look.normalize_mode("transcribe") == {:ok, "text"}
      assert Look.normalize_mode("Transcription") == {:ok, "text"}
    end

    test "rejects unknown modes" do
      assert {:error, message} = Look.normalize_mode("describe")
      assert message =~ "Invalid mode"
    end

    test "rejects non-string modes" do
      assert {:error, message} = Look.normalize_mode(42)
      assert message =~ "Invalid mode"
    end
  end

  describe "compose_prompt/2" do
    test "text mode without a prompt uses the transcription preset" do
      prompt = Look.compose_prompt("text", nil)
      assert prompt =~ "Transcribe every character of text"
      assert prompt =~ "Do not reformat as Markdown"
      refute prompt =~ "Additional instruction"
    end

    test "text mode without a prompt ignores blank strings" do
      assert Look.compose_prompt("text", "   ") == Look.compose_prompt("text", nil)
    end

    test "text mode appends extra instructions to the preset" do
      prompt = Look.compose_prompt("text", "Only the last 5 lines")
      assert prompt =~ "Transcribe every character of text"
      assert prompt =~ "Additional instruction"
      assert prompt =~ "Only the last 5 lines"
    end

    test "analyze mode passes the prompt through untouched" do
      assert Look.compose_prompt("analyze", "Describe the chart") == "Describe the chart"
    end
  end

  describe "build_request_body/4" do
    test "analyze mode keeps the legacy request shape" do
      body = Look.build_request_body("deepseek-v4.1-flash", [], "text", mode: "analyze")

      assert body.max_tokens == 4096
      refute Map.has_key?(body, :thinking)
      refute Map.has_key?(body, :temperature)
      refute Map.has_key?(body, :response_format)
    end

    test "text mode disables reasoning, pins temperature and raises the token budget" do
      body = Look.build_request_body("deepseek-v4.1-flash", [], "text", mode: "text")

      assert body.max_tokens == 8192
      assert body.thinking == %{type: "disabled"}
      assert body.temperature == 0
    end

    test "thinking: true re-enables reasoning in text mode" do
      body =
        Look.build_request_body("deepseek-v4.1-flash", [], "text", mode: "text", thinking: true)

      refute Map.has_key?(body, :thinking)
    end

    test "thinking: false disables reasoning in analyze mode" do
      body =
        Look.build_request_body("deepseek-v4.1-flash", [], "text",
          mode: "analyze",
          thinking: false
        )

      assert body.thinking == %{type: "disabled"}
    end

    test "json response format is preserved" do
      body = Look.build_request_body("deepseek-v4.1-flash", [], "json", mode: "analyze")

      assert body.response_format == %{type: "json_object"}
    end
  end

  describe "thinking_enabled?/2" do
    test "defaults by mode" do
      assert Look.thinking_enabled?("analyze", nil)
      refute Look.thinking_enabled?("text", nil)
    end

    test "explicit booleans win" do
      refute Look.thinking_enabled?("analyze", false)
      assert Look.thinking_enabled?("text", true)
    end

    test "non-boolean values fall back to the mode default" do
      assert Look.thinking_enabled?("analyze", "yes")
      refute Look.thinking_enabled?("text", "yes")
    end
  end

  describe "strip_code_fences/1" do
    test "unwraps a fully fenced block" do
      assert Look.strip_code_fences("```bash\nls -la\n```") == "ls -la"
      assert Look.strip_code_fences("```\nroot@host:~#\n```") == "root@host:~#"
    end

    test "keeps multi-line content intact" do
      assert Look.strip_code_fences("```text\nline 1\nline 2\n```") == "line 1\nline 2"
    end

    test "leaves unfenced text untouched" do
      assert Look.strip_code_fences("root@host:~# ls") == "root@host:~# ls"
    end

    test "does not strip a fence that is not the whole response" do
      input = "output:\n```\nls\n```\nthat is all"
      assert Look.strip_code_fences(input) == input
    end
  end

  describe "compose_prompt/3" do
    test "max_lines appends a last-N instruction" do
      prompt = Look.compose_prompt("text", nil, %{max_lines: 30})
      assert prompt =~ "at most the last 30 visible lines"
    end

    test "only_new embeds the known transcript" do
      prompt = Look.compose_prompt("text", nil, %{only_new: "root@host:~# ls"})
      assert prompt =~ "BEGIN KNOWN TEXT"
      assert prompt =~ "root@host:~# ls"
    end

    test "blank only_new and non-positive max_lines are ignored" do
      base = Look.compose_prompt("text", nil)
      assert Look.compose_prompt("text", nil, %{only_new: "  ", max_lines: 0}) == base
    end

    test "analyze mode ignores the screen-reading options" do
      assert Look.compose_prompt("analyze", "Describe", %{max_lines: 10}) == "Describe"
    end
  end

  describe "postprocess/2" do
    test "keeps only the last N lines" do
      assert Look.postprocess("a\nb\nc\nd", %{max_lines: 2}) == "c\nd"
    end

    test "drops lines already present in only_new" do
      known = "root@host:~# ls\nfile.txt"
      text = "root@host:~# ls\nfile.txt\nnew.txt"
      assert Look.postprocess(text, %{only_new: known}) == "new.txt"
    end

    test "appends an expect verdict" do
      assert Look.postprocess("all good", %{expect: "all\ngood"}) == "all good\n[expect: ok 2]"
      assert Look.postprocess("all good", %{expect: "all\nnope"}) =~ "[expect: MISSING nope]"
    end

    test "is a no-op without options" do
      assert Look.postprocess("x\ny", %{}) == "x\ny"
    end
  end
end
