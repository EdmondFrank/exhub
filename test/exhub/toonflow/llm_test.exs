defmodule Exhub.Toonflow.LLMTest do
  use ExUnit.Case, async: true

  alias Exhub.Toonflow.LLM

  describe "error_reason/1" do
    test "keeps a binary message verbatim" do
      assert LLM.error_reason("upstream 502") == {:llm_failed, "upstream 502"}
    end

    test "inspects a struct reason, so the payload stays JSON-safe" do
      err = struct(LangChain.LangChainError, type: "unknown_error", message: "LLM error: timeout")

      assert {:llm_failed, text} = LLM.error_reason(err)
      assert text == inspect(err)

      # The reason is a tuple, and `JSON.Encoder` implements no Tuple — so it is
      # only encodable once it has been through `Exhub.Toonflow.json_safe/1`
      # (which both the pipeline report and the progress fan-out apply).
      assert_raise Protocol.UndefinedError, fn -> JSON.encode!({:llm_failed, text}) end
      assert is_binary(JSON.encode!(Exhub.Toonflow.json_safe({:llm_failed, text})))
    end

    test "inspects any other non-binary term" do
      assert LLM.error_reason({:http, 500, "boom"}) == {:llm_failed, "{:http, 500, \"boom\"}"}
      assert LLM.error_reason(:timeout) == {:llm_failed, ":timeout"}
    end
  end
end
