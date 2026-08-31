defmodule Exhub.Llm.TranslateTest do
  use ExUnit.Case, async: true

  alias Exhub.Llm.Translate

  describe "resolve_llm_name/1" do
    setup do
      on_exit(fn -> Application.delete_env(:exhub, :translate_llm) end)
      :ok
    end

    test "returns nil when no custom translate model is configured" do
      Application.delete_env(:exhub, :translate_llm)
      assert Translate.resolve_llm_name([]) == nil
    end

    test "uses the configured :exhub, :translate_llm app env" do
      Application.put_env(:exhub, :translate_llm, "codestral/codestral-latest")
      assert Translate.resolve_llm_name([]) == "codestral/codestral-latest"
    end

    test "prefers the :llm option over the app env" do
      Application.put_env(:exhub, :translate_llm, "codestral/codestral-latest")

      assert Translate.resolve_llm_name(llm: "openai/deepseek-v4-flash") ==
               "openai/deepseek-v4-flash"
    end

    test "treats blank or non-binary values as unconfigured" do
      Application.put_env(:exhub, :translate_llm, "")
      assert Translate.resolve_llm_name([]) == nil

      Application.put_env(:exhub, :translate_llm, "  ")
      assert Translate.resolve_llm_name([]) == nil

      Application.put_env(:exhub, :translate_llm, 42)
      assert Translate.resolve_llm_name([]) == nil
    end
  end

  describe "fix_grammar_prompt/1" do
    test "asks for minimal grammar/spelling corrections keeping meaning, tone, and style" do
      prompt = Translate.fix_grammar_prompt("i wud like 2 go")

      assert prompt =~ "Fix only the grammar, spelling, and punctuation errors"
      assert prompt =~ "Keep the original meaning, tone, and style exactly the same"
      assert prompt =~ "Make minimal changes"
    end

    test "wraps the content in the Input text section" do
      prompt = Translate.fix_grammar_prompt("He go to school yesterday.")

      assert prompt =~ "Input text:"
      assert prompt =~ ~s["He go to school yesterday."]
      assert prompt =~ "Output:"
    end

    test "does not instruct to translate or change language" do
      prompt = Translate.fix_grammar_prompt("bonjour tout le monde")

      refute prompt =~ ~r/translate/i
      refute prompt =~ "to `EN`"
    end
  end
end
