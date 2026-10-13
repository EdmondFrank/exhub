defmodule Exhub.Router.ModelVariantTest do
  use ExUnit.Case, async: true

  alias Exhub.Router.ModelVariant

  doctest ModelVariant

  describe "split/1" do
    test "returns the name unchanged when there is no suffix" do
      assert ModelVariant.split("qwen3.6-flash") == {"qwen3.6-flash", []}
    end

    test "parses no-think into enable_thinking/preserve_thinking false" do
      assert ModelVariant.split("qwen3.6-flash--no-think") ==
               {"qwen3.6-flash", [enable_thinking: false, preserve_thinking: false]}
    end

    test "parses think into enable_thinking/preserve_thinking true" do
      assert ModelVariant.split("qwen3.6-flash--think") ==
               {"qwen3.6-flash", [enable_thinking: true, preserve_thinking: true]}
    end

    test "parses effort levels" do
      assert ModelVariant.split("qwen3.6-flash--effort-low") ==
               {"qwen3.6-flash", [reasoning_effort: "low"]}

      assert ModelVariant.split("qwen3.6-flash--effort-medium") ==
               {"qwen3.6-flash", [reasoning_effort: "medium"]}

      assert ModelVariant.split("qwen3.6-flash--effort-xhigh") ==
               {"qwen3.6-flash", [reasoning_effort: "xhigh"]}
    end

    test "parses kv modifiers, coercing true/false" do
      assert ModelVariant.split("qwen3.6-flash--kv-top_p=0.9") ==
               {"qwen3.6-flash", [kv: {"top_p", "0.9"}]}

      assert ModelVariant.split("qwen3.6-flash--kv-foo=true") ==
               {"qwen3.6-flash", [kv: {"foo", true}]}
    end

    test "joined modifiers keep file order and later ones win in apply_params" do
      assert ModelVariant.split("qwen3.6-flash--no-think+effort-medium") ==
               {"qwen3.6-flash",
                [enable_thinking: false, preserve_thinking: false, reasoning_effort: "medium"]}
    end

    test "ignores a variant on a non-Qwen base" do
      assert ModelVariant.split("kimi-k3--no-think") == {"kimi-k3--no-think", []}
    end

    test "ignores an unknown modifier" do
      assert ModelVariant.split("qwen3.6-flash--bogus") == {"qwen3.6-flash--bogus", []}

      assert ModelVariant.split("qwen3.6-flash--no-think+bogus") ==
               {"qwen3.6-flash--no-think+bogus", []}
    end

    test "ignores an empty suffix" do
      assert ModelVariant.split("qwen3.6-flash--") == {"qwen3.6-flash--", []}
    end
  end

  describe "normalize/1 and variant?/1" do
    test "strips a recognised suffix" do
      assert ModelVariant.normalize("qwen3.6-flash--no-think") == "qwen3.6-flash"
      assert ModelVariant.variant?("qwen3.6-flash--no-think")
    end

    test "leaves other names untouched" do
      assert ModelVariant.normalize("deepseek-v3") == "deepseek-v3"
      assert ModelVariant.normalize("kimi-k3--no-think") == "kimi-k3--no-think"
      refute ModelVariant.variant?("deepseek-v3")
    end
  end

  describe "qwen?/1" do
    test "detects the Qwen family after stripping the suffix" do
      assert ModelVariant.qwen?("qwen3.6-flash--no-think")
      assert ModelVariant.qwen?("Qwen3.6-Flash")
      refute ModelVariant.qwen?("deepseek-v3")
    end
  end

  describe "reasoning_effort/1" do
    test "returns the requested level or nil" do
      assert ModelVariant.reasoning_effort("qwen3.6-flash--effort-xhigh") == "xhigh"
      assert ModelVariant.reasoning_effort("qwen3.6-flash--no-think") == nil
      assert ModelVariant.reasoning_effort("qwen3.6-flash") == nil
    end
  end

  describe "apply_params/2" do
    test "adds chat_template_kwargs for no-think" do
      body = ModelVariant.apply_params(%{"model" => "x"}, "qwen3.6-flash--no-think")

      assert body["chat_template_kwargs"] == %{
               "enable_thinking" => false,
               "preserve_thinking" => false
             }
    end

    test "adds reasoning_effort alongside chat_template_kwargs" do
      body = ModelVariant.apply_params(%{"model" => "x"}, "qwen3.6-flash--no-think+effort-medium")

      assert body["reasoning_effort"] == "medium"
      assert body["chat_template_kwargs"]["enable_thinking"] == false
    end

    test "preserves and overrides existing chat_template_kwargs" do
      body =
        ModelVariant.apply_params(
          %{"chat_template_kwargs" => %{"enable_thinking" => true, "other" => 1}},
          "qwen3.6-flash--no-think"
        )

      assert body["chat_template_kwargs"] == %{
               "enable_thinking" => false,
               "preserve_thinking" => false,
               "other" => 1
             }
    end

    test "later modifiers override earlier ones for the same key" do
      body = ModelVariant.apply_params(%{"model" => "x"}, "qwen3.6-flash--no-think+keep-think")

      assert body["chat_template_kwargs"]["enable_thinking"] == false
      assert body["chat_template_kwargs"]["preserve_thinking"] == true
    end

    test "returns the body unchanged for non-variants and non-Qwen names" do
      body = %{"model" => "x"}

      assert ModelVariant.apply_params(body, "qwen3.6-flash") == body
      assert ModelVariant.apply_params(body, "deepseek-v3--no-think") == body
    end
  end
end
