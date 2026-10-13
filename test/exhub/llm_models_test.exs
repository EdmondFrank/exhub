defmodule Exhub.LLMModelsTest do
  use ExUnit.Case, async: true

  alias Exhub.LLMModels

  describe "default_llm_name/0" do
    test "defaults to the qwen3.8-flash no-think variant" do
      assert LLMModels.default_llm_name() == "openai/qwen3.8-flash--no-think"
    end

    test "the default's base model is a known GiteeAI model" do
      base =
        LLMModels.default_llm_name()
        |> String.split("/", parts: 2)
        |> List.last()
        |> Exhub.Router.ModelVariant.normalize()

      assert base == "qwen3.8-flash"
      assert base in LLMModels.giteeai_models()
      assert LLMModels.known_model?(base)
    end
  end

  describe "build_llms_config/1" do
    test "exposes qwen3.8-flash under the GiteeAI endpoint" do
      config = LLMModels.build_llms_config(%{giteeai_api_key: "test"})

      assert %{api_base: "https://api.moark.com/v1", model: "openai/qwen3.8-flash"} =
               config["openai/qwen3.8-flash"]
    end
  end
end
