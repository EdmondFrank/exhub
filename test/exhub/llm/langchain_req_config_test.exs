defmodule Exhub.Llm.LangChainReqConfigTest do
  @moduledoc """
  LangChain chat models dial providers through Req/Finch/Mint, so the TLS compat
  verify_fun has to travel with them as `req_config`. Without it, TrustAsia-style
  chains (`api.moark.com`) abort the handshake with `key_usage_mismatch`.
  """
  use ExUnit.Case, async: true

  alias Exhub.Genclaw.LLMHelper
  alias Exhub.Sagents.Factory

  defp llm_config(provider) do
    %{
      model: "#{provider}/some-model",
      api_key: "key",
      api_base: "https://api.moark.com/v1"
    }
  end

  defp verify_fun(model) do
    with %{connect_options: connect_options} <- Map.get(model, :req_config),
         transport_opts when is_list(transport_opts) <- connect_options[:transport_opts],
         {fun, state} when is_function(fun, 3) and state == nil <- transport_opts[:verify_fun] do
      fun
    else
      _ -> nil
    end
  end

  describe "Genclaw.LLMHelper.build_langchain_model/2" do
    test "openai-compatible models carry the verify_fun" do
      model = LLMHelper.build_langchain_model(llm_config("openai"))

      assert model.__struct__ == LangChain.ChatModels.ChatOpenAI
      assert is_function(verify_fun(model), 3)
    end

    test "google models carry the verify_fun" do
      model = LLMHelper.build_langchain_model(llm_config("google"))

      assert model.__struct__ == LangChain.ChatModels.ChatGoogleAI
      assert is_function(verify_fun(model), 3)
    end

    test "anthropic structs have no req_config field to set" do
      model = LLMHelper.build_langchain_model(llm_config("anthropic"))

      assert model.__struct__ == LangChain.ChatModels.ChatAnthropic
      refute Map.has_key?(model, :req_config)
    end
  end

  describe "Sagents.Factory.create_langchain_model/1" do
    test "openai-compatible models carry the verify_fun" do
      model = Factory.create_langchain_model(llm_config("openai"))

      assert model.__struct__ == LangChain.ChatModels.ChatOpenAI
      assert is_function(verify_fun(model), 3)
    end

    test "google models carry the verify_fun" do
      model = Factory.create_langchain_model(llm_config("google"))

      assert model.__struct__ == LangChain.ChatModels.ChatGoogleAI
      assert is_function(verify_fun(model), 3)
    end
  end
end
