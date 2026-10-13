defmodule Exhub.Llm.LlmConfigServerTest do
  use ExUnit.Case, async: false

  alias Exhub.Llm.LlmConfigServer

  setup do
    original = Application.get_env(:exhub, :llms)

    Application.put_env(:exhub, :llms, %{
      "openai/qwen3.8-flash" => %{
        api_base: "https://api.moark.com/v1",
        api_key: "test",
        model: "openai/qwen3.8-flash"
      }
    })

    on_exit(fn ->
      case original do
        nil -> Application.delete_env(:exhub, :llms)
        value -> Application.put_env(:exhub, :llms, value)
      end
    end)

    :ok
  end

  test "resolves the variant-suffixed default to the base model's config" do
    start_supervised!(LlmConfigServer)

    assert LlmConfigServer.get_default_llm_name() == "openai/qwen3.8-flash--no-think"

    assert {:ok, %{model: "openai/qwen3.8-flash", api_base: "https://api.moark.com/v1"}} =
             LlmConfigServer.get_default_llm_config()
  end

  test "get_llm_config/1 resolves a variant-suffixed name too" do
    start_supervised!(LlmConfigServer)

    assert {:ok, %{model: "openai/qwen3.8-flash"}} =
             LlmConfigServer.get_llm_config("openai/qwen3.8-flash--effort-medium")
  end

  test "an unknown name still returns an error" do
    start_supervised!(LlmConfigServer)

    assert {:error, message} = LlmConfigServer.get_llm_config("openai/nope")
    assert message =~ "not found"
  end
end
