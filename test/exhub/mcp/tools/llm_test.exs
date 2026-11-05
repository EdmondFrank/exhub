defmodule Exhub.MCP.Tools.LlmTest do
  use ExUnit.Case, async: false

  alias Exhub.Llm.LlmConfigServer
  alias Exhub.MCP.Hub.BuiltInRegistry
  alias Exhub.MCP.Tools.Llm.Chat
  alias Exhub.MCP.Tools.Llm.ChatJson
  alias Exhub.MCP.Tools.Llm.Models

  @llms %{
    "openai/qwen3.8-flash" => %{
      api_base: "https://api.moark.com/v1",
      api_key: "test",
      model: "openai/qwen3.8-flash"
    },
    "openai/kimi-k2.6" => %{
      api_base: "https://api.moark.com/v1",
      api_key: "test",
      model: "openai/kimi-k2.6"
    }
  }

  setup do
    original_llms = Application.get_env(:exhub, :llms)

    Application.put_env(:exhub, :llms, @llms)

    on_exit(fn ->
      case original_llms do
        nil -> Application.delete_env(:exhub, :llms)
        value -> Application.put_env(:exhub, :llms, value)
      end
    end)

    start_supervised!(LlmConfigServer)
    :ok
  end

  defp text(%{content: [%{"text" => text} | _]}), do: text

  test "models lists the configured models" do
    {:reply, resp, %{}} = Models.execute(%{}, %{})

    assert resp.isError == false
    decoded = resp |> text() |> Jason.decode!()

    assert decoded["default"] == "openai/qwen3.8-flash--no-think"
    assert decoded["count"] == 2
    assert Enum.any?(decoded["models"], &(&1["name"] == "openai/kimi-k2.6"))
  end

  test "chat reports a missing prompt without calling a model" do
    {:reply, resp, %{}} = Chat.execute(%{}, %{})

    assert resp.isError == true
    assert text(resp) =~ "`messages` (array) or `prompt`"
  end

  test "chat_json requires a schema" do
    {:reply, resp, %{}} = ChatJson.execute(%{prompt: "hi"}, %{})

    assert resp.isError == true
    assert text(resp) =~ "`schema` is required"
  end

  test "models never returns API keys or base URLs" do
    {:reply, resp, %{}} = Models.execute(%{}, %{})
    payload = text(resp)

    refute payload =~ "api.moark.com"
    refute payload =~ "api_key"
  end

  describe "registration" do
    test "is exposed as the built-in `llm` server" do
      assert BuiltInRegistry.built_in?("llm")
      assert BuiltInRegistry.server_module("llm") == Exhub.MCP.LlmServer
      assert BuiltInRegistry.server_info("llm")["name"] == "exhub-llm-server"
    end

    test "advertises models, chat and chat_json" do
      tools = BuiltInRegistry.list_tools("llm")

      assert tools |> Enum.map(& &1["name"]) |> Enum.sort() == ["chat", "chat_json", "models"]
      assert Enum.all?(tools, &(&1["server"] == "llm"))

      chat = Enum.find(tools, &(&1["name"] == "chat"))
      properties = chat["inputSchema"]["properties"] |> Map.keys()

      for field <- ~w(model messages prompt system temperature max_tokens timeout_ms) do
        assert field in properties
      end

      chat_json = Enum.find(tools, &(&1["name"] == "chat_json"))
      assert "schema" in (chat_json["inputSchema"]["properties"] |> Map.keys())
    end
  end
end
