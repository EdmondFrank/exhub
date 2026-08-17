defmodule Exhub.Router.TokenPoolTest do
  use ExUnit.Case, async: false

  alias Exhub.Router.TokenPool

  @default_keys [:giteeai_api_key, :giteeai_token_api_key, :giteeai_request_api_key, :giteeai_pool_threshold]

  setup do
    original =
      Enum.map(@default_keys, fn key ->
        {key, Application.get_env(:exhub, key)}
      end)

    Enum.each(@default_keys, fn key -> Application.delete_env(:exhub, key) end)

    on_exit(fn ->
      Enum.each(original, fn {key, value} ->
        if value == nil, do: Application.delete_env(:exhub, key), else: Application.put_env(:exhub, key, value)
      end)
    end)

    :ok
  end

  describe "threshold/0" do
    test "defaults to 20_000 when not configured" do
      assert TokenPool.threshold() == 20_000
    end

    test "reads the configured threshold" do
      Application.put_env(:exhub, :giteeai_pool_threshold, 50_000)
      assert TokenPool.threshold() == 50_000
    end
  end

  describe "select_mode/1" do
    test "uses token-based below the threshold" do
      assert TokenPool.select_mode(0) == :token_based
      assert TokenPool.select_mode(19_999) == :token_based
    end

    test "uses request-based at and above the threshold" do
      assert TokenPool.select_mode(20_000) == :request_based
      assert TokenPool.select_mode(200_000) == :request_based
    end

    test "falls back to token-based for unknown token counts" do
      assert TokenPool.select_mode(nil) == :token_based
    end
  end

  describe "enabled?/0" do
    test "disabled when no pool keys are configured" do
      refute TokenPool.enabled?()
    end

    test "enabled when either pool key is configured" do
      Application.put_env(:exhub, :giteeai_token_api_key, "token-key")
      assert TokenPool.enabled?()

      Application.delete_env(:exhub, :giteeai_token_api_key)
      Application.put_env(:exhub, :giteeai_request_api_key, "request-key")
      assert TokenPool.enabled?()
    end
  end

  describe "api_key/1" do
    test "returns the key for the given mode" do
      Application.put_env(:exhub, :giteeai_token_api_key, "token-key")
      Application.put_env(:exhub, :giteeai_request_api_key, "request-key")

      assert TokenPool.api_key(:token_based) == "token-key"
      assert TokenPool.api_key(:request_based) == "request-key"
    end

    test "falls back to the default giteeai key when the mode key is unset" do
      Application.put_env(:exhub, :giteeai_api_key, "default-key")
      assert TokenPool.api_key(:token_based) == "default-key"
      assert TokenPool.api_key(:request_based) == "default-key"
    end
  end

  describe "estimate_tokens/1" do
    test "estimates from messages, system, and tools" do
      body = %{
        "messages" => [%{"content" => "Hello world"}],
        "system" => "You are a helpful assistant.",
        "tools" => [%{"name" => "web_search", "input_schema" => %{"type" => "object"}}]
      }

      assert TokenPool.estimate_tokens(body) > 0
    end

    test "handles list content and empty bodies" do
      body = %{
        "messages" => [
          %{"content" => [%{"type" => "text", "text" => "Hi there"}]},
          %{"content" => ""}
        ]
      }

      assert TokenPool.estimate_tokens(body) >= 2
      assert TokenPool.estimate_tokens(%{}) == 0
      assert TokenPool.estimate_tokens(nil) == 0
    end
  end

  describe "resolve_token/3" do
    test "returns the fallback when the pool is disabled" do
      Application.put_env(:exhub, :giteeai_api_key, "default-key")

      assert TokenPool.resolve_token("deepseek-v3", %{"messages" => []}, fallback: "default-key") ==
               "default-key"
    end

    test "returns the fallback for non-GiteeAI models" do
      Application.put_env(:exhub, :giteeai_token_api_key, "token-key")

      assert TokenPool.resolve_token("claude-3.7-sonnet", %{"messages" => []}, fallback: "fallback") ==
               "fallback"
    end

    test "uses the token-based pool for small contexts" do
      Application.put_env(:exhub, :giteeai_token_api_key, "token-key")
      Application.put_env(:exhub, :giteeai_request_api_key, "request-key")

      body = %{"messages" => [%{"content" => "short"}]}

      assert TokenPool.resolve_token("deepseek-v3", body, fallback: "fallback") == "token-key"
    end

    test "uses the request-based pool at or above the threshold" do
      Application.put_env(:exhub, :giteeai_token_api_key, "token-key")
      Application.put_env(:exhub, :giteeai_request_api_key, "request-key")

      body = %{"messages" => [%{"content" => String.duplicate("x", 80_001)}]}

      assert TokenPool.resolve_token("qwen3.5-27b", body, fallback: "fallback") == "request-key"
    end

    test "falls back to the default key when the selected mode key is unset" do
      Application.put_env(:exhub, :giteeai_token_api_key, "token-key")
      Application.put_env(:exhub, :giteeai_api_key, "default-key")

      body = %{"messages" => [%{"content" => String.duplicate("x", 80_001)}]}

      assert TokenPool.resolve_token("deepseek-v3", body, fallback: "fallback") == "default-key"
    end
  end

  describe "estimate_langchain_tokens/1" do
    test "estimates from LangChain message structs" do
      messages = [
        LangChain.Message.new_system!("You are a helpful assistant."),
        LangChain.Message.new_user!("Hello")
      ]

      assert TokenPool.estimate_langchain_tokens(messages) > 0
    end

    test "estimates from ContentPart lists" do
      messages = [
        %LangChain.Message{
          role: :user,
          content: [
            %LangChain.Message.ContentPart{type: :text, content: "Hello there"},
            %LangChain.Message.ContentPart{type: :text, content: "Second part"}
          ]
        }
      ]

      assert TokenPool.estimate_langchain_tokens(messages) >= 4
    end

    test "tolerates plain maps and non-list inputs" do
      assert TokenPool.estimate_langchain_tokens([%{"content" => "hi"}]) >= 0
      assert TokenPool.estimate_langchain_tokens(nil) == 0
    end
  end

  describe "resolve_langchain_key/3" do
    test "returns fallback when the pool is disabled" do
      assert TokenPool.resolve_langchain_key("deepseek-v3", [%LangChain.Message{role: :user, content: "hi"}], fallback: "original") ==
               "original"
    end

    test "returns fallback for non-GiteeAI models" do
      Application.put_env(:exhub, :giteeai_token_api_key, "token-key")

      assert TokenPool.resolve_langchain_key("claude-sonnet-4", [], fallback: "original") == "original"
    end

    test "uses the token-based pool for small contexts" do
      Application.put_env(:exhub, :giteeai_token_api_key, "token-key")
      Application.put_env(:exhub, :giteeai_request_api_key, "request-key")

      key =
        TokenPool.resolve_langchain_key(
          "openai/deepseek-v3",
          [LangChain.Message.new_user!("short")],
          fallback: "original"
        )

      assert key == "token-key"
    end

    test "uses the request-based pool at or above the threshold" do
      Application.put_env(:exhub, :giteeai_token_api_key, "token-key")
      Application.put_env(:exhub, :giteeai_request_api_key, "request-key")

      key =
        TokenPool.resolve_langchain_key(
          "deepseek-v3",
          [LangChain.Message.new_user!(String.duplicate("x", 80_001))],
          fallback: "original"
        )

      assert key == "request-key"
    end

    test "falls back when the selected mode key is unset" do
      Application.put_env(:exhub, :giteeai_token_api_key, "token-key")

      key =
        TokenPool.resolve_langchain_key(
          "deepseek-v3",
          [LangChain.Message.new_user!(String.duplicate("x", 80_001))],
          fallback: "original"
        )

      assert key == "original"
    end
  end
end