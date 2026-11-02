defmodule Exhub.Router.TokenPoolTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  alias Exhub.Router.TokenPool

  @default_keys [
    :giteeai_api_key,
    :giteeai_token_api_key,
    :giteeai_request_api_key,
    :giteeai_pool_threshold,
    :giteeai_pool_weights
  ]

  setup do
    original =
      Enum.map(@default_keys, fn key ->
        {key, Application.get_env(:exhub, key)}
      end)

    Enum.each(@default_keys, fn key -> Application.delete_env(:exhub, key) end)

    on_exit(fn ->
      Enum.each(original, fn {key, value} ->
        if value == nil,
          do: Application.delete_env(:exhub, key),
          else: Application.put_env(:exhub, key, value)
      end)
    end)

    :ok
  end

  describe "threshold/0" do
    test "defaults to 64_000 when not configured" do
      assert TokenPool.threshold() == 64_000
    end

    test "reads the configured threshold" do
      Application.put_env(:exhub, :giteeai_pool_threshold, 50_000)
      assert TokenPool.threshold() == 50_000
    end
  end

  describe "select_mode/1" do
    test "uses token-based below the threshold" do
      assert TokenPool.select_mode(0) == :token_based
      assert TokenPool.select_mode(63_999) == :token_based
    end

    test "uses request-based at and above the threshold" do
      assert TokenPool.select_mode(64_000) == :request_based
      assert TokenPool.select_mode(200_000) == :request_based
    end

    test "falls back to token-based for unknown token counts" do
      assert TokenPool.select_mode(nil) == :token_based
    end

    test "honours configured weights when selecting the mode" do
      Application.put_env(:exhub, :giteeai_pool_weights, %{token: 2.0, request: 20_000.0})

      assert TokenPool.select_mode(9_999) == :token_based
      assert TokenPool.select_mode(10_000) == :request_based
    end
  end

  describe "weights/0" do
    test "defaults to token: 1.0 and the configured threshold as the request weight" do
      assert TokenPool.weights() == %{token: 1.0, request: 64_000.0}

      Application.put_env(:exhub, :giteeai_pool_threshold, 50_000)
      assert TokenPool.weights() == %{token: 1.0, request: 50_000.0}
    end

    test "reads configured weights, filling unspecified keys from defaults" do
      Application.put_env(:exhub, :giteeai_pool_weights, %{token: 2.0, request: 8_000.0})
      assert TokenPool.weights() == %{token: 2.0, request: 8_000.0}

      Application.put_env(:exhub, :giteeai_pool_weights, %{token: 2.0})
      assert TokenPool.weights() == %{token: 2.0, request: 64_000.0}
    end

    test "ignores invalid weight values" do
      Application.put_env(:exhub, :giteeai_pool_weights, %{token: -1.0, request: "bad"})
      assert TokenPool.weights() == %{token: 1.0, request: 64_000.0}
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

    test "counts tool-call arguments, not just textual content" do
      content_only = %{"messages" => [%{"role" => "assistant", "content" => "ok"}]}

      with_tool_calls = %{
        "messages" => [
          %{
            "role" => "assistant",
            "content" => nil,
            "tool_calls" => [
              %{
                "id" => "call_1",
                "function" => %{
                  "name" => "write_file",
                  "arguments" => String.duplicate("x", 40_000)
                }
              }
            ]
          }
        ]
      }

      assert TokenPool.estimate_tokens(with_tool_calls) >
               TokenPool.estimate_tokens(content_only) + 9_000
    end

    test "routes a large agent-shaped request to the request pool" do
      # The content-only heuristic saw ~60k tokens here and kept the request on
      # the token pool; the tool arguments, tool results and tool schema push
      # the real size past the break-even.
      body = %{
        "messages" => [
          %{"role" => "user", "content" => String.duplicate("u", 240_000)},
          %{
            "role" => "assistant",
            "content" => nil,
            "tool_calls" => [
              %{
                "function" => %{
                  "name" => "write_file",
                  "arguments" => String.duplicate("a", 240_000)
                }
              }
            ]
          },
          %{"role" => "tool", "content" => String.duplicate("t", 240_000)}
        ],
        "tools" => [
          %{
            "name" => "search",
            "input_schema" => %{"description" => String.duplicate("s", 240_000)}
          }
        ]
      }

      estimate = TokenPool.estimate_tokens(body)

      assert estimate > 200_000
      assert TokenPool.select_mode(estimate) == :request_based

      Application.put_env(:exhub, :giteeai_token_api_key, "token-key")
      Application.put_env(:exhub, :giteeai_request_api_key, "request-key")

      assert TokenPool.resolve_token("deepseek-v3", body, fallback: "default-key") ==
               "request-key"
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

      assert TokenPool.resolve_token("claude-3.7-sonnet", %{"messages" => []},
               fallback: "fallback"
             ) ==
               "fallback"
    end

    test "uses the token-based pool for small contexts" do
      Application.put_env(:exhub, :giteeai_token_api_key, "token-key")
      Application.put_env(:exhub, :giteeai_request_api_key, "request-key")

      body = %{"messages" => [%{"content" => "short"}]}

      assert TokenPool.resolve_token("deepseek-v3", body, fallback: "fallback") == "token-key"
    end

    test "uses the token-based pool for GLM-5.3-Flash" do
      Application.put_env(:exhub, :giteeai_token_api_key, "token-key")

      body = %{"messages" => [%{"content" => "short"}]}

      assert TokenPool.resolve_token("GLM-5.3-Flash", body, fallback: "fallback") == "token-key"
    end

    test "uses the token-based pool for MiniMax-M3.1-Flash" do
      Application.put_env(:exhub, :giteeai_token_api_key, "token-key")

      body = %{"messages" => [%{"content" => "short"}]}

      assert TokenPool.resolve_token("MiniMax-M3.1-Flash", body, fallback: "fallback") ==
               "token-key"
    end

    test "uses the request-based pool for MiniMax-M3.1-Flash at or above the threshold" do
      Application.put_env(:exhub, :giteeai_token_api_key, "token-key")
      Application.put_env(:exhub, :giteeai_request_api_key, "request-key")

      body = %{"messages" => [%{"content" => String.duplicate("x", 400_001)}]}

      assert TokenPool.resolve_token("MiniMax-M3.1-Flash", body, fallback: "fallback") ==
               "request-key"
    end

    test "uses the request-based pool at or above the threshold" do
      Application.put_env(:exhub, :giteeai_token_api_key, "token-key")
      Application.put_env(:exhub, :giteeai_request_api_key, "request-key")

      body = %{"messages" => [%{"content" => String.duplicate("x", 400_001)}]}

      assert TokenPool.resolve_token("qwen3.5-27b", body, fallback: "fallback") == "request-key"
    end

    test "falls back to the default key when the selected mode key is unset" do
      Application.put_env(:exhub, :giteeai_token_api_key, "token-key")
      Application.put_env(:exhub, :giteeai_api_key, "default-key")

      body = %{"messages" => [%{"content" => String.duplicate("x", 400_001)}]}

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

    test "counts tool results, which hold the bulk of an agent context" do
      result = %LangChain.Message.ToolResult{
        name: "read_file",
        tool_call_id: "call_1",
        content: String.duplicate("y", 40_000)
      }

      message = %LangChain.Message{role: :tool, tool_results: [result]}

      assert TokenPool.estimate_langchain_tokens([message]) > 9_000
    end

    test "counts tool-call arguments" do
      call = %LangChain.Message.ToolCall{
        call_id: "call_1",
        name: "write_file",
        arguments: %{"content" => String.duplicate("z", 40_000)}
      }

      message = %LangChain.Message{role: :assistant, content: nil, tool_calls: [call]}

      assert TokenPool.estimate_langchain_tokens([message]) > 9_000
    end

    test "counts the tool definitions given to the chain" do
      tool = %LangChain.Function{
        name: "search",
        description: "Search the vault",
        parameters_schema: %{
          "type" => "object",
          "properties" => %{"q" => %{"description" => String.duplicate("d", 40_000)}}
        }
      }

      messages = [LangChain.Message.new_user!("hi")]

      assert TokenPool.estimate_langchain_tokens(messages, [tool]) >
               TokenPool.estimate_langchain_tokens(messages) + 9_000
    end
  end

  describe "resolve_langchain_key/3" do
    test "returns fallback when the pool is disabled" do
      assert TokenPool.resolve_langchain_key(
               "deepseek-v3",
               [%LangChain.Message{role: :user, content: "hi"}],
               fallback: "original"
             ) ==
               "original"
    end

    test "returns fallback for non-GiteeAI models" do
      Application.put_env(:exhub, :giteeai_token_api_key, "token-key")

      assert TokenPool.resolve_langchain_key("claude-sonnet-4", [], fallback: "original") ==
               "original"
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
          [LangChain.Message.new_user!(String.duplicate("x", 400_001))],
          fallback: "original"
        )

      assert key == "request-key"
    end

    test "falls back when the selected mode key is unset" do
      Application.put_env(:exhub, :giteeai_token_api_key, "token-key")

      key =
        TokenPool.resolve_langchain_key(
          "deepseek-v3",
          [LangChain.Message.new_user!(String.duplicate("x", 400_001))],
          fallback: "original"
        )

      assert key == "original"
    end

    test "counts the tools passed in opts towards the estimate" do
      Application.put_env(:exhub, :giteeai_token_api_key, "token-key")
      Application.put_env(:exhub, :giteeai_request_api_key, "request-key")

      messages = [LangChain.Message.new_user!("hi")]

      tool = %LangChain.Function{
        name: "search",
        description: "Search the vault",
        parameters_schema: %{"description" => String.duplicate("d", 300_000)}
      }

      assert TokenPool.resolve_langchain_key("deepseek-v3", messages,
               fallback: "original",
               tools: [tool]
             ) == "request-key"
    end
  end

  describe "decision logging" do
    test "logs the estimate and the selected pool without the key" do
      Application.put_env(:exhub, :giteeai_token_api_key, "token-key")

      log =
        capture_log(fn ->
          TokenPool.resolve_token("deepseek-v3", %{"messages" => [%{"content" => "hi"}]},
            fallback: "fallback"
          )
        end)

      assert log =~ "[TokenPool]"
      assert log =~ "using token_based pool"
      refute log =~ "token-key"
    end
  end
end
