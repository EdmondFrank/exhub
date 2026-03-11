defmodule Exhub.Router do
  alias Exhub.ProxyPlug
  require Logger
  use Plug.Router
  use PlugSocket
  alias UUID
  alias Exhub.Llm.LlmConfigServer

  @proxy Application.compile_env(:exhub, :proxy, "")
  @default_timeout Application.compile_env(:exhub, :default_timeout, 1_800_000)

  plug(Plug.Parsers,
    parsers: [:urlencoded, {:json, json_decoder: Jason}],
    pass: ["*/*"]
  )

  socket("/exhub", Exhub.SocketHandler,
    websocket: [timeout: @default_timeout, recv_timeout: @default_timeout],
    longpoll: false
  )

  plug(:match)
  plug(:dispatch)

  post "/groq/v1/*path" do
    ProxyPlug.forward_upstream(conn, "https://api.groq.com/openai/v1",
      client_options: [proxy: @proxy]
    )
  end

  post "/google/v1/*path" do
    ProxyPlug.forward_upstream(conn, "https://generativelanguage.googleapis.com/",
      client_options: [proxy: @proxy]
    )
  end

  post "/cohere/v1/*path" do
    ProxyPlug.forward_upstream(conn, "https://api.cohere.ai/compatibility/v1",
      client_options: [proxy: @proxy]
    )
  end

  post "/samba/v1/*path" do
    ProxyPlug.forward_upstream(conn, "https://api.sambanova.ai/v1",
      client_options: [proxy: @proxy]
    )
  end

  get "/openai/v1/*path" do
    token = Application.get_env(:exhub, :openai_api_key, "")

    options = [
      custom_headers: [{"Authorization", "Bearer #{token}"}],
      client_options: [timeout: @default_timeout, recv_timeout: @default_timeout]
    ]

    Logger.info("[OpenAI Proxy] Forwarding request - models")
    ProxyPlug.forward_upstream(conn, "http://20.246.88.31:8080/v1", options)
  end

  post "/openai/v1/*path" do
    # Extract model name from JSON body (if present)
    model_name =
      case Plug.Conn.get_req_header(conn, "content-type") do
        ["application/json" <> _] ->
          case conn.body_params do
            %{"model" => model} -> model
            _ -> nil
          end

        _ ->
          nil
      end

    # Model to target mapping (extendable)
    model_target_map = %{
      "step3" => "https://ai.gitee.com/v1",
      "glm-4_5" => "https://ai.gitee.com/v1",
      "glm-4_5v" => "https://ai.gitee.com/v1",
      "glm-4.6" => "https://ai.gitee.com/v1",
      "glm-4.7" => "https://ai.gitee.com/v1",
      "glm-5" => "https://ai.gitee.com/v1",
      "deepseek-v3" => "https://ai.gitee.com/v1",
      "deepseek-r1" => "https://ai.gitee.com/v1",
      "deepseek-v3_1" => "https://ai.gitee.com/v1",
      "deepseek-v3_1-terminus" => "https://ai.gitee.com/v1",
      "deepseek-v3.2" => "https://ai.gitee.com/v1",
      "deepseek-v3.2-exp" => "https://ai.gitee.com/v1",
      "gpt-oss-120b" => "https://ai.gitee.com/v1",
      "internvl3-78b" => "https://ai.gitee.com/v1",
      "kimi-k2.5" => "https://ai.gitee.com/v1",
      "kimi-k2-instruct" => "https://ai.gitee.com/v1",
      "kimi-k2-thinking" => "https://ai.gitee.com/v1",
      "kimi-for-coding" => "https://api.kimi.com/coding/v1",
      "qwen3.5-27b" => "https://ai.gitee.com/v1",
      "qwen3.5-35b-a3b" => "https://ai.gitee.com/v1",
      "qwen3-235b-a22b" => "https://ai.gitee.com/v1",
      "qwen3-235b-a22b-instruct-2507" => "https://ai.gitee.com/v1",
      "qwen3-next-80b-a3b-instruct" => "https://ai.gitee.com/v1",
      "qwen3-next-80b-a3b-thinking" => "https://ai.gitee.com/v1",
      "qwen3-coder-next" => "https://ai.gitee.com/v1",
      "qwen3-coder-480b-a35b-instruct" => "https://ai.gitee.com/v1",
      "tngtech/deepseek-r1t2-chimera:free" => "https://openrouter.ai/api/v1",
      "minimax/minimax-m2:free" => "https://openrouter.ai/api/v1",
      "minimax-m2" => "https://ai.gitee.com/v1",
      "minimax-m2.1" => "https://ai.gitee.com/v1",
      "minimax-m2.5" => "https://ai.gitee.com/v1",
      "minimax-m2-preview" => "https://api.minimaxi.com/v1",
      "gemini-2.5-pro" => "http://localhost:8765/v1",
      "gemini-2.5-flash" => "http://localhost:8765/v1",
      "openrouter/polaris-alpha" => "https://openrouter.ai/api/v1"
    }

    # Model to token mapping (extendable)
    model_token_map = %{
      "step3" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "glm-4_5" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "glm-4_5v" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "glm-4.6" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "glm-4.7" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "glm-5" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "deepseek-v3" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "deepseek-r1" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "deepseek-v3_1" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "deepseek-v3_1-terminus" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "deepseek-v3.2" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "deepseek-v3.2-exp" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "gpt-oss-120b" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "internvl3-78b" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "kimi-k2.5" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "kimi-k2-instruct" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "kimi-k2-thinking" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "kimi-for-coding" => Application.get_env(:exhub, :kimi_api_key, ""),
      "qwen3.5-27b" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "qwen3.5-35b-a3b" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "qwen3-235b-a22b" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "qwen3-235b-a22b-instruct-2507" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "qwen3-next-80b-a3b-instruct" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "qwen3-next-80b-a3b-thinking" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "qwen3-coder-next" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "qwen3-coder-480b-a35b-instruct" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "tngtech/deepseek-r1t2-chimera:free" =>
        Application.get_env(:exhub, :openrouter_api_key, ""),
      "minimax/minimax-m2:free" => Application.get_env(:exhub, :openrouter_api_key, ""),
      "minimax-m2" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "minimax-m2.1" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "minimax-m2.5" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "minimax-m2-preview" => Application.get_env(:exhub, :minimax_api_key, ""),
      "openrouter/polaris-alpha" => Application.get_env(:exhub, :openrouter_api_key, "")
    }

    target_url = Map.get(model_target_map, model_name, "http://20.246.88.31:8080/v1")

    token = Map.get(model_token_map, model_name, Application.get_env(:exhub, :openai_api_key, ""))

    options = [
      custom_headers: [{"Authorization", "Bearer #{token}"}],
      client_options: [timeout: @default_timeout, recv_timeout: @default_timeout]
    ]

    Logger.info(
      "[OpenAI Proxy] Forwarding request - model: #{inspect(model_name)}, target: #{target_url}, has_token: #{token != ""}"
    )

    ProxyPlug.forward_upstream(
      conn,
      target_url,
      options
    )
  end

  post "/anthropic/v1/*path" do
    model_name =
      case Plug.Conn.get_req_header(conn, "content-type") do
        ["application/json" <> _] ->
          case conn.body_params do
            %{"model" => model} -> model
            _ -> nil
          end

        _ ->
          nil
      end

    model_target_map = %{
      "minimax-m2.1" => "https://api.minimaxi.com/anthropic/v1",
      "minimax-m2-preview" => "https://api.minimaxi.com/anthropic/v1"
    }

    model_token_map = %{
      "minimax-m2.1" => Application.get_env(:exhub, :minimax_api_key, ""),
      "minimax-m2-preview" => Application.get_env(:exhub, :minimax_api_key, "")
    }

    model_proxy_map = %{
      "minimax-m2.1" => false,
      "minimax-m2-preview" => false
    }

    target_url = Map.get(model_target_map, model_name, "http://20.246.88.31:8080/v1")

    token =
      Map.get(model_token_map, model_name, Application.get_env(:exhub, :anthropic_api_key, ""))

    use_proxy = Map.get(model_proxy_map, model_name, false)

    proxy = if use_proxy, do: @proxy, else: ""

    options = [
      custom_headers: [{"x-api-key", token}],
      client_options: [timeout: @default_timeout, recv_timeout: @default_timeout, proxy: proxy]
    ]

    Logger.info(
      "[Anthropic Proxy] Forwarding request - model: #{inspect(model_name)}, target: #{target_url}, proxy: #{proxy}, use_proxy: #{use_proxy}"
    )

    ProxyPlug.forward_upstream(
      conn,
      target_url,
      options
    )
  end

  defp convert_anthropic_to_langchain(anthropic_request) do
    messages = Map.get(anthropic_request, "messages", [])
    system = Map.get(anthropic_request, "system")
    tools = Map.get(anthropic_request, "tools", [])
    max_tokens = Map.get(anthropic_request, "max_tokens", 4096)

    langchain_messages = convert_messages_to_langchain(messages)

    result = %{
      messages: langchain_messages,
      max_tokens: max_tokens
    }

    result =
      if system do
        system_message = convert_system_to_langchain(system)
        if system_message, do: Map.put(result, :system, system_message), else: result
      else
        result
      end

    if tools != [] do
      langchain_tools = convert_tools_to_langchain(tools)
      Map.put(result, :tools, langchain_tools)
    else
      result
    end
  end

  defp convert_system_to_langchain(system) when is_binary(system) and system != "" do
    LangChain.Message.new_system!(system)
  end

  defp convert_system_to_langchain(list) when is_list(list) and list != [] do
    content = convert_content_blocks_to_string(list)
    LangChain.Message.new_system!(content)
  end

  defp convert_system_to_langchain(_), do: nil

  defp convert_messages_to_langchain(messages) when is_list(messages) do
    Enum.map(messages, fn msg ->
      role = Map.get(msg, "role")
      content = Map.get(msg, "content")

      case role do
        "user" ->
          case content do
            str when is_binary(str) ->
              LangChain.Message.new_user!(str)

            list when is_list(list) ->
              parts = convert_content_blocks_to_parts(list)
              LangChain.Message.new_user!(parts)

            _ ->
              nil
          end

        "assistant" ->
          case content do
            str when is_binary(str) ->
              LangChain.Message.new_assistant!(str)

            list when is_list(list) ->
              parts = convert_content_blocks_to_parts(list)
              LangChain.Message.new_assistant!(parts)

            _ ->
              nil
          end

        _ ->
          nil
      end
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp convert_messages_to_langchain(_), do: []

  defp convert_content_blocks_to_parts(blocks) when is_list(blocks) do
    Enum.map(blocks, fn block ->
      case block do
        %{"type" => "text", "text" => text} when is_binary(text) ->
          LangChain.Message.ContentPart.text!(text)

        %{"type" => "text", "text" => text, "citations" => _citations} when is_binary(text) ->
          LangChain.Message.ContentPart.text!(text)

        %{"type" => "image", "source" => source} ->
          convert_image_source(source)

        %{"type" => "tool_use", "id" => id, "name" => name, "input" => input} ->
          %{
            type: :tool_use,
            id: id,
            name: name,
            input: input
          }

        %{"type" => "tool_result", "tool_use_id" => tool_use_id, "content" => content} ->
          %{
            type: :tool_result,
            tool_use_id: tool_use_id,
            content: convert_tool_result_content(content)
          }

        %{"type" => "thinking", "thinking" => thinking, "signature" => signature} ->
          %{
            type: :thinking,
            thinking: thinking,
            signature: signature
          }

        _ ->
          nil
      end
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp convert_content_blocks_to_string(blocks) when is_list(blocks) do
    Enum.map_join(blocks, fn block ->
      case block do
        %{"type" => "text", "text" => text} when is_binary(text) ->
          text

        %{"type" => "thinking", "thinking" => thinking} when is_binary(thinking) ->
          "[Thinking]\n#{thinking}\n[/Thinking]"

        _ ->
          ""
      end
    end)
  end

  defp convert_image_source(source) do
    case source do
      %{"type" => "base64", "data" => data, "media_type" => media_type}
      when is_binary(data) and is_binary(media_type) ->
        media = convert_media_type(media_type)
        LangChain.Message.ContentPart.image!(data, media: media)

      %{"type" => "url", "url" => _url} ->
        raise "Anthropic doesn't support image_url directly"

      _ ->
        nil
    end
  end

  defp convert_media_type("image/jpeg"), do: :jpeg
  defp convert_media_type("image/png"), do: :png
  defp convert_media_type("image/gif"), do: :gif
  defp convert_media_type("image/webp"), do: :webp
  defp convert_media_type(media_type) when is_binary(media_type), do: media_type
  defp convert_media_type(_), do: :png

  defp convert_tool_result_content(content) do
    case content do
      str when is_binary(str) -> str
      list when is_list(list) -> convert_content_blocks_to_string(list)
      _ -> to_string(content)
    end
  end

  defp convert_tools_to_langchain(tools) when is_list(tools) do
    # Enum.map(tools, fn tool ->
    #   name = Map.get(tool, "name")
    #   description = Map.get(tool, "description", "")
    #   input_schema = Map.get(tool, "input_schema", %{})

    #   LangChain.Function.new!(%{
    #     name: name,
    #     description: description,
    #     schema: input_schema
    #   })
    # end)
    []
  end

  defp convert_tools_to_langchain(_), do: []

  # Anthropic API compatible endpoints
  post "/v1/messages" do
    # Parse the request body
    body = conn.body_params

    # Extract required fields - ensure null values become defaults
    model = Map.get(body, "model", "")
    messages = Map.get(body, "messages", []) || []
    system = Map.get(body, "system")
    stream = Map.get(body, "stream", true)
    tools = Map.get(body, "tools", []) || []
    tool_choice = Map.get(body, "tool_choice")
    max_tokens = Map.get(body, "max_tokens")

    Logger.info("Starting streaming response for model: #{model}")

    # Build Anthropic request format for conversion
    anthropic_request = %{
      "model" => model,
      "messages" => messages,
      "system" => system,
      "stream" => stream,
      "tools" => tools,
      "tool_choice" => tool_choice,
      "max_tokens" => max_tokens || 4096
    }

    # Convert to LangChain format using the existing function.
    langchain_request = convert_anthropic_to_langchain(anthropic_request)
    langchain_messages = Map.get(langchain_request, :messages, []) || []
    langchain_tools = Map.get(langchain_request, :tools, []) || []
    langchain_system = Map.get(langchain_request, :system)

    # Prepend system message if present
    all_messages =
      if langchain_system do
        [langchain_system | langchain_messages]
      else
        langchain_messages
      end

    # Prepare custom context for tools
    custom_context = %{}

    # Get LLM chain - need to convert model name to LLM name format
    {:ok, config} = LlmConfigServer.get_default_llm_config()

    llm_name = config[:model]

    api_key = get_api_key_for_model(model)

    # Create chain with API key if available
    chain_options = %{stream: stream}

    chain_options =
      if api_key != "", do: Map.put(chain_options, :api_key, api_key), else: chain_options

    case Exhub.Llm.Chain.create_llm_chain(llm_name, chain_options) do
      llm_chain when is_map(llm_chain) ->
        if stream do
          # Streaming mode
          response_id = "msg_#{UUID.uuid4()}"

          Logger.info(
            "Starting streaming response for model: #{model}, llm: #{llm_name}, response_id: #{response_id}"
          )

          # Start chunked response
          conn = put_resp_header(conn, "content-type", "text/event-stream")
          conn = put_resp_header(conn, "cache-control", "no-cache")
          conn = put_resp_header(conn, "connection", "keep-alive")
          conn = send_chunked(conn, 200)

          # Send message_start event with proper Anthropic format
          input_tokens = estimate_input_tokens(messages, system, tools)
          model_name = model || ""

          message_start = %{
            "type" => "message_start",
            "message" => %{
              "id" => response_id,
              "type" => "message",
              "role" => "assistant",
              "model" => model_name,
              "content" => [],
              "stop_reason" => nil,
              "stop_sequence" => nil,
              "usage" => %{
                "input_tokens" => input_tokens,
                "cache_creation_input_tokens" => 0,
                "cache_read_input_tokens" => 0,
                "output_tokens" => 0
              }
            }
          }

          conn = send_sse_event(conn, "message_start", message_start)

          # Send content_block_start event for first text block
          # Note: content block type depends on whether thinking is enabled
          # For standard responses, start with text block
          # For extended thinking, start with thinking block first
          conn =
            send_sse_event(conn, "content_block_start", %{
              "type" => "content_block_start",
              "index" => 0,
              "content_block" => %{"type" => "text", "text" => ""}
            })

          # Send ping event to keep connection alive
          conn = send_sse_event(conn, "ping", %{"type" => "ping"})

          # Set up callbacks that send messages to this process
          parent = self()

          callbacks = %{
            on_llm_new_delta: fn _chain, deltas ->
              Enum.each(deltas, fn delta ->
                if match?(%LangChain.MessageDelta{}, delta) do
                  delta_content_raw = Map.get(delta, :content)

                  delta_text =
                    cond do
                      is_nil(delta_content_raw) ->
                        nil

                      is_binary(delta_content_raw) ->
                        delta_content_raw

                      is_list(delta_content_raw) ->
                        Enum.map_join(delta_content_raw, "", fn part ->
                          if is_binary(part), do: part, else: Map.get(part, :content, "") || ""
                        end)

                      true ->
                        to_string(delta_content_raw)
                    end

                  if is_binary(delta_text) and delta_text != "" do
                    send(parent, {
                      :stream_chunk,
                      response_id,
                      %{
                        "type" => "content_block_delta",
                        "index" => 0,
                        "delta" => %{"type" => "text_delta", "text" => delta_text}
                      }
                    })

                    Logger.info("[#{response_id}] #{delta_text}")
                  end
                end
              end)
            end,
            on_message_processed: fn _chain, message ->
              Logger.info("LLM chain execution completed for #{response_id}")
              send(parent, {:message_complete, response_id, message})
            end,
            on_llm_error: fn _chain, error ->
              Logger.error("on_llm_error: LLM error - #{inspect(error)}")
              send(parent, {:error, response_id, error})
            end
          }

          # Add logging for task start
          task =
            Task.async(fn ->
              result =
                Exhub.Llm.Chain.run(
                  llm_chain,
                  all_messages,
                  langchain_tools,
                  custom_context,
                  callbacks
                )

              result
            end)

          # Stream results back to client using main process receive loop
          # Pass tracking context for token usage recording
          tracking_ctx = %{
            model: model || llm_name || "unknown",
            provider: "exhub",
            input_tokens: input_tokens
          }

          stream_response_loop(conn, task, response_id, tracking_ctx, "")

          conn
        else
          # Non-streaming mode
          Logger.info("Starting non-streaming response for model: #{model}, llm: #{llm_name}")

          case Exhub.Llm.Chain.execute(
                 llm_chain,
                 all_messages,
                 langchain_tools,
                 custom_context
               ) do
            response when is_binary(response) ->
              response_id = "msg_#{UUID.uuid4()}"

              # Ensure response is never null/empty
              safe_response =
                if is_nil(response) or not is_binary(response), do: "", else: response

              # Build content blocks in Anthropic format - always ensure content is an array
              # Each content block must have "type" and "text" fields
              content =
                if safe_response != "" do
                  [%{"type" => "text", "text" => safe_response}]
                else
                  [%{"type" => "text", "text" => ""}]
                end

              input_tokens = estimate_input_tokens(messages, system, tools)
              output_tokens = estimate_output_tokens(safe_response)

              # Track token usage
              spawn(fn ->
                Exhub.TokenUsage.TokenUsageStore.record_usage(
                  model || llm_name || "unknown",
                  "exhub",
                  input_tokens,
                  output_tokens,
                  %{request_id: response_id, timestamp: DateTime.utc_now()}
                )
              end)

              anthropic_response = %{
                "id" => response_id,
                "type" => "message",
                "role" => "assistant",
                "model" => model || "",
                "content" => content,
                "stop_reason" => "end_turn",
                "stop_sequence" => nil,
                "usage" => %{
                  "input_tokens" => input_tokens,
                  "output_tokens" => output_tokens,
                  "cache_creation_input_tokens" => 0,
                  "cache_read_input_tokens" => 0
                }
              }

              Logger.debug(
                "Non-streaming response - id: #{response_id}, input_tokens: #{input_tokens}, output_tokens: #{output_tokens}"
              )

              conn
              |> put_resp_content_type("application/json")
              |> send_resp(200, Jason.encode!(anthropic_response))

            error ->
              Logger.error("LLM execution failed: #{inspect(error)}")

              # Provide more helpful error messages
              error_message =
                cond do
                  String.contains?(to_string(error), "timeout") ->
                    "Request timed out. Please try again with a shorter request or reduce max_tokens."

                  String.contains?(to_string(error), "rate_limit") ->
                    "Rate limit exceeded. Please wait and try again."

                  String.contains?(to_string(error), "authentication") or
                      String.contains?(to_string(error), "api_key") ->
                    "Authentication failed. Please check your API key configuration."

                  true ->
                    "LLM execution failed: #{inspect(error)}"
                end

              conn
              |> put_resp_content_type("application/json")
              |> send_resp(
                500,
                Jason.encode!(%{
                  "type" => "error",
                  "error" => %{
                    "type" => "invalid_request_error",
                    "message" => error_message
                  }
                })
              )
          end
        end
    end
  end

  # Add helper function to get API key based on model/provider
  defp get_api_key_for_model(_model) do
    Application.get_env(:exhub, :giteeai_api_key, "")
  end

  # Add helper function to send SSE events
  defp send_sse_event(conn, event_type, data) do
    {:ok, conn} = chunk(conn, "event: #{event_type}\ndata: #{Jason.encode!(data)}\n\n")
    conn
  end

  # Add helper function to estimate input tokens
  defp estimate_input_tokens(messages, system, tools) do
    # Ensure messages and tools are lists (not nil/null)
    messages = if is_list(messages), do: messages, else: []
    tools = if is_list(tools), do: tools, else: []

    # Rough estimation: count characters and divide by 4
    message_tokens =
      Enum.reduce(messages, 0, fn msg, acc ->
        content = Map.get(msg, "content", "")
        content_str = if is_binary(content), do: content, else: inspect(content)
        acc + div(String.length(content_str), 4)
      end)

    system_tokens =
      if system do
        system_str = if is_binary(system), do: system, else: inspect(system)
        div(String.length(system_str), 4)
      else
        0
      end

    tool_tokens =
      Enum.reduce(tools, 0, fn tool, acc ->
        acc + div(String.length(Map.get(tool, "name", "")), 4) +
          div(String.length(inspect(Map.get(tool, "input_schema", ""))), 4)
      end)

    message_tokens + system_tokens + tool_tokens
  end

  # Add helper function to estimate output tokens
  defp estimate_output_tokens(response) when is_binary(response) do
    div(String.length(response), 4)
  end

  defp estimate_output_tokens(_), do: 0

  # Streaming response loop - processes events in main process
  defp stream_response_loop(conn, task, response_id, tracking_ctx, acc_text) do
    receive do
      {:stream_chunk, ^response_id, event} ->
        delta_data = Map.get(event, "delta", %{})
        delta_type = Map.get(delta_data, "type", "text_delta")

        # Accumulate text for token estimation
        new_acc_text =
          case delta_type do
            "text_delta" ->
              text = Map.get(delta_data, "text", "")
              acc_text <> text

            _ ->
              acc_text
          end

        conn =
          case delta_type do
            "text_delta" ->
              text_content = Map.get(delta_data, "text")

              text_content =
                case text_content do
                  nil -> nil
                  "" -> nil
                  str when is_binary(str) -> str
                  _ -> nil
                end

              if text_content != nil and text_content != "" do
                content_delta = %{
                  "type" => "content_block_delta",
                  "index" => 0,
                  "delta" => %{
                    "type" => "text_delta",
                    "text" => text_content
                  }
                }

                send_sse_event(conn, "content_block_delta", content_delta)
              else
                conn
              end

            "input_json_delta" ->
              partial_json = Map.get(delta_data, "partial_json", "")

              content_delta = %{
                "type" => "content_block_delta",
                "index" => 0,
                "delta" => %{
                  "type" => "input_json_delta",
                  "partial_json" => partial_json
                }
              }

              send_sse_event(conn, "content_block_delta", content_delta)

            "thinking_delta" ->
              thinking_content = Map.get(delta_data, "thinking", "")

              content_delta = %{
                "type" => "content_block_delta",
                "index" => 0,
                "delta" => %{
                  "type" => "thinking_delta",
                  "thinking" => thinking_content
                }
              }

              send_sse_event(conn, "content_block_delta", content_delta)

            "signature_delta" ->
              signature = Map.get(delta_data, "signature", "")

              content_delta = %{
                "type" => "content_block_delta",
                "index" => 0,
                "delta" => %{
                  "type" => "signature_delta",
                  "signature" => signature
                }
              }

              send_sse_event(conn, "content_block_delta", content_delta)

            _ ->
              send_sse_event(conn, "content_block_delta", event)
          end

        stream_response_loop(conn, task, response_id, tracking_ctx, new_acc_text)

      {:message_complete, ^response_id, _message} ->
        # Track token usage for streaming response
        if map_size(tracking_ctx) > 0 do
          output_tokens = estimate_output_tokens(acc_text)

          spawn(fn ->
            Exhub.TokenUsage.TokenUsageStore.record_usage(
              tracking_ctx[:model] || "unknown",
              tracking_ctx[:provider] || "exhub",
              tracking_ctx[:input_tokens] || 0,
              output_tokens,
              %{request_id: response_id, timestamp: DateTime.utc_now()}
            )
          end)
        end

        conn =
          send_sse_event(conn, "content_block_stop", %{
            "type" => "content_block_stop",
            "index" => 0
          })

        message_delta = %{
          "type" => "message_delta",
          "delta" => %{
            "stop_reason" => "end_turn",
            "stop_sequence" => nil
          },
          "usage" => %{
            "output_tokens" => estimate_output_tokens(acc_text)
          }
        }

        conn = send_sse_event(conn, "message_delta", message_delta)
        conn = send_sse_event(conn, "message_stop", %{"type" => "message_stop"})

        {:ok, conn} = chunk(conn, "data: [DONE]\n\n")

        Task.await(task)

        conn

      {:error, ^response_id, error} ->
        Logger.error(
          "stream_response_loop: Received error for #{response_id} - #{inspect(error)}"
        )

        error_message = inspect(error)

        error_event = %{
          "type" => "error",
          "error" => %{
            "type" => "internal_server_error",
            "message" => error_message
          }
        }

        conn = send_sse_event(conn, "error", error_event)
        conn = send_sse_event(conn, "message_stop", %{"type" => "message_stop"})

        {:ok, conn} = chunk(conn, "data: [DONE]\n\n")

        Task.await(task)

        conn

      {:DOWN, _ref, :process, _pid, :normal} ->
        conn

      {:DOWN, _ref, :process, _pid, reason} ->
        Logger.error(
          "stream_response_loop: Task DOWN (crashed) for #{response_id} - reason: #{inspect(reason)}"
        )

        error_event = %{
          "type" => "error",
          "error" => %{
            "type" => "internal_server_error",
            "message" => "Stream processing failed: #{inspect(reason)}"
          }
        }

        conn = send_sse_event(conn, "error", error_event)
        conn = send_sse_event(conn, "message_stop", %{"type" => "message_stop"})

        {:ok, conn} = chunk(conn, "data: [DONE]\n\n")

        conn
    end
  end

  post "/v1/messages/count_tokens" do
    # Parse the request body
    body = conn.body_params

    # Extract required fields
    _model = Map.get(body, "model")
    messages = Map.get(body, "messages", [])
    tools = Map.get(body, "tools", [])

    # Simple token estimation: count characters and divide by 4 (approximate)
    # This is a rough approximation - in production, use a proper tokenizer
    total_tokens =
      Enum.reduce(messages, 0, fn msg, acc ->
        content = Map.get(msg, "content")

        case content do
          nil ->
            acc

          str when is_binary(str) ->
            acc + div(String.length(str), 4)

          list when is_list(list) ->
            Enum.reduce(list, acc, fn item, acc2 ->
              case item do
                %{"type" => "text", "text" => text} ->
                  acc2 + div(String.length(text), 4)

                %{"type" => "tool_use", "name" => name, "input" => input} ->
                  acc2 + div(String.length(name), 4) + div(String.length(inspect(input)), 4)

                %{"type" => "tool_result", "tool_use_id" => tool_id, "content" => content} ->
                  acc2 + div(String.length(tool_id), 4) + div(String.length(inspect(content)), 4)

                _ ->
                  acc2 + div(String.length(inspect(item)), 4)
              end
            end)

          _ ->
            acc + div(String.length(inspect(content)), 4)
        end
      end)

    # Add tokens for tools
    tool_tokens =
      Enum.reduce(tools, 0, fn tool, acc ->
        acc + div(String.length(Map.get(tool, "name", "")), 4) +
          div(String.length(inspect(Map.get(tool, "input_schema", ""))), 4)
      end)

    total_tokens = total_tokens + tool_tokens

    # Return Anthropic format response
    token_count_response = %{
      input_tokens: total_tokens
    }

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Jason.encode!(token_count_response))
  end

  # MCP endpoint for habit configuration
  forward("/mcp",
    to: Hermes.Server.Transport.StreamableHTTP.Plug,
    init_opts: [server: Exhub.MCP.HabitServer]
  )

  # Token Usage Dashboard API
  get "/dashboard/token_usage" do
    # Parse query parameters
    filters =
      %{
        start_date: conn.params["start_date"],
        end_date: conn.params["end_date"],
        model: conn.params["model"],
        provider: conn.params["provider"],
        limit: parse_int(conn.params["limit"], 100),
        offset: parse_int(conn.params["offset"], 0)
      }
      |> Enum.reject(fn {_k, v} -> is_nil(v) end)
      |> Map.new()

    case Exhub.TokenUsage.TokenUsageStore.get_usage(filters) do
      {:ok, records} ->
        response = %{
          success: true,
          data: records,
          pagination: %{
            limit: filters[:limit] || 100,
            offset: filters[:offset] || 0,
            total: length(records)
          }
        }

        conn
        |> put_resp_content_type("application/json")
        |> send_resp(200, Jason.encode!(response))

      {:error, reason} ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(500, Jason.encode!(%{success: false, error: inspect(reason)}))
    end
  end

  get "/dashboard/stats" do
    # Parse query parameters
    start_date = conn.params["start_date"]
    end_date = conn.params["end_date"]
    group_by = conn.params["group_by"] || "model"

    filters =
      %{}
      |> maybe_put(:start_date, start_date)
      |> maybe_put(:end_date, end_date)

    group_by_atom =
      case group_by do
        "provider" -> :provider
        "day" -> :day
        _ -> :model
      end

    with {:ok, grouped_stats} <-
           Exhub.TokenUsage.TokenUsageStore.get_stats(group_by_atom, filters),
         {:ok, summary} <- Exhub.TokenUsage.TokenUsageStore.get_summary(filters) do
      response = %{
        success: true,
        data: %{
          summary: summary,
          grouped: grouped_stats,
          group_by: group_by
        }
      }

      conn
      |> put_resp_content_type("application/json")
      |> send_resp(200, Jason.encode!(response))
    else
      {:error, reason} ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(500, Jason.encode!(%{success: false, error: inspect(reason)}))
    end
  end

  get "/dashboard/data" do
    days = parse_int(conn.params["days"], 30)

    case Exhub.TokenUsage.TokenUsageStats.get_dashboard_data(days) do
      {:ok, data} ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(200, Jason.encode!(%{success: true, data: data}))

      {:error, reason} ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(500, Jason.encode!(%{success: false, error: inspect(reason)}))
    end
  end

  get "/dashboard" do
    dashboard_html = """
    <!DOCTYPE html>
    <html lang="en">
    <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>Exhub Token Usage Dashboard</title>
      <style>
        * { box-sizing: border-box; margin: 0; padding: 0; }
        body {
          font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif;
          background: #0d1117;
          color: #c9d1d9;
          line-height: 1.6;
        }
        .container {
          max-width: 1400px;
          margin: 0 auto;
          padding: 20px;
        }
        header {
          background: #161b22;
          border-bottom: 1px solid #30363d;
          padding: 20px 0;
          margin-bottom: 30px;
        }
        h1 {
          color: #58a6ff;
          font-size: 28px;
        }
        .subtitle {
          color: #8b949e;
          font-size: 14px;
        }
        .stats-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
          gap: 20px;
          margin-bottom: 30px;
        }
        .stat-card {
          background: #161b22;
          border: 1px solid #30363d;
          border-radius: 12px;
          padding: 24px;
          transition: transform 0.2s, box-shadow 0.2s;
        }
        .stat-card:hover {
          transform: translateY(-2px);
          box-shadow: 0 8px 24px rgba(0,0,0,0.3);
        }
        .stat-label {
          font-size: 12px;
          text-transform: uppercase;
          letter-spacing: 0.5px;
          color: #8b949e;
          margin-bottom: 8px;
        }
        .stat-value {
          font-size: 32px;
          font-weight: 700;
          color: #58a6ff;
        }
        .stat-value.cost {
          color: #238636;
        }
        .section {
          background: #161b22;
          border: 1px solid #30363d;
          border-radius: 12px;
          padding: 24px;
          margin-bottom: 24px;
        }
        .section h2 {
          font-size: 18px;
          margin-bottom: 20px;
          color: #f0f6fc;
        }
        table {
          width: 100%;
          border-collapse: collapse;
        }
        th, td {
          text-align: left;
          padding: 12px;
          border-bottom: 1px solid #30363d;
        }
        th {
          font-weight: 600;
          color: #8b949e;
          font-size: 12px;
          text-transform: uppercase;
        }
        tr:hover {
          background: #21262d;
        }
        .loading {
          text-align: center;
          padding: 40px;
          color: #8b949e;
        }
        .error {
          background: #da3633;
          color: white;
          padding: 16px;
          border-radius: 8px;
          margin-bottom: 20px;
        }
        .refresh-btn {
          background: #238636;
          color: white;
          border: none;
          padding: 8px 16px;
          border-radius: 6px;
          cursor: pointer;
          font-size: 14px;
          margin-bottom: 20px;
        }
        .refresh-btn:hover {
          background: #2ea043;
        }
        .chart-container {
          height: 300px;
          position: relative;
        }
        .bar-chart {
          display: flex;
          align-items: flex-end;
          justify-content: space-around;
          height: 250px;
          padding: 20px 0;
          border-bottom: 2px solid #30363d;
        }
        .bar {
          background: linear-gradient(to top, #58a6ff, #79c0ff);
          border-radius: 4px 4px 0 0;
          min-width: 30px;
          position: relative;
          transition: opacity 0.2s;
        }
        .bar:hover {
          opacity: 0.8;
        }
        .bar-label {
          position: absolute;
          bottom: -25px;
          left: 50%;
          transform: translateX(-50%);
          font-size: 11px;
          color: #8b949e;
          white-space: nowrap;
        }
        .bar-value {
          position: absolute;
          top: -20px;
          left: 50%;
          transform: translateX(-50%);
          font-size: 11px;
          color: #c9d1d9;
        }
      </style>
    </head>
    <body>
      <header>
        <div class="container">
          <h1>Token Usage Dashboard</h1>
          <p class="subtitle">Monitor your LLM API usage and costs</p>
        </div>
      </header>

      <div class="container">
        <button class="refresh-btn" onclick="loadDashboard()">Refresh Data</button>
        <div id="error"></div>

        <div class="stats-grid" id="stats-grid">
          <div class="stat-card">
            <div class="stat-label">Total Requests</div>
            <div class="stat-value" id="total-requests">-</div>
          </div>
          <div class="stat-card">
            <div class="stat-label">Total Tokens</div>
            <div class="stat-value" id="total-tokens">-</div>
          </div>
          <div class="stat-card">
            <div class="stat-label">Input Tokens</div>
            <div class="stat-value" id="input-tokens">-</div>
          </div>
          <div class="stat-card">
            <div class="stat-label">Output Tokens</div>
            <div class="stat-value" id="output-tokens">-</div>
          </div>
          <div class="stat-card">
            <div class="stat-label">Estimated Cost</div>
            <div class="stat-value cost" id="total-cost">-</div>
          </div>
          <div class="stat-card">
            <div class="stat-label">Unique Models</div>
            <div class="stat-value" id="unique-models">-</div>
          </div>
        </div>

        <div class="section">
          <h2>Usage Trends (Last 30 Days)</h2>
          <div class="chart-container">
            <div class="bar-chart" id="trends-chart">
              <div class="loading">Loading...</div>
            </div>
          </div>
        </div>

        <div class="section">
          <h2>Top Models</h2>
          <table>
            <thead>
              <tr>
                <th>Model</th>
                <th>Requests</th>
                <th>Input Tokens</th>
                <th>Output Tokens</th>
                <th>Total Tokens</th>
                <th>Cost (USD)</th>
                <th>Percentage</th>
              </tr>
            </thead>
            <tbody id="top-models">
              <tr><td colspan="7" class="loading">Loading...</td></tr>
            </tbody>
          </table>
        </div>

        <div class="section">
          <h2>Recent Usage</h2>
          <table>
            <thead>
              <tr>
                <th>Timestamp</th>
                <th>Model</th>
                <th>Provider</th>
                <th>Input Tokens</th>
                <th>Output Tokens</th>
                <th>Total</th>
                <th>Cost (USD)</th>
              </tr>
            </thead>
            <tbody id="recent-usage">
              <tr><td colspan="7" class="loading">Loading...</td></tr>
            </tbody>
          </table>
        </div>
      </div>

      <script>
        function formatNumber(num) {
          if (num >= 1000000) return (num / 1000000).toFixed(1) + 'M';
          if (num >= 1000) return (num / 1000).toFixed(1) + 'K';
          return num.toString();
        }

        function formatCurrency(num) {
          return '$' + num.toFixed(4);
        }

        function formatDate(isoString) {
          const date = new Date(isoString);
          return date.toLocaleString();
        }

        async function loadDashboard() {
          try {
            document.getElementById('error').innerHTML = '';

            const response = await fetch('/dashboard/data?days=30');
            const result = await response.json();

            if (!result.success) {
              throw new Error(result.error || 'Failed to load data');
            }

            const data = result.data;

            // Update summary stats
            const summary = data.summary;
            document.getElementById('total-requests').textContent = formatNumber(summary.total_requests);
            document.getElementById('total-tokens').textContent = formatNumber(summary.total_tokens);
            document.getElementById('input-tokens').textContent = formatNumber(summary.total_input_tokens);
            document.getElementById('output-tokens').textContent = formatNumber(summary.total_output_tokens);
            document.getElementById('total-cost').textContent = formatCurrency(summary.total_cost);
            document.getElementById('unique-models').textContent = summary.unique_models_count;

            // Update trends chart
            const trendsHtml = data.trends.map(day => {
              const maxTokens = Math.max(...data.trends.map(d => d.total_tokens), 1);
              const height = (day.total_tokens / maxTokens * 200);
              return `
                <div class="bar" style="height: ${height}px">
                  <div class="bar-value">${formatNumber(day.total_tokens)}</div>
                  <div class="bar-label">${day.date.slice(5)}</div>
                </div>
              `;
            }).join('');
            document.getElementById('trends-chart').innerHTML = trendsHtml || '<div class="loading">No data</div>';

            // Update top models table
            const modelsHtml = data.top_models.map(model => `
              <tr>
                <td>${model.model}</td>
                <td>${formatNumber(model.request_count)}</td>
                <td>${formatNumber(model.total_input)}</td>
                <td>${formatNumber(model.total_output)}</td>
                <td>${formatNumber(model.total_tokens)}</td>
                <td>${formatCurrency(model.total_cost)}</td>
                <td>${model.percentage}%</td>
              </tr>
            `).join('');
            document.getElementById('top-models').innerHTML = modelsHtml || '<tr><td colspan="7" class="loading">No data</td></tr>';

            // Update recent usage table
            const recentHtml = data.recent_usage.map(usage => `
              <tr>
                <td>${formatDate(usage.timestamp)}</td>
                <td>${usage.model}</td>
                <td>${usage.provider}</td>
                <td>${formatNumber(usage.input_tokens)}</td>
                <td>${formatNumber(usage.output_tokens)}</td>
                <td>${formatNumber(usage.total_tokens)}</td>
                <td>${formatCurrency(usage.estimated_cost)}</td>
              </tr>
            `).join('');
            document.getElementById('recent-usage').innerHTML = recentHtml || '<tr><td colspan="7" class="loading">No data</td></tr>';

          } catch (err) {
            document.getElementById('error').innerHTML = `<div class="error">Error: ${err.message}</div>`;
            console.error('Dashboard error:', err);
          }
        }

        // Load dashboard on page load
        loadDashboard();

        // Auto-refresh every 60 seconds
        setInterval(loadDashboard, 60000);
      </script>
    </body>
    </html>
    """

    conn
    |> put_resp_content_type("text/html")
    |> send_resp(200, dashboard_html)
  end

  # Helper functions
  defp parse_int(nil, default), do: default

  defp parse_int(value, default) when is_binary(value) do
    case Integer.parse(value) do
      {int, _} -> int
      :error -> default
    end
  end

  defp parse_int(value, _default) when is_integer(value), do: value
  defp parse_int(_, default), do: default

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)

  match _ do
    send_resp(
      conn,
      200,
      "<html><head></head><body>Connect to WS enpoint at \"/exhub\"</body></html>"
    )
  end
end
