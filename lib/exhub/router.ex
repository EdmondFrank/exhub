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
      "qwen3-235b-a22b" => "https://ai.gitee.com/v1",
      "qwen3-235b-a22b-instruct-2507" => "https://ai.gitee.com/v1",
      "qwen3-next-80b-a3b-instruct" => "https://ai.gitee.com/v1",
      "qwen3-next-80b-a3b-thinking" => "https://ai.gitee.com/v1",
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
      "qwen3-235b-a22b" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "qwen3-235b-a22b-instruct-2507" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "qwen3-next-80b-a3b-instruct" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "qwen3-next-80b-a3b-thinking" => Application.get_env(:exhub, :giteeai_api_key, ""),
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

    # Convert to LangChain format using existing function
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
          stream_response_loop(conn, task, response_id)

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

  # Helper function to determine error type for Anthropic format
  defp determine_error_type(error) do
    error_str = to_string(error)

    cond do
      String.contains?(error_str, "timeout") ->
        "request_timeout"

      String.contains?(error_str, "rate_limit") ->
        "rate_limit_error"

      String.contains?(error_str, "authentication") or String.contains?(error_str, "api_key") or
          String.contains?(error_str, "unauthorized") ->
        "authentication_error"

      String.contains?(error_str, "not found") or String.contains?(error_str, "unknown model") or
          String.contains?(error_str, "model not found") ->
        "model_not_found_error"

      String.contains?(error_str, "invalid request") or String.contains?(error_str, "validation") ->
        "invalid_request_error"

      String.contains?(error_str, "permission") or String.contains?(error_str, "forbidden") ->
        "permission_error"

      String.contains?(error_str, "overloaded") ->
        "overloaded_error"

      true ->
        "invalid_request_error"
    end
  end

  # Streaming response loop - processes events in main process
  defp stream_response_loop(conn, task, response_id) do
    receive do
      {:stream_chunk, ^response_id, event} ->
        delta_data = Map.get(event, "delta", %{})
        delta_type = Map.get(delta_data, "type", "text_delta")

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

        stream_response_loop(conn, task, response_id)

      {:message_complete, ^response_id, _message} ->
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
            "output_tokens" => 0
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

        error_type = determine_error_type(error)
        error_message = inspect(error)

        error_event = %{
          "type" => "error",
          "error" => %{
            "type" => error_type,
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
  forward "/mcp", to: Hermes.Server.Transport.StreamableHTTP.Plug,
    init_opts: [server: Exhub.MCP.HabitServer]

  match _ do
    send_resp(
      conn,
      200,
      "<html><head></head><body>Connect to WS enpoint at \"/exhub\"</body></html>"
    )
  end
end
