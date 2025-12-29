defmodule Exhub.Router do
  alias Exhub.ProxyPlug
  require Logger
  use Plug.Router
  use PlugSocket
  alias UUID
  alias LangChain.MessageDelta

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
      "deepseek-v3" => "https://ai.gitee.com/v1",
      "deepseek-r1" => "https://ai.gitee.com/v1",
      "deepseek-v3_1" => "https://ai.gitee.com/v1",
      "deepseek-v3_1-terminus" => "https://ai.gitee.com/v1",
      "deepseek-v3.2" => "https://ai.gitee.com/v1",
      "deepseek-v3.2-exp" => "https://ai.gitee.com/v1",
      "gpt-oss-120b" => "https://ai.gitee.com/v1",
      "internvl3-78b" => "https://ai.gitee.com/v1",
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
      "minimax-m2.1" => "https://api.minimaxi.com/v1",
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
      "deepseek-v3" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "deepseek-r1" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "deepseek-v3_1" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "deepseek-v3_1-terminus" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "deepseek-v3.2" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "deepseek-v3.2-exp" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "gpt-oss-120b" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "internvl3-78b" => Application.get_env(:exhub, :giteeai_api_key, ""),
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
      "minimax-m2.1" => Application.get_env(:exhub, :minimax_api_key, ""),
      "minimax-m2-preview" => Application.get_env(:exhub, :minimax_api_key, ""),
      "openrouter/polaris-alpha" => Application.get_env(:exhub, :openrouter_api_key, "")
    }

    target_url = Map.get(model_target_map, model_name, "http://localhost:4444/v1")

    token = Map.get(model_token_map, model_name, Application.get_env(:exhub, :openai_api_key, ""))

    options = [
      custom_headers: [{"Authorization", "Bearer #{token}"}],
      client_options: [timeout: @default_timeout, recv_timeout: @default_timeout]
    ]

    ProxyPlug.forward_upstream(
      conn,
      target_url,
      options
    )
  end

  post "/anthropic/v1/*path" do
    ProxyPlug.forward_upstream(conn, "https://api.anthropic.com/v1",
      client_options: [proxy: @proxy]
    )
  end

  defp convert_anthropic_to_langchain(anthropic_request) do
    # Extract messages and system from the request
    messages = Map.get(anthropic_request, "messages", [])
    system = Map.get(anthropic_request, "system")
    tools = Map.get(anthropic_request, "tools", [])
    tool_choice = Map.get(anthropic_request, "tool_choice")

    # Convert system message if present
    langchain_system =
      if system do
        case system do
          str when is_binary(str) ->
            [LangChain.Message.ContentPart.text!(str)]

          list when is_list(list) ->
            Enum.map(list, fn block ->
              case block do
                %{"type" => "text", "text" => text} ->
                  LangChain.Message.ContentPart.text!(text)

                _ ->
                  LangChain.Message.ContentPart.text!(inspect(block))
              end
            end)

          _ ->
            nil
        end
      else
        nil
      end

    # Convert each Anthropic message to LangChain format
    converted_messages =
      Enum.flat_map(messages, fn msg ->
        role = Map.get(msg, "role", "user")
        content = Map.get(msg, "content", "")

        # Handle content that is a list of content blocks
        blocks =
          if is_list(content) do
            content
          else
            [%{"type" => "text", "text" => content}]
          end

        # First pass: collect all tool_result blocks
        tool_result_blocks =
          Enum.filter(blocks, fn block ->
            Map.get(block, "type") == "tool_result"
          end)

        # Convert content blocks to LangChain format using proper struct creation
        converted_content =
          Enum.map(blocks, fn block ->
            block_type = Map.get(block, "type")

            case block_type do
              "text" ->
                text = Map.get(block, "text", "")
                LangChain.Message.ContentPart.text!(text)

              "image" ->
                # Handle image content blocks
                source = Map.get(block, "source", %{})

                media_type =
                  case source do
                    %{"type" => "base64", "media_type" => media_type} -> media_type
                    %{"media_type" => media_type} -> media_type
                    # Default to PNG if not specified
                    _ -> "image/png"
                  end

                # For images, we need to handle them properly based on source type
                if Map.get(source, "type") == "base64" do
                  data = Map.get(source, "data", "")
                  LangChain.Message.ContentPart.image!(data, media: media_type)
                else
                  # For URL-based images, create a text representation
                  text = "Image (url: #{inspect(source)})"
                  LangChain.Message.ContentPart.text!(text)
                end

              "tool_use" ->
                # For tool_use, create a ToolCall struct using proper constructor
                id = Map.get(block, "id", "")
                name = Map.get(block, "name", "")
                input = Map.get(block, "input", %{})

                LangChain.Message.ToolCall.new!(%{
                  call_id: id,
                  name: name,
                  arguments: input
                })

              "tool_result" ->
                # Tool results are handled separately as dedicated messages
                nil

              _ ->
                # Unknown block type, convert to text
                text = inspect(block)
                LangChain.Message.ContentPart.text!(text)
            end
          end)
          |> Enum.filter(&(&1 != nil))

        # Build the message based on role using documented API patterns
        base_msg =
          case role do
            "system" ->
              {:ok, msg} =
                LangChain.Message.new(%{
                  role: :system,
                  content: converted_content,
                  status: :complete
                })

              msg

            "assistant" ->
              # Check for tool calls in the blocks
              tool_calls =
                Enum.filter(converted_content, fn item ->
                  match?(%LangChain.Message.ToolCall{}, item)
                end)

              # Filter out tool_calls from content, keep only ContentParts
              content_parts =
                Enum.filter(converted_content, fn item ->
                  not match?(%LangChain.Message.ToolCall{}, item)
                end)

              {:ok, msg} =
                LangChain.Message.new(%{
                  role: :assistant,
                  content: content_parts,
                  tool_calls: tool_calls,
                  status: :complete
                })

              msg

            "user" ->
              {:ok, msg} =
                LangChain.Message.new(%{
                  role: :user,
                  content: converted_content,
                  status: :complete
                })

              msg

            "tool" ->
              # Handle tool result messages using ToolResult.new!/1
              tool_results =
                Enum.map(tool_result_blocks, fn block ->
                  tool_use_id = Map.get(block, "tool_use_id", "")
                  result_content = Map.get(block, "content", "")

                  # Determine if it's an error by checking content
                  is_error =
                    case result_content do
                      str when is_binary(str) ->
                        String.downcase(str) =~ ~r/error|fail|exception/i

                      list when is_list(list) ->
                        Enum.any?(list, fn item ->
                          case item do
                            %{"type" => "text", "text" => text} ->
                              String.downcase(text) =~ ~r/error|fail|exception/i

                            _ ->
                              false
                          end
                        end)

                      _ ->
                        false
                    end

                  # Create ToolResult using proper constructor
                  LangChain.Message.ToolResult.new!(%{
                    call_id: tool_use_id,
                    content: result_content,
                    is_error: is_error
                  })
                end)

              {:ok, msg} =
                LangChain.Message.new_tool_result(%{
                  tool_results: tool_results,
                  content: nil
                })

              msg

            _ ->
              # Default to user role for unknown roles
              {:ok, msg} =
                LangChain.Message.new(%{
                  role: :user,
                  content: converted_content,
                  status: :complete
                })

              msg
          end

        # If there are tool_result blocks in user messages, create a tool message after this one
        # This handles the case where tool results are embedded in user messages
        tool_messages =
          if tool_result_blocks != [] and role == "user" do
            tool_results =
              Enum.map(tool_result_blocks, fn block ->
                tool_use_id = Map.get(block, "tool_use_id", "")
                result_content = Map.get(block, "content", "")

                # Determine if it's an error
                is_error =
                  case result_content do
                    str when is_binary(str) ->
                      String.downcase(str) =~ ~r/error|fail|exception/i

                    list when is_list(list) ->
                      Enum.any?(list, fn item ->
                        case item do
                          %{"type" => "text", "text" => text} ->
                            String.downcase(text) =~ ~r/error|fail|exception/i

                          _ ->
                            false
                        end
                      end)

                    _ ->
                      false
                  end

                # Create ToolResult using proper constructor
                LangChain.Message.ToolResult.new!(%{
                  call_id: tool_use_id,
                  content: result_content,
                  is_error: is_error
                })
              end)

            {:ok, tool_msg} =
              LangChain.Message.new_tool_result(%{
                tool_results: tool_results,
                content: nil
              })

            [tool_msg]
          else
            []
          end

        [base_msg | tool_messages]
      end)

    # Reverse since we built the list backwards
    converted_messages = Enum.reverse(converted_messages)

    # Convert tools from Anthropic format to LangChain Function format
    # See LangChain.Function documentation for the proper structure
    converted_tools =
      Enum.map(tools, fn tool ->
        name = Map.get(tool, "name", "")
        description = Map.get(tool, "description", "")
        input_schema = Map.get(tool, "input_schema", %{})

        # Return as a map that can be used with Function.new!/1
        %{
          name: name,
          description: description,
          parameters_schema: input_schema
        }
      end)

    %{
      messages: converted_messages,
      system: langchain_system,
      tools: converted_tools,
      tool_choice: tool_choice
    }
  end

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
    langchain_system = Map.get(langchain_request, :system)
    langchain_tools = Map.get(langchain_request, :tools, []) || []

    # Convert tools to LangChain Function format
    converted_tools =
      Enum.map(langchain_tools || [], fn tool ->
        name = Map.get(tool, :name, "") || Map.get(tool, "name", "")
        description = Map.get(tool, :description, "") || Map.get(tool, "description", "")

        parameters_schema =
          Map.get(tool, :parameters_schema, %{}) || Map.get(tool, "parameters_schema", %{})

        # Return as a map that can be used with Function.new!/1
        %{
          name: name,
          description: description,
          parameters_schema: parameters_schema
        }
      end)

    # Prepare custom context for tools
    custom_context = %{}

    # Get LLM chain - need to convert model name to LLM name format
    llm_name = get_llm_name_from_model(model)
    api_key = get_api_key_for_model(model)

    # Create chain with API key if available
    chain_options = %{stream: stream}

    chain_options =
      if api_key != "", do: Map.put(chain_options, :api_key, api_key), else: chain_options

    case Exhub.Llm.Chain.create_llm_chain(llm_name, chain_options) do
      llm_chain ->
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

          # Log the input message for this response
          last_user_msg =
            Enum.find_value(Enum.reverse(messages), fn
              %{"role" => "user", "content" => content} -> content
              _ -> nil
            end)

          input_preview =
            if is_binary(last_user_msg),
              do: String.slice(last_user_msg, 0, 200),
              else: inspect(last_user_msg)

          Logger.info("[#{response_id}] Input: #{input_preview}")

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
                  langchain_messages,
                  converted_tools,
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
                 langchain_messages,
                 converted_tools,
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

      {:error, reason} ->
        Logger.error(
          "Failed to create LLM chain for model '#{model}' (llm_name: #{llm_name}): #{inspect(reason)}"
        )

        # Provide more helpful error messages based on common issues
        error_message =
          cond do
            String.contains?(to_string(reason), "not found") or
              String.contains?(to_string(reason), "unknown") or
                String.contains?(to_string(reason), "not supported") ->
              "Model '#{model}' is not supported or unknown. Please check the model name and try again."

            String.contains?(to_string(reason), "timeout") ->
              "Request timed out. Please try again with a shorter request or reduce max_tokens."

            String.contains?(to_string(reason), "rate_limit") ->
              "Rate limit exceeded. Please wait and try again."

            String.contains?(to_string(reason), "authentication") or
              String.contains?(to_string(reason), "api_key") or
                String.contains?(to_string(reason), "unauthorized") ->
              "Authentication failed. Please check your API key configuration for #{model}."

            String.contains?(to_string(reason), "connection") or
                String.contains?(to_string(reason), "network") ->
              "Unable to connect to the model provider. Please check your network connection."

            true ->
              "Failed to process request for model '#{model}': #{inspect(reason)}"
          end

        # Determine appropriate HTTP status code
        status_code =
          cond do
            String.contains?(to_string(reason), "not found") -> 404
            String.contains?(to_string(reason), "authentication") -> 401
            String.contains?(to_string(reason), "rate_limit") -> 429
            String.contains?(to_string(reason), "timeout") -> 408
            true -> 400
          end

        conn
        |> put_resp_content_type("application/json")
        |> send_resp(
          status_code,
          Jason.encode!(%{
            "type" => "error",
            "error" => %{
              "type" => determine_error_type(reason),
              "message" => error_message
            }
          })
        )
    end
  end

  # Add helper function to map model to LLM name with proper provider prefix handling
  defp get_llm_name_from_model(model) do
    "openai/kimi-k2-instruct"
  end

  # Add helper function to get API key based on model/provider
  defp get_api_key_for_model(model) do
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
    model = Map.get(body, "model")
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

  match _ do
    send_resp(
      conn,
      200,
      "<html><head></head><body>Connect to WS enpoint at \"/exhub\"</body></html>"
    )
  end
end
