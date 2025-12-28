defmodule Exhub.Router do
  alias Exhub.ProxyPlug
  require Logger
  use Plug.Router
  use PlugSocket
  alias UUID
  alias LangChain.MessageDelta

  @proxy Application.compile_env(:exhub, :proxy, "")
  @default_timeout Application.compile_env(:exhub, :default_timeout, 1_800_000)

  plug Plug.Parsers,
    parsers: [:urlencoded, {:json, json_decoder: Jason}],
    pass:  ["*/*"]

  socket "/exhub", Exhub.SocketHandler,
    websocket: [timeout: @default_timeout, recv_timeout: @default_timeout],
    longpoll: false

  plug :match
  plug :dispatch



  post "/groq/v1/*path" do
    ProxyPlug.forward_upstream(conn, "https://api.groq.com/openai/v1", client_options: [proxy: @proxy])
  end

  post "/google/v1/*path" do
    ProxyPlug.forward_upstream(conn, "https://generativelanguage.googleapis.com/", client_options: [proxy: @proxy])
  end

  post "/cohere/v1/*path" do
    ProxyPlug.forward_upstream(conn, "https://api.cohere.ai/compatibility/v1", client_options: [proxy: @proxy])
  end

  post "/samba/v1/*path" do
    ProxyPlug.forward_upstream(conn, "https://api.sambanova.ai/v1", client_options: [proxy: @proxy])
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
        _ -> nil
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
      "tngtech/deepseek-r1t2-chimera:free" => Application.get_env(:exhub, :openrouter_api_key, ""),
      "minimax/minimax-m2:free" => Application.get_env(:exhub, :openrouter_api_key, ""),
      "minimax-m2" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "minimax-m2.1" => Application.get_env(:exhub, :minimax_api_key, ""),
      "minimax-m2-preview" => Application.get_env(:exhub, :minimax_api_key, ""),
      "openrouter/polaris-alpha" => Application.get_env(:exhub, :openrouter_api_key, "")
    }

    target_url = Map.get(model_target_map, model_name, "http://localhost:4444/v1")

    token = Map.get(model_token_map, model_name, Application.get_env(:exhub, :openai_api_key, ""))

    options = [custom_headers: [{"Authorization", "Bearer #{token}"}], client_options: [timeout: @default_timeout, recv_timeout: @default_timeout]]

    ProxyPlug.forward_upstream(
      conn,
      target_url,
      options
    )
  end

  post "/anthropic/v1/*path" do
    ProxyPlug.forward_upstream(conn, "https://api.anthropic.com/v1", client_options: [proxy: @proxy])
  end

  defp convert_anthropic_to_langchain(anthropic_request) do
    # Extract messages and system from the request
    messages = Map.get(anthropic_request, "messages", [])
    system = Map.get(anthropic_request, "system")
    tools = Map.get(anthropic_request, "tools", [])
    tool_choice = Map.get(anthropic_request, "tool_choice")

    # Convert system message if present
    langchain_system = if system do
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
    converted_messages = Enum.flat_map(messages, fn msg ->
      role = Map.get(msg, "role", "user")
      content = Map.get(msg, "content", "")

      # Handle content that is a list of content blocks
      blocks = if is_list(content) do
        content
      else
        [%{"type" => "text", "text" => content}]
      end

      # First pass: collect all tool_result blocks
      tool_result_blocks = Enum.filter(blocks, fn block ->
        Map.get(block, "type") == "tool_result"
      end)

      # Convert content blocks to LangChain format using proper struct creation
      converted_content = Enum.map(blocks, fn block ->
        block_type = Map.get(block, "type")
        case block_type do
          "text" ->
            text = Map.get(block, "text", "")
            LangChain.Message.ContentPart.text!(text)

          "image" ->
            # Handle image content blocks
            source = Map.get(block, "source", %{})
            media_type = case source do
              %{"type" => "base64", "media_type" => media_type} -> media_type
              %{"media_type" => media_type} -> media_type
              _ -> "image/png"  # Default to PNG if not specified
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
      end) |> Enum.filter(&(&1 != nil))

      # Build the message based on role using documented API patterns
      base_msg = case role do
        "system" ->
          {:ok, msg} = LangChain.Message.new(%{
            role: :system,
            content: converted_content,
            status: :complete
          })
          msg

        "assistant" ->
          # Check for tool calls in the blocks
          tool_calls = Enum.filter(converted_content, fn item ->
            match?(%LangChain.Message.ToolCall{}, item)
          end)

          # Filter out tool_calls from content, keep only ContentParts
          content_parts = Enum.filter(converted_content, fn item ->
            not match?(%LangChain.Message.ToolCall{}, item)
          end)

          {:ok, msg} = LangChain.Message.new(%{
            role: :assistant,
            content: content_parts,
            tool_calls: tool_calls,
            status: :complete
          })
          msg

        "user" ->
          {:ok, msg} = LangChain.Message.new(%{
            role: :user,
            content: converted_content,
            status: :complete
          })
          msg

        "tool" ->
          # Handle tool result messages using ToolResult.new!/1
          tool_results = Enum.map(tool_result_blocks, fn block ->
            tool_use_id = Map.get(block, "tool_use_id", "")
            result_content = Map.get(block, "content", "")

            # Determine if it's an error by checking content
            is_error = case result_content do
              str when is_binary(str) ->
                String.downcase(str) =~ ~r/error|fail|exception/i
              list when is_list(list) ->
                Enum.any?(list, fn item ->
                  case item do
                    %{"type" => "text", "text" => text} ->
                      String.downcase(text) =~ ~r/error|fail|exception/i
                    _ -> false
                  end
                end)
              _ -> false
            end

            # Create ToolResult using proper constructor
            LangChain.Message.ToolResult.new!(%{
              call_id: tool_use_id,
              content: result_content,
              is_error: is_error
            })
          end)

          {:ok, msg} = LangChain.Message.new_tool_result(%{
            tool_results: tool_results,
            content: nil
          })
          msg

        _ ->
          # Default to user role for unknown roles
          {:ok, msg} = LangChain.Message.new(%{
            role: :user,
            content: converted_content,
            status: :complete
          })
          msg
      end

      # If there are tool_result blocks in user messages, create a tool message after this one
      # This handles the case where tool results are embedded in user messages
      tool_messages = if tool_result_blocks != [] and role == "user" do
        tool_results = Enum.map(tool_result_blocks, fn block ->
          tool_use_id = Map.get(block, "tool_use_id", "")
          result_content = Map.get(block, "content", "")

          # Determine if it's an error
          is_error = case result_content do
            str when is_binary(str) ->
              String.downcase(str) =~ ~r/error|fail|exception/i
            list when is_list(list) ->
              Enum.any?(list, fn item ->
                case item do
                  %{"type" => "text", "text" => text} ->
                    String.downcase(text) =~ ~r/error|fail|exception/i
                  _ -> false
                end
              end)
            _ -> false
          end

          # Create ToolResult using proper constructor
          LangChain.Message.ToolResult.new!(%{
            call_id: tool_use_id,
            content: result_content,
            is_error: is_error
          })
        end)

        {:ok, tool_msg} = LangChain.Message.new_tool_result(%{
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
    converted_tools = Enum.map(tools, fn tool ->
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

    # Extract required fields
    model = Map.get(body, "model")
    messages = Map.get(body, "messages", [])
    max_tokens = Map.get(body, "max_tokens", 4096)
    stream = Map.get(body, "stream", false)
    temperature = Map.get(body, "temperature", 1.0)
    top_p = Map.get(body, "top_p")
    top_k = Map.get(body, "top_k")
    stop_sequences = Map.get(body, "stop_sequences")
    tools = Map.get(body, "tools", [])
    tool_choice = Map.get(body, "tool_choice")

    # Build options map for LLM chain from request parameters
    opts = %{
      max_tokens: max_tokens,
      temperature: temperature,
      stream: stream
    }
    # Add optional parameters if present
    opts = if top_p != nil, do: Map.put(opts, :top_p, top_p), else: opts
    opts = if top_k != nil, do: Map.put(opts, :top_k, top_k), else: opts
    opts = if stop_sequences != nil and stop_sequences != [], do: Map.put(opts, :stop, stop_sequences), else: opts

    # Get LLM config based on model
    case Exhub.Llm.Chain.create_llm_chain(model, opts) do
      llm_chain ->
        # Convert Anthropic message format to LangChain format
        langchain_messages = Enum.map(messages, fn msg ->
          role = Map.get(msg, "role")
          content = Map.get(msg, "content")

          # Convert role string to atom
          role_atom =
            case role do
              "system" -> :system
              "user" -> :user
              "assistant" -> :assistant
              _ -> :user  # default fallback
            end

          # Process content based on its type
          processed_content =
            case content do
              nil -> ""
              str when is_binary(str) -> str
              list when is_list(list) ->
                # For lists, extract text content and convert other types to strings
                Enum.map_join(list, "\n", fn item ->
                  case item do
                    %{"type" => "text", "text" => text} -> text
                    %{"type" => "tool_use", "name" => name, "input" => input} ->
                      "Tool use: #{name} with input: #{inspect(input)}"
                    %{"type" => "tool_result", "tool_use_id" => tool_id, "content" => content} ->
                      "Tool result for #{tool_id}: #{inspect(content)}"
                    %{"type" => "image", "source" => source} ->
                      "Image content: #{inspect(source)}"
                    _ -> inspect(item)
                  end
                end)
              _ -> inspect(content)
            end

          # Create a proper Message struct using the new!/1 function
          Message.new!(%{
            role: role_atom,
            content: processed_content
          })
        end)

        # Prepare custom context for tools
        custom_context = %{}

        if stream do
          # Streaming mode
          response_id = "msg_#{UUID.uuid4()}"

          # Start chunked response
          conn = send_chunked(conn, 200)

          # Send initial events
          message_start = %{
            "type" => "message_start",
            "message" => %{
              "id" => response_id,
              "type" => "message",
              "role" => "assistant",
              "model" => model,
              "content" => [],
              "stop_reason" => nil,
              "stop_sequence" => nil,
              "usage" => %{
                "input_tokens" => 0,
                "output_tokens" => 0
              }
            }
          }
          {:ok, conn} = chunk(conn, "event: message_start\ndata: #{Jason.encode!(message_start)}\n\n")

          {:ok, conn} = chunk(conn, "event: content_block_start\ndata: #{Jason.encode!(%{"type" => "content_block_start", "index" => 0, "content_block" => %{"type" => "text", "text" => ""}})}\n\n")

          # Set up callbacks that send messages to this process
          parent = self()

          callbacks = %{
            on_llm_new_delta: fn _chain, deltas ->
            Enum.each(deltas, fn delta ->
              delta_text = MessageDelta.content_to_string(delta)
              send(parent, {
                    :stream_chunk, %{
                      "type" => "content_block_delta",
                      "index" => 0,
                      "delta" => %{
                        "type" => "text_delta",
                        "text" => delta_text
                      }
                    }})
            end)
          end,
            on_message_processed: fn _chain, _message ->
              send(parent, {:stream_done, response_id})
            end
          }

          # Run the chain in a task
          task = Task.async(fn ->
            Exhub.Llm.Chain.run(llm_chain, langchain_messages, tools, custom_context, callbacks)
          end)

          # Stream results back to client
          conn = stream_response_loop(conn, task)

          conn
        else
          # Non-streaming mode - use existing execute logic
          case Exhub.Llm.Chain.execute(llm_chain, langchain_messages, tools, custom_context) do
            {:ok, response} ->
              response_id = "msg_#{UUID.uuid4()}"

              content = [
                %{"type" => "text", "text" => response}
              ]

              anthropic_response = %{
                id: response_id,
                model: model,
                role: "assistant",
                content: content,
                type: "message",
                stop_reason: "end_turn",
                usage: %{
                  input_tokens: 0,
                  output_tokens: String.length(response) |> div(4)
                }
              }

              # Log the response for debugging
              Logger.debug("Anthropic response: #{inspect(anthropic_response)}")

              conn
              |> put_resp_content_type("application/json")
              |> send_resp(200, Jason.encode!(anthropic_response))

            error ->
              conn
              |> put_resp_content_type("application/json")
              |> send_resp(500, Jason.encode!(%{error: "LLM execution failed", details: inspect(error)}))
          end
        end

      {:error, reason} ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(400, Jason.encode!(%{error: "Invalid model", details: reason}))
    end
  end

  defp stream_response_loop(conn, task) do
    receive do
      {:stream_chunk, event} ->
        {:ok, conn} = chunk(conn, "event: content_block_delta\ndata: #{Jason.encode!(event)}\n\n")
        stream_response_loop(conn, task)

      {:stream_done, _response_id} ->
        # Send final events
        {:ok, conn} = chunk(conn, "event: content_block_stop\ndata: #{Jason.encode!(%{"type" => "content_block_stop", "index" => 0})}\n\n")

        message_delta = %{
          "type" => "message_delta",
          "delta" => %{"stop_reason" => "end_turn", "stop_sequence" => nil},
          "usage" => %{"output_tokens" => 0}
        }
        {:ok, conn} = chunk(conn, "event: message_delta\ndata: #{Jason.encode!(message_delta)}\n\n")

        {:ok, conn} = chunk(conn, "event: message_stop\ndata: #{Jason.encode!(%{"type" => "message_stop"})}\n\n")
        {:ok, conn} = chunk(conn, "data: [DONE]\n\n")

        # Wait for task to finish
        Task.await(task)

        conn

      {:DOWN, _ref, :process, _pid, _reason} ->
        # Task finished or crashed
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
    total_tokens = Enum.reduce(messages, 0, fn msg, acc ->
      content = Map.get(msg, "content")
      case content do
        nil -> acc
        str when is_binary(str) -> acc + div(String.length(str), 4)
        list when is_list(list) ->
          Enum.reduce(list, acc, fn item, acc2 ->
            case item do
              %{"type" => "text", "text" => text} -> acc2 + div(String.length(text), 4)
              %{"type" => "tool_use", "name" => name, "input" => input} ->
                acc2 + div(String.length(name), 4) + div(String.length(inspect(input)), 4)
              %{"type" => "tool_result", "tool_use_id" => tool_id, "content" => content} ->
                acc2 + div(String.length(tool_id), 4) + div(String.length(inspect(content)), 4)
              _ -> acc2 + div(String.length(inspect(item)), 4)
            end
          end)
        _ -> acc + div(String.length(inspect(content)), 4)
      end
    end)

    # Add tokens for tools
    tool_tokens = Enum.reduce(tools, 0, fn tool, acc ->
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
    send_resp(conn, 200, "<html><head></head><body>Connect to WS enpoint at \"/exhub\"</body></html>")
  end
end
