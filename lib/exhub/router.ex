defmodule Exhub.Router do
  alias Exhub.ProxyPlug
  require Logger
  use Plug.Router
  use PlugSocket
  alias UUID
  alias LangChain.Message
  alias LangChain.MessageDelta
  alias LangChain.Message.ContentPart

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
      "glm-4.6" => "https://ai.gitee.com/v1",
      "glm-4_5v" => "https://ai.gitee.com/v1",
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
      "minimax-m2-preview" => "https://api.minimaxi.com/v1",
      "gemini-2.5-pro" => "http://localhost:8765/v1",
      "gemini-2.5-flash" => "http://localhost:8765/v1",
      "openrouter/polaris-alpha" => "https://openrouter.ai/api/v1"
    }

    # Model to token mapping (extendable)
    model_token_map = %{
      "step3" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "glm-4_5" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "glm-4.6" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "glm-4_5v" => Application.get_env(:exhub, :giteeai_api_key, ""),
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
