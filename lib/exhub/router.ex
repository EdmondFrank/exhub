defmodule Exhub.Router do
  @moduledoc """
  HTTP Router for Exhub API.

  Handles routing for:
  - LLM provider proxy endpoints (OpenAI, Anthropic, Groq, Google, Cohere, Samba)
  - Anthropic-compatible API (/v1/messages)
  - Token counting endpoint
  - MCP habit configuration endpoint
  - Token usage dashboard

  ## Routes

  ### Proxy Endpoints
  - `POST /groq/v1/*path` - Proxies to Groq API
  - `POST /google/v1/*path` - Proxies to Google Generative Language API
  - `POST /cohere/v1/*path` - Proxies to Cohere API
  - `POST /samba/v1/*path` - Proxies to SambaNova API
  - `GET|POST /openai/v1/*path` - Proxies to OpenAI-compatible endpoints with model routing
  - `POST /anthropic/v1/*path` - Proxies to Anthropic API with model routing
  - `GET|POST /burncloud/v1/*path` - Proxies to BurnCloud API with Bearer token auth

  ### API Endpoints
  - `POST /v1/messages` - Anthropic-compatible messages endpoint
  - `POST /v1/messages/count_tokens` - Token counting endpoint

  ### Dashboard
  - `GET /dashboard` - HTML dashboard for token usage
  - `GET /dashboard/token_usage` - API for token usage records
  - `GET /dashboard/stats` - API for aggregated statistics
  - `GET /dashboard/data` - API for dashboard data

  ### MCP
  - `POST /mcp` - MCP habit server endpoint
  - `POST /think/mcp` - MCP think/plan server endpoint
  - `POST /time/mcp` - MCP time server endpoint
  - `POST /web-tools/mcp` - MCP web tools server endpoint
  - `POST /archery/mcp` - MCP Archery SQL audit platform endpoint
  - `POST /browser-use/mcp` - MCP browser automation server endpoint
  - `POST /image-gen/mcp` - MCP image generation server endpoint
  - `POST /todo/mcp` - MCP multi-tenant todo list server endpoint
  - `POST /desktop/mcp` - MCP desktop commander server endpoint (filesystem, process, search)
  - `POST /doc-extract/mcp` - MCP document extraction server endpoint (PDF, DOCX, images via Gitee AI)
  """

  use Plug.Router
  use PlugSocket

  require Logger

  alias Exhub.ProxyPlug
  alias Exhub.Router.Config, as: RouterConfig
  alias Exhub.Router.DashboardView
  alias Exhub.Router.Helpers
  alias Exhub.Converters.Anthropic, as: AnthropicConverter
  alias Exhub.Llm.LlmConfigServer
  alias UUID

  plug(Plug.Parsers,
    parsers: [:urlencoded, {:json, json_decoder: Jason}],
    pass: ["*/*"]
  )

  socket("/exhub", Exhub.SocketHandler,
    websocket: [timeout: RouterConfig.get_timeout(), recv_timeout: RouterConfig.get_timeout()],
    longpoll: false
  )

  plug(:match)
  plug(:dispatch)

  # ============================================================================
  # Provider Proxy Routes
  # ============================================================================

  # Simple proxy routes - all follow the same pattern
  @proxy_routes [
    {"/groq/v1", "https://api.groq.com/openai/v1"},
    {"/google/v1", "https://generativelanguage.googleapis.com/"},
    {"/cohere/v1", "https://api.cohere.ai/compatibility/v1"},
    {"/samba/v1", "https://api.sambanova.ai/v1"}
  ]

  for {path, upstream} <- @proxy_routes do
    post "#{path}/*path" do
      ProxyPlug.forward_upstream(conn, unquote(upstream),
        client_options: [proxy: RouterConfig.get_proxy()]
      )
    end
  end

  # ============================================================================
  # OpenAI Proxy Routes (with model-based routing)
  # ============================================================================

  get "/openai/v1/*path" do
    token = Application.get_env(:exhub, :openai_api_key, "")

    options = [
      custom_headers: [{"Authorization", "Bearer #{token}"}],
      client_options: [
        timeout: RouterConfig.get_timeout(),
        recv_timeout: RouterConfig.get_timeout()
      ]
    ]

    Logger.info("[OpenAI Proxy] Forwarding request - models")
    ProxyPlug.forward_upstream(conn, "https://pinova.ai/v1", options)
  end

  post "/openai/v1/*path" do
    model = Helpers.extract_model(conn)
    target_url = RouterConfig.get_model_target(model)
    token = RouterConfig.get_model_api_key(model)

    options = [
      custom_headers: [{"Authorization", "Bearer #{token}"}],
      client_options: [
        timeout: RouterConfig.get_timeout(),
        recv_timeout: RouterConfig.get_timeout()
      ]
    ]

    Logger.info(
      "[OpenAI Proxy] Forwarding request - model: #{inspect(model)}, target: #{target_url}, has_token: #{token != ""}"
    )

    ProxyPlug.forward_upstream(conn, target_url, options)
  end

  # ============================================================================
  # BurnCloud Proxy Routes
  # ============================================================================

  get "/burncloud/v1/*path" do
    token = Application.get_env(:exhub, :burncloud_api_key, "")

    options = [
      custom_headers: [{"Authorization", "Bearer #{token}"}],
      client_options: [
        timeout: RouterConfig.get_timeout(),
        recv_timeout: RouterConfig.get_timeout()
      ]
    ]

    Logger.info("[BurnCloud Proxy] Forwarding request - #{inspect(path)}")
    ProxyPlug.forward_upstream(conn, RouterConfig.get_burncloud_target(), options)
  end

  post "/burncloud/v1/*path" do
    token = Application.get_env(:exhub, :burncloud_api_key, "")

    options = [
      custom_headers: [{"Authorization", "Bearer #{token}"}],
      client_options: [
        timeout: RouterConfig.get_timeout(),
        recv_timeout: RouterConfig.get_timeout()
      ]
    ]

    Logger.info("[BurnCloud Proxy] Forwarding request - #{inspect(path)}")
    ProxyPlug.forward_upstream(conn, RouterConfig.get_burncloud_target(), options)
  end

  # ============================================================================
  # Anthropic Proxy Route (with model-based routing)
  # ============================================================================

  post "/anthropic/v1/*path" do
    model = Helpers.extract_model(conn)
    target_url = RouterConfig.get_anthropic_target(model)
    token = RouterConfig.get_model_api_key(model)
    use_proxy = RouterConfig.use_proxy_for_model?(model)
    proxy = if use_proxy, do: RouterConfig.get_proxy(), else: ""

    options = [
      custom_headers: [{"x-api-key", token}],
      client_options: [
        timeout: RouterConfig.get_timeout(),
        recv_timeout: RouterConfig.get_timeout(),
        proxy: proxy
      ]
    ]

    Logger.info(
      "[Anthropic Proxy] Forwarding request - model: #{inspect(model)}, target: #{target_url}, proxy: #{proxy}, use_proxy: #{use_proxy}"
    )

    ProxyPlug.forward_upstream(conn, target_url, options)
  end

  # ============================================================================
  # Anthropic-compatible API Endpoint
  # ============================================================================

  post "/v1/messages" do
    body = conn.body_params

    model = Map.get(body, "model", "")
    messages = Map.get(body, "messages", []) || []
    system = Map.get(body, "system")
    stream = Map.get(body, "stream", true)
    tools = Map.get(body, "tools", []) || []
    max_tokens = Map.get(body, "max_tokens")

    Logger.info("Starting streaming response for model: #{model}")

    # Convert to LangChain format
    langchain_request =
      AnthropicConverter.to_langchain(%{
        "model" => model,
        "messages" => messages,
        "system" => system,
        "stream" => stream,
        "tools" => tools,
        "max_tokens" => max_tokens || 4096
      })

    langchain_messages = Map.get(langchain_request, :messages, []) || []
    langchain_tools = Map.get(langchain_request, :tools, []) || []
    langchain_system = Map.get(langchain_request, :system)

    all_messages =
      if langchain_system do
        [langchain_system | langchain_messages]
      else
        langchain_messages
      end

    # Get LLM chain configuration
    {:ok, config} = LlmConfigServer.get_default_llm_config()
    llm_name = config[:model]
    api_key = RouterConfig.get_model_api_key(model)

    chain_options = %{stream: stream}

    chain_options =
      if api_key != "", do: Map.put(chain_options, :api_key, api_key), else: chain_options

    case Exhub.Llm.Chain.create_llm_chain(llm_name, chain_options) do
      llm_chain when is_map(llm_chain) ->
        if stream do
          handle_streaming_request(
            conn,
            llm_chain,
            all_messages,
            langchain_tools,
            model,
            llm_name
          )
        else
          handle_non_streaming_request(
            conn,
            llm_chain,
            all_messages,
            langchain_tools,
            model,
            llm_name
          )
        end
    end
  end

  # ============================================================================
  # Token Count Endpoint
  # ============================================================================

  post "/v1/messages/count_tokens" do
    body = conn.body_params
    messages = Map.get(body, "messages", [])
    tools = Map.get(body, "tools", [])

    total_tokens = estimate_tokens_detailed(messages, tools)

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Jason.encode!(%{input_tokens: total_tokens}))
  end

  # ============================================================================
  # MCP Endpoint
  # ============================================================================

  forward("/mcp",
    to: Exhub.MCP.LazyPlug,
    init_opts: [server: Exhub.MCP.HabitServer, request_timeout: 120_000]
  )

  forward("/web-tools/mcp",
    to: Exhub.MCP.LazyPlug,
    init_opts: [server: Exhub.MCP.WebToolsServer, request_timeout: 120_000]
  )

  forward("/think/mcp",
    to: Exhub.MCP.LazyPlug,
    init_opts: [server: Exhub.MCP.ThinkServer, request_timeout: 120_000]
  )

  forward("/time/mcp",
    to: Exhub.MCP.LazyPlug,
    init_opts: [server: Exhub.MCP.TimeServer, request_timeout: 120_000]
  )

  forward("/archery/mcp",
    to: Exhub.MCP.LazyPlug,
    init_opts: [server: Exhub.MCP.ArcheryServer, request_timeout: 120_000]
  )

  forward("/browser-use/mcp",
    to: Exhub.MCP.LazyPlug,
    init_opts: [server: Exhub.MCP.BrowserUseServer, request_timeout: 120_000]
  )

  forward("/image-gen/mcp",
    to: Exhub.MCP.LazyPlug,
    init_opts: [server: Exhub.MCP.ImageGenServer, request_timeout: 120_000]
  )

  forward("/todo/mcp",
    to: Exhub.MCP.LazyPlug,
    init_opts: [server: Exhub.MCP.TodoServer, request_timeout: 120_000]
  )

  forward("/desktop/mcp",
    to: Exhub.MCP.LazyPlug,
    init_opts: [server: Exhub.MCP.DesktopServer, request_timeout: 120_000]
  )

  forward("/doc-extract/mcp",
    to: Exhub.MCP.LazyPlug,
    init_opts: [server: Exhub.MCP.DocExtractServer, request_timeout: 120_000]
  )

  # ============================================================================
  # Dashboard Routes
  # ============================================================================

  get "/dashboard" do
    conn
    |> put_resp_content_type("text/html")
    |> send_resp(200, DashboardView.render())
  end

  get "/dashboard/token_usage" do
    filters =
      %{
        start_date: conn.params["start_date"],
        end_date: conn.params["end_date"],
        model: conn.params["model"],
        provider: conn.params["provider"],
        limit: Helpers.parse_int(conn.params["limit"], 100),
        offset: Helpers.parse_int(conn.params["offset"], 0)
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
    start_date = conn.params["start_date"]
    end_date = conn.params["end_date"]
    group_by = conn.params["group_by"] || "model"

    filters =
      %{}
      |> Helpers.maybe_put(:start_date, start_date)
      |> Helpers.maybe_put(:end_date, end_date)

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
    days = Helpers.parse_int(conn.params["days"], 30)

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

  # ============================================================================
  # Catch-all Route
  # ============================================================================

  match _ do
    send_resp(
      conn,
      200,
      "<html><head></head><body>Connect to WS endpoint at \"/exhub\"</body></html>"
    )
  end

  # ============================================================================
  # Private Functions - Request Handlers
  # ============================================================================

  defp handle_streaming_request(conn, llm_chain, messages, tools, model, llm_name) do
    response_id = "msg_#{UUID.uuid4()}"

    Logger.info(
      "Starting streaming response for model: #{model}, llm: #{llm_name}, response_id: #{response_id}"
    )

    # Setup SSE headers
    conn =
      conn
      |> put_resp_header("content-type", "text/event-stream")
      |> put_resp_header("cache-control", "no-cache")
      |> put_resp_header("connection", "keep-alive")
      |> send_chunked(200)

    # Send initial events
    conn = send_message_start(conn, response_id, model, messages)
    conn = send_content_block_start(conn)
    conn = send_ping(conn)

    # Setup callbacks and run chain
    parent = self()

    callbacks = %{
      on_llm_new_delta: fn _chain, deltas ->
        Enum.each(deltas, &send_delta_to_parent(&1, response_id, parent))
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

    task =
      Task.async(fn ->
        Exhub.Llm.Chain.run(llm_chain, messages, tools, %{}, callbacks)
      end)

    tracking_ctx = %{
      model: model || llm_name || "unknown",
      provider: "exhub",
      input_tokens: Helpers.estimate_input_tokens(messages, nil, tools)
    }

    stream_response_loop(conn, task, response_id, tracking_ctx, "")
  end

  defp handle_non_streaming_request(conn, llm_chain, messages, tools, model, llm_name) do
    Logger.info("Starting non-streaming response for model: #{model}, llm: #{llm_name}")

    case Exhub.Llm.Chain.execute(llm_chain, messages, tools, %{}) do
      response when is_binary(response) ->
        send_non_streaming_response(conn, response, model, llm_name, messages, tools)

      error ->
        send_error_response(conn, error)
    end
  end

  # ============================================================================
  # Private Functions - Response Building
  # ============================================================================

  defp send_message_start(conn, response_id, model, messages) do
    input_tokens = Helpers.estimate_input_tokens(messages, nil, [])

    message_start = %{
      "type" => "message_start",
      "message" => %{
        "id" => response_id,
        "type" => "message",
        "role" => "assistant",
        "model" => model || "",
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

    Helpers.send_sse_event(conn, "message_start", message_start)
  end

  defp send_content_block_start(conn) do
    Helpers.send_sse_event(conn, "content_block_start", %{
      "type" => "content_block_start",
      "index" => 0,
      "content_block" => %{"type" => "text", "text" => ""}
    })
  end

  defp send_ping(conn) do
    Helpers.send_sse_event(conn, "ping", %{"type" => "ping"})
  end

  defp send_non_streaming_response(conn, response, model, llm_name, messages, tools) do
    response_id = "msg_#{UUID.uuid4()}"
    safe_response = if is_binary(response), do: response, else: ""

    content =
      if safe_response != "" do
        [%{"type" => "text", "text" => safe_response}]
      else
        [%{"type" => "text", "text" => ""}]
      end

    input_tokens = Helpers.estimate_input_tokens(messages, nil, tools)
    output_tokens = Helpers.estimate_output_tokens(safe_response)

    Helpers.track_usage(
      model || llm_name || "unknown",
      "exhub",
      input_tokens,
      output_tokens,
      response_id
    )

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
  end

  defp send_error_response(conn, error) do
    Logger.error("LLM execution failed: #{inspect(error)}")

    error_message = Helpers.get_error_message(error)

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

  # ============================================================================
  # Private Functions - Streaming
  # ============================================================================

  defp send_delta_to_parent(delta, response_id, parent) do
    if match?(%LangChain.MessageDelta{}, delta) do
      delta_text = extract_delta_text(delta)

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
  end

  defp extract_delta_text(delta) do
    delta_content_raw = Map.get(delta, :content)

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
  end

  defp stream_response_loop(conn, task, response_id, tracking_ctx, acc_text) do
    receive do
      {:stream_chunk, ^response_id, event} ->
        {conn, new_acc_text} = handle_stream_chunk(conn, event, acc_text)
        stream_response_loop(conn, task, response_id, tracking_ctx, new_acc_text)

      {:message_complete, ^response_id, _message} ->
        conn = finalize_stream(conn, response_id, tracking_ctx, acc_text)
        Task.await(task)
        conn

      {:error, ^response_id, error} ->
        conn = handle_stream_error(conn, response_id, error)
        Task.await(task)
        conn

      {:DOWN, _ref, :process, _pid, :normal} ->
        conn

      {:DOWN, _ref, :process, _pid, reason} ->
        Logger.error(
          "stream_response_loop: Task DOWN (crashed) for #{response_id} - reason: #{inspect(reason)}"
        )

        conn
        |> send_error_sse("Stream processing failed: #{inspect(reason)}")
        |> send_final_chunk()
    end
  end

  defp handle_stream_chunk(conn, event, acc_text) do
    delta_data = Map.get(event, "delta", {})
    delta_type = Map.get(delta_data, "type", "text_delta")
    text = Map.get(delta_data, "text", "")

    new_acc_text = if delta_type == "text_delta", do: acc_text <> text, else: acc_text

    conn =
      if delta_type == "text_delta" and text not in [nil, ""] do
        Helpers.send_sse_event(conn, "content_block_delta", %{
          "type" => "content_block_delta",
          "index" => 0,
          "delta" => %{"type" => "text_delta", "text" => text}
        })
      else
        conn
      end

    {conn, new_acc_text}
  end

  defp finalize_stream(conn, response_id, tracking_ctx, acc_text) do
    if map_size(tracking_ctx) > 0 do
      output_tokens = Helpers.estimate_output_tokens(acc_text)

      Helpers.track_usage(
        tracking_ctx[:model] || "unknown",
        tracking_ctx[:provider] || "exhub",
        tracking_ctx[:input_tokens] || 0,
        output_tokens,
        response_id
      )
    end

    conn
    |> Helpers.send_sse_event("content_block_stop", %{
      "type" => "content_block_stop",
      "index" => 0
    })
    |> Helpers.send_sse_event("message_delta", %{
      "type" => "message_delta",
      "delta" => %{
        "stop_reason" => "end_turn",
        "stop_sequence" => nil
      },
      "usage" => %{
        "output_tokens" => Helpers.estimate_output_tokens(acc_text)
      }
    })
    |> Helpers.send_sse_event("message_stop", %{"type" => "message_stop"})
    |> send_final_chunk()
  end

  defp handle_stream_error(conn, response_id, error) do
    Logger.error("stream_response_loop: Received error for #{response_id} - #{inspect(error)}")

    conn
    |> send_error_sse(inspect(error))
    |> Helpers.send_sse_event("message_stop", %{"type" => "message_stop"})
    |> send_final_chunk()
  end

  defp send_error_sse(conn, message) do
    Helpers.send_sse_event(conn, "error", %{
      "type" => "error",
      "error" => %{
        "type" => "internal_server_error",
        "message" => message
      }
    })
  end

  defp send_final_chunk(conn) do
    {:ok, conn} = Plug.Conn.chunk(conn, "data: [DONE]\n\n")
    conn
  end

  # ============================================================================
  # Private Functions - Token Estimation
  # ============================================================================

  defp estimate_tokens_detailed(messages, tools) do
    Enum.reduce(messages, 0, fn msg, acc ->
      content = Map.get(msg, "content")
      acc + estimate_content_tokens(content)
    end) + estimate_tool_tokens(tools)
  end

  defp estimate_content_tokens(nil), do: 0

  defp estimate_content_tokens(str) when is_binary(str) do
    div(String.length(str), 4)
  end

  defp estimate_content_tokens(list) when is_list(list) do
    Enum.reduce(list, 0, fn item, acc ->
      acc + estimate_item_tokens(item)
    end)
  end

  defp estimate_content_tokens(other), do: div(String.length(inspect(other)), 4)

  defp estimate_item_tokens(%{"type" => "text", "text" => text}) do
    div(String.length(text), 4)
  end

  defp estimate_item_tokens(%{"type" => "tool_use", "name" => name, "input" => input}) do
    div(String.length(name), 4) + div(String.length(inspect(input)), 4)
  end

  defp estimate_item_tokens(%{
         "type" => "tool_result",
         "tool_use_id" => tool_id,
         "content" => content
       }) do
    div(String.length(tool_id), 4) + div(String.length(inspect(content)), 4)
  end

  defp estimate_item_tokens(other) do
    div(String.length(inspect(other)), 4)
  end

  defp estimate_tool_tokens(tools) when is_list(tools) do
    Enum.reduce(tools, 0, fn tool, acc ->
      acc + div(String.length(Map.get(tool, "name", "")), 4) +
        div(String.length(inspect(Map.get(tool, "input_schema", ""))), 4)
    end)
  end

  defp estimate_tool_tokens(_), do: 0
end
