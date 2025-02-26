defmodule Protocol do
  @moduledoc """
  This file implements the core protocol layer for JSON-RPC communication in the MCP SDK.
  It handles the protocol-level concerns of JSON-RPC messaging, including request/response
  correlation, progress tracking, request cancellation, and error handling.

  Key Components:

  1. Protocol:
    - Core type managing JSON-RPC communication
    - Handles message correlation and lifecycle
    - Supports:
    - Request/Response with timeouts
    - Notifications (one-way messages)
    - Progress updates during long operations
    - Request cancellation
    - Error propagation

  2. Request Handling:
    - Automatic request ID generation
    - Context-based cancellation
    - Configurable timeouts
    - Progress callback support
    - Response correlation using channels

  3. Message Types:
    - JSONRPCRequest: Outgoing requests with IDs
    - JSONRPCNotification: One-way messages
    - JSONRPCError: Error responses
    - Progress: Updates during long operations

  4. Handler Registration:
    - Request handlers for method calls
    - Notification handlers for events
    - Progress handlers for long operations
    - Error handlers for protocol errors

  Thread Safety:
    - All public methods are thread-safe
    - Uses sync.RWMutex for state protection
    - Safe for concurrent requests and handlers

  Usage:

      transport := NewStdioTransport()
      protocol := NewProtocol(transport)

      # Start protocol
      protocol.Connect(transport)
      defer protocol.Close()

      # Make a request
      ctx := context.Background()
      response, err := protocol.Request(ctx, "method", params, &RequestOptions{
          Timeout: 5 * time.Second,
          OnProgress: func(p Progress) {
              # Handle progress updates
          },
      })

  Error Handling:
    - Context-based cancellation
    - Timeout management
    - Proper cleanup of pending requests
    - Detailed error information

  For more details, see the test file protocol_test.go.
  """

  import JSON
  import Logger

  @default_request_timeout 60_000

  @derive {JSON.Encoder, only: [:progress, :total]}
  defstruct progress: 0, total: 0

  @derive {JSON.Encoder, only: [:enforce_strict_capabilities]}
  defstruct enforce_strict_capabilities: false

  @derive {JSON.Encoder, only: [:on_progress, :context, :timeout]}
  defstruct on_progress: nil, context: nil, timeout: @default_request_timeout

  @derive {JSON.Encoder, only: [:context]}
  defstruct context: nil

  @derive {JSON.Encoder, only: [:request_message_id, :request_handlers, :request_cancellers, :notification_handlers, :response_handlers, :progress_handlers, :on_close, :on_error, :fallback_request_handler, :fallback_notification_handler]}
  defstruct request_message_id: 0,
            request_handlers: %{},
            request_cancellers: %{},
            notification_handlers: %{},
            response_handlers: %{},
            progress_handlers: %{},
            on_close: nil,
            on_error: nil,
            fallback_request_handler: nil,
            fallback_notification_handler: nil

  defstruct response_envelope: nil, err: nil

  def new(options \\ %{}) do
    %Protocol{
      options: options,
      request_handlers: %{},
      request_cancellers: %{},
      notification_handlers: %{},
      response_handlers: %{},
      progress_handlers: %{},
    }
  end

  def connect(%Protocol{} = protocol, transport) do
    protocol
    |> set_transport(transport)
    |> set_close_handler(&handle_close/1)
    |> set_error_handler(&handle_error/2)
    |> set_message_handler(&handle_message/3)

    transport.start_link()
  end

  def handle_close(%Protocol{} = protocol) do
    protocol
    |> clear_handlers()
    |> cancel_pending_requests()
    |> close_response_channels()
    |> invoke_on_close()
  end

  def handle_error(%Protocol{} = protocol, err) do
    protocol
    |> invoke_on_error(err)
  end

  def handle_message(%Protocol{} = protocol, ctx, message) do
    case message.type do
      :jsonrpc_request -> handle_request(protocol, ctx, message.jsonrpc_request)
      :jsonrpc_notification -> handle_notification(protocol, message.jsonrpc_notification)
      :jsonrpc_response -> handle_response(protocol, message.jsonrpc_response, nil)
      :jsonrpc_error -> handle_response(protocol, nil, message.jsonrpc_error)
    end
  end

  def handle_notification(%Protocol{} = protocol, notification) do
    handler = get_notification_handler(protocol, notification.method)

    if handler do
      Task.start_link(fn -> handler.(notification) end)
    end
  end

  def handle_request(%Protocol{} = protocol, ctx, request) do
    handler = get_request_handler(protocol, request.method)

    cancel = Task.Supervisor.start_child(Task.Supervisor.child_spec(&handle_request_task/3, [protocol, ctx, request, handler]))

    protocol
    |> add_request_canceller(request.id, cancel)
    |> invoke_request_handler(ctx, request, handler)
  end

  def handle_request_task(%Protocol{} = protocol, ctx, request, handler) do
    result, err = handler.(ctx, request, %RequestHandlerExtra{context: ctx})

    if err do
      send_error_response(protocol, request.id, err)
    else
      send_response(protocol, request.id, result)
    end
  end

  def handle_progress_notification(%Protocol{} = protocol, notification) do
    params = JSON.decode!(notification.params)

    handler = get_progress_handler(protocol, params.progress_token)

    if handler do
      handler.(%Progress{progress: params.progress, total: params.total})
    end
  end

  def handle_cancelled_notification(%Protocol{} = protocol, notification) do
    params = JSON.decode!(notification.params)

    cancel = get_request_canceller(protocol, params.request_id)

    if cancel do
      cancel.()
    end
  end

  def handle_response(%Protocol{} = protocol, response, err_resp) do
    id = response.id
    result = response.result
    err = nil

    if err_resp do
      id = err_resp.id
      err = "RPC error #{err_resp.error.code}: #{err_resp.error.message}"
    end

    ch = get_response_handler(protocol, id)

    if ch do
      send(ch, %ResponseEnvelope{response: result, err: err})
    end
  end

  def close(%Protocol{} = protocol) do
    transport = protocol.transport

    if transport do
      transport.close()
    end
  end

  def request(%Protocol{} = protocol, ctx, method, params, opts \\ %{}) do
    if protocol.transport do
      id = protocol.request_message_id
      ch = Task.Supervisor.start_child(Task.Supervisor.child_spec(&handle_request_task/3, [protocol, ctx, request, handler]))

      protocol
      |> increment_request_message_id()
      |> add_response_handler(id, ch)
      |> add_progress_handler(id, opts.on_progress)

      request = %BaseJSONRPCRequest{
        jsonrpc: "2.0",
        method: method,
        params: JSON.encode(params),
        id: id
      }

      protocol.transport.send(ctx, %BaseMessageRequest{request: request})

      receive do
        %ResponseEnvelope{response: response, err: err} ->
          if err do
            {:error, err}
          else
            {:ok, response}
          end
        :timeout ->
          send_cancel_notification(protocol, id, "request timeout")
          {:error, "request timeout after #{opts.timeout}ms"}
      end
    else
      {:error, "not connected"}
    end
  end

  def send_cancel_notification(%Protocol{} = protocol, request_id, reason) do
    params = %{
      request_id: request_id,
      reason: reason
    }

    notification = %BaseJSONRPCNotification{
      jsonrpc: "2.0",
      method: "notifications/cancelled",
      params: JSON.encode(params)
    }

    protocol.transport.send(:background, %BaseMessageNotification{notification: notification})
  end

  def send_error_response(%Protocol{} = protocol, request_id, err) do
    response = %BaseJSONRPCError{
      jsonrpc: "2.0",
      id: request_id,
      error: %BaseJSONRPCErrorInner{
        code: -32000,
        message: err
      }
    }

    protocol.transport.send(:background, %BaseMessageError{response: response})
  end

  def notification(%Protocol{} = protocol, method, params) do
    if protocol.transport do
      notification = %BaseJSONRPCNotification{
        jsonrpc: "2.0",
        method: method,
        params: JSON.encode(params)
      }

      protocol.transport.send(:background, %BaseMessageNotification{notification: notification})
    else
      {:error, "not connected"}
    end
  end

  def set_request_handler(%Protocol{} = protocol, method, handler) do
    protocol
    |> add_request_handler(method, handler)
  end

  def remove_request_handler(%Protocol{} = protocol, method) do
    protocol
    |> delete_request_handler(method)
  end

  def set_notification_handler(%Protocol{} = protocol, method, handler) do
    protocol
    |> add_notification_handler(method, handler)
  end

  def remove_notification_handler(%Protocol{} = protocol, method) do
    protocol
    |> delete_notification_handler(method)
  end

  defp set_transport(%Protocol{} = protocol, transport) do
    %Protocol{protocol | transport: transport}
  end

  defp set_close_handler(%Protocol{} = protocol, handler) do
    protocol.transport.set_close_handler(handler)
    protocol
  end

  defp set_error_handler(%Protocol{} = protocol, handler) do
    protocol.transport.set_error_handler(handler)
    protocol
  end

  defp set_message_handler(%Protocol{} = protocol, handler) do
    protocol.transport.set_message_handler(handler)
    protocol
  end

  defp clear_handlers(%Protocol{} = protocol) do
    %Protocol{
      protocol |
      request_handlers: %{},
      notification_handlers: %{}
    }
  end

  defp cancel_pending_requests(%Protocol{} = protocol) do
    Enum.each(protocol.request_cancellers, fn {_, cancel} -> cancel.() end)
    %Protocol{protocol | request_cancellers: %{}}
  end

  defp close_response_channels(%Protocol{} = protocol) do
    Enum.each(protocol.response_handlers, fn {id, ch} ->
      send(ch, %ResponseEnvelope{err: "connection closed"})
      :ok
    end)
    %Protocol{protocol | response_handlers: %{}}
  end

  defp invoke_on_close(%Protocol{} = protocol) do
    if protocol.on_close do
      protocol.on_close.()
    end
    protocol
  end

  defp invoke_on_error(%Protocol{} = protocol, err) do
    if protocol.on_error do
      protocol.on_error.(err)
    end
    protocol
  end

  defp get_notification_handler(%Protocol{} = protocol, method) do
    protocol.notification_handlers[method] || protocol.fallback_notification_handler
  end

  defp get_request_handler(%Protocol{} = protocol, method) do
    protocol.request_handlers[method] || protocol.fallback_request_handler
  end

  defp get_progress_handler(%Protocol{} = protocol, token) do
    protocol.progress_handlers[token]
  end

  defp get_request_canceller(%Protocol{} = protocol, id) do
    protocol.request_cancellers[id]
  end

  defp get_response_handler(%Protocol{} = protocol, id) do
    protocol.response_handlers[id]
  end

  defp add_request_handler(%Protocol{} = protocol, method, handler) do
    %Protocol{protocol | request_handlers: Map.put(protocol.request_handlers, method, handler)}
  end

  defp delete_request_handler(%Protocol{} = protocol, method) do
    %Protocol{protocol | request_handlers: Map.delete(protocol.request_handlers, method)}
  end

  defp add_notification_handler(%Protocol{} = protocol, method, handler) do
    %Protocol{protocol | notification_handlers: Map.put(protocol.notification_handlers, method, handler)}
  end

  defp delete_notification_handler(%Protocol{} = protocol, method) do
    %Protocol{protocol | notification_handlers: Map.delete(protocol.notification_handlers, method)}
  end

  defp add_request_canceller(%Protocol{} = protocol, id, cancel) do
    %Protocol{protocol | request_cancellers: Map.put(protocol.request_cancellers, id, cancel)}
  end

  defp add_response_handler(%Protocol{} = protocol, id, ch) do
    %Protocol{protocol | response_handlers: Map.put(protocol.response_handlers, id, ch)}
  end

  defp add_progress_handler(%Protocol{} = protocol, id, handler) do
    %Protocol{protocol | progress_handlers: Map.put(protocol.progress_handlers, id, handler)}
  end

  defp increment_request_message_id(%Protocol{} = protocol) do
    %Protocol{protocol | request_message_id: protocol.request_message_id + 1}
  end

  defp invoke_request_handler(ctx, request, handler) do
    handler.(ctx, request, %RequestHandlerExtra{context: ctx})
  end

  defp send_response(%Protocol{} = protocol, id, result) do
    response = %BaseJSONRPCResponse{
      jsonrpc: "2.0",
      id: id,
      result: JSON.encode(result)
    }

    protocol.transport.send(:background, %BaseMessageResponse{response: response})
  end
end
