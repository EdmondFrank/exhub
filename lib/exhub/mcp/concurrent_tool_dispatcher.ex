defmodule Exhub.MCP.ConcurrentToolDispatcher do
  @moduledoc """
  Handles tools/call requests concurrently, bypassing the session GenServer.

  When a tools/call request is received, this module:
  1. Finds the tool definition from the server module
  2. Validates input parameters
  3. Executes the tool handler in a Task.Supervisor child
  4. Replies directly to the HTTP client

  This prevents one long-running tool from blocking other requests to the same
  MCP server session.

  ## Why bypass the session?

  The `Anubis.Server.Session` GenServer handles ALL MCP requests via a
  synchronous `handle_call({:mcp_request, ...})`. When a tool handler is
  executing (e.g., `execute_command` waiting 30s), the session GenServer is
  blocked — no other requests to the same MCP server can be processed.

  By intercepting tools/call requests in the LazyPlug (before they reach the
  session GenServer), we can execute tool handlers in separate Tasks, keeping
  the session GenServer free for other requests (tools/list, initialize, etc.).

  ## Safety

  - No Exhub tool handler uses `frame.assigns` or `frame.context` for state —
    they all use external GenServers (ProcessStore, AgentStore, etc.).
  - No tool handler calls session notification functions (`send(self(), ...)`).
  - A fresh Frame with proper Context is constructed for each tool call.
  """

  alias Anubis.MCP.Error
  alias Anubis.MCP.Message
  alias Anubis.Server.Component.Schema
  alias Anubis.Server.Component.Tool
  alias Anubis.Server.Context
  alias Anubis.Server.Frame
  alias Anubis.Server.Response
  alias Anubis.Server.Transport.StreamableHTTP
  alias Exhub.MCP.ServerHelpers

  require Logger

  @session_header "mcp-session-id"

  @doc """
  Attempts to handle a tools/call request concurrently.

  Returns `{:handled, conn}` if the request was a tools/call and was handled,
  `{:not_handled, conn}` otherwise.
  """
  @spec maybe_handle(Plug.Conn.t(), keyword()) ::
          {:handled, Plug.Conn.t()} | {:not_handled, Plug.Conn.t()}
  def maybe_handle(conn, opts) do
    server = Keyword.fetch!(opts, :server)
    timeout = Keyword.get(opts, :request_timeout, 600_000)

    with %{method: "POST"} <- conn,
         body when is_map(body) <- conn.body_params,
         %{"method" => "tools/call", "id" => request_id} <- body,
         %{"name" => tool_name} <- Map.get(body, "params", %{}) do
      arguments = Map.get(body["params"], "arguments", %{})
      session_id = get_session_id(conn)
      wants_sse = wants_sse?(conn)

      conn =
        handle_concurrent(
          conn,
          server,
          timeout,
          request_id,
          tool_name,
          arguments,
          session_id,
          wants_sse,
          opts
        )

      {:handled, conn}
    else
      _ -> {:not_handled, conn}
    end
  end

  # --- Concurrent tool execution ---

  defp handle_concurrent(conn, server, timeout, request_id, tool_name, arguments, session_id, wants_sse, opts) do
    frame = build_frame(conn, session_id)

    # Get tools and apply filtering (same as ServerHelpers.handle_request_with_filtered_tools)
    tools = server.__components__(:tool) ++ Frame.get_tools(frame)
    headers = normalize_headers(conn.req_headers)
    filtered_tools = ServerHelpers.filter_tools_by_headers(tools, headers)

    case find_tool(filtered_tools, tool_name) do
      nil ->
        error = Error.protocol(:invalid_params, %{message: "Tool not found: #{tool_name}"})
        reply_error(conn, error, request_id, session_id, wants_sse, server, opts)

      tool ->
        case validate_params(arguments, tool) do
          {:ok, params} ->
            execute_and_reply(
              conn,
              server,
              tool,
              params,
              frame,
              timeout,
              request_id,
              session_id,
              wants_sse,
              opts
            )

          {:error, error} ->
            reply_error(conn, error, request_id, session_id, wants_sse, server, opts)
        end
    end
  end

  defp execute_and_reply(conn, server, tool, params, frame, timeout, request_id, session_id, wants_sse, opts) do
    task =
      Task.Supervisor.async_nolink(Exhub.MCP.ToolTaskSupervisor, fn ->
        execute_tool(server, tool, params, frame)
      end)

    case Task.yield(task, timeout) do
      {:ok, {:ok, result}} ->
        reply_result(conn, result, request_id, session_id, wants_sse, server, opts)

      {:ok, {:error, error}} ->
        reply_error(conn, error, request_id, session_id, wants_sse, server, opts)

      nil ->
        Task.shutdown(task, :brutal_kill)
        error = Error.execution("Tool execution timed out after #{timeout}ms")
        reply_error(conn, error, request_id, session_id, wants_sse, server, opts)

      {:exit, reason} ->
        Logger.error(
          "[ConcurrentToolDispatcher] Tool '#{tool.name}' crashed: #{inspect(reason)}"
        )

        error = Error.execution("Tool execution failed: #{format_exit_reason(reason)}")
        reply_error(conn, error, request_id, session_id, wants_sse, server, opts)
    end
  end

  # Replicates Anubis.Server.Handlers.Tools.forward_to/4 + maybe_validate_output_schema/4
  defp execute_tool(server, tool, params, frame) do
    case tool.handler do
      nil ->
        # Tool with no handler module — use server.handle_tool_call/3
        case server.handle_tool_call(tool.name, params, frame) do
          {:reply, %Response{} = response, _frame} ->
            {:ok, validate_output(tool, response)}

          {:noreply, _frame} ->
            {:ok, {:reply, %{"content" => [], "isError" => false}, nil}}

          {:error, %Error{} = error, _frame} ->
            {:error, error}
        end

      handler ->
        case handler.execute(params, frame) do
          {:reply, %Response{} = response, _frame} ->
            {:ok, validate_output(tool, response)}

          {:noreply, _frame} ->
            {:ok, {:reply, %{"content" => [], "isError" => false}, nil}}

          {:error, %Error{} = error, _frame} ->
            {:error, error}
        end
    end
  rescue
    e ->
      {:error, Error.execution("Tool execution failed: #{Exception.message(e)}")}
  end

  # Replicates Anubis.Server.Handlers.Tools.maybe_validate_output_schema/4
  defp validate_output(%Tool{output_schema: nil}, resp) do
    {:reply, Response.to_protocol(resp), nil}
  end

  defp validate_output(_tool, %Response{isError: true} = resp) do
    {:reply, Response.to_protocol(resp), nil}
  end

  defp validate_output(%Tool{} = tool, %Response{structured_content: nil}) do
    error = Error.execution("Tool doesn't conform to its output schema", %{tool_name: tool.name})
    throw({:error, error})
  end

  defp validate_output(%Tool{} = tool, %Response{} = resp) do
    case tool.validate_output.(resp.structured_content) do
      {:ok, _} ->
        {:reply, Response.to_protocol(resp), nil}

      {:error, errors} ->
        error = Error.execution("Tool doesn't conform to its output schema", %{errors: errors})
        throw({:error, error})
    end
  end

  # --- Reply helpers ---

  defp reply_result(conn, {:reply, result, _frame}, request_id, session_id, wants_sse, server, opts) do
    response = Message.build_response(result, request_id)
    encoded = JSON.encode!(response)
    send_response(conn, encoded, session_id, wants_sse, server, opts)
  end

  defp reply_error(conn, %Error{} = error, request_id, session_id, wants_sse, server, opts) do
    error_map = Error.build_json_rpc(error, request_id)
    encoded = JSON.encode!(error_map)
    send_response(conn, encoded, session_id, wants_sse, server, opts)
  end

  defp send_response(conn, encoded, session_id, false = _wants_sse, _server, _opts) do
    send_json_response(conn, encoded, session_id)
  end

  defp send_response(conn, encoded, session_id, true = _wants_sse, server, _opts) do
    transport = Anubis.Server.Registry.transport_name(server, :streamable_http)

    case StreamableHTTP.get_sse_handler(transport, session_id) do
      pid when is_pid(pid) ->
        if Process.alive?(pid) do
          send(pid, {:sse_message, encoded})

          conn
          |> Plug.Conn.put_resp_content_type("application/json")
          |> maybe_add_session_header(session_id)
          |> Plug.Conn.send_resp(202, "{}")
        else
          send_json_response(conn, encoded, session_id)
        end

      _ ->
        # No SSE handler — fall back to JSON response
        send_json_response(conn, encoded, session_id)
    end
  end

  defp send_json_response(conn, encoded, session_id) do
    conn
    |> Plug.Conn.put_resp_content_type("application/json")
    |> maybe_add_session_header(session_id)
    |> Plug.Conn.send_resp(200, encoded)
  end

  # --- Frame construction ---

  defp build_frame(conn, session_id) do
    context = %Context{
      session_id: session_id,
      client_info: nil,
      headers: normalize_headers(conn.req_headers),
      remote_ip: conn.remote_ip
    }

    Frame.new(conn.assigns)
    |> Map.put(:context, context)
  end

  # --- Param validation (replicates Anubis.Server.Handlers.Tools.validate_params/3) ---

  defp validate_params(_, %Tool{validate_input: nil}), do: {:ok, %{}}

  defp validate_params(params, %Tool{} = tool) do
    case tool.validate_input.(params) do
      {:ok, validated} -> {:ok, validated}
      {:error, errors} -> {:error, Error.protocol(:invalid_params, %{message: Schema.format_errors(errors)})}
    end
  end

  # --- Helpers ---

  defp find_tool(tools, name), do: Enum.find(tools, &(&1.name == name))

  defp get_session_id(conn) do
    case Plug.Conn.get_req_header(conn, @session_header) do
      [session_id | _] when is_binary(session_id) and session_id != "" -> session_id
      _ -> nil
    end
  end

  defp wants_sse?(conn) do
    conn
    |> Plug.Conn.get_req_header("accept")
    |> List.first("")
    |> String.contains?("text/event-stream")
  end

  defp normalize_headers(req_headers) when is_list(req_headers) do
    Map.new(req_headers, fn {k, v} -> {String.downcase(k), v} end)
  end

  defp normalize_headers(_), do: %{}

  defp maybe_add_session_header(conn, session_id) do
    if Plug.Conn.get_req_header(conn, @session_header) == [] do
      Plug.Conn.put_resp_header(conn, @session_header, session_id)
    else
      conn
    end
  end

  defp format_exit_reason({:noproc, _}), do: "process exited unexpectedly"
  defp format_exit_reason({:timeout, _}), do: "process timed out"
  defp format_exit_reason({:killed, _}), do: "process was killed"
  defp format_exit_reason(reason), do: inspect(reason)
end
