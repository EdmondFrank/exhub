defmodule Exhub.MCP.Agent.Runner do
  @moduledoc """
  Agent lifecycle and prompt execution shared by the ACP MCP tools.

  `agent_initialize` / `agent_new_session` and the one-shot `agent_task` tool all
  need the same three steps — spawn (if needed), open a session, run a prompt.
  Keeping them here means the MCP server address handed to a subagent, the
  session bookkeeping and the status mirroring have a single implementation.
  """

  alias Exhub.MCP.Agent.{Config, Events, Status, Store}

  @default_prompt_timeout 300_000
  @max_prompt_timeout 570_000

  # How long to keep draining the event queue after `Client.prompt/4` returns.
  @drain_idle_ms 600

  def default_prompt_timeout, do: @default_prompt_timeout
  def max_prompt_timeout, do: @max_prompt_timeout

  ## Lifecycle

  @doc "Returns a live agent entry, spawning the process when needed."
  @spec ensure_agent(String.t()) :: {:ok, map()} | {:error, String.t()}
  def ensure_agent(agent_id) do
    case Store.get(agent_id) do
      {:ok, entry} ->
        if Process.alive?(entry.client) do
          {:ok, entry}
        else
          # Stale entry — clean up and fall through to a fresh start.
          Store.unregister(agent_id)
          start_agent(agent_id)
        end

      {:error, :not_found} ->
        start_agent(agent_id)
    end
  end

  @doc "Spawns an ACP agent process and registers it."
  @spec start_agent(String.t()) :: {:ok, map()} | {:error, String.t()}
  def start_agent(agent_id) do
    case Config.get_agent_command(agent_id) do
      {:ok, cmd_list} ->
        result =
          try do
            ExMCP.ACP.Client.start_link(
              command: cmd_list,
              handler: Exhub.MCP.Agent.Handler,
              handler_opts: [agent_id: agent_id]
            )
          catch
            :exit, reason -> {:error, {:exit, reason}}
          end

        case result do
          {:ok, client_pid} ->
            if Process.alive?(client_pid) do
              # Detach the client from the caller. Hub built-in tool calls run in
              # a short-lived Task, and a GenServer started with start_link/1
              # terminates when its starter exits — which would take the agent
              # subprocess down as soon as the tool returns. The Store monitors
              # the client and `agent_shutdown` stops it.
              Process.unlink(client_pid)

              case Store.register(agent_id, client_pid, cmd_list) do
                {:ok, entry} -> {:ok, entry}
                {:error, :already_exists} -> {:error, "Agent '#{agent_id}' is already running."}
              end
            else
              {:error, "Agent '#{agent_id}' started but died immediately"}
            end

          {:error, {:exit, {:timeout, _}}} ->
            Store.unregister(agent_id)

            {:error,
             "Agent '#{agent_id}' timed out during initialization. The agent process may be slow to start."}

          {:error, {:exit, reason}} ->
            Store.unregister(agent_id)
            {:error, "Agent '#{agent_id}' crashed during initialization: #{inspect(reason)}"}

          {:error, reason} ->
            {:error, "Failed to start agent: #{inspect(reason)}"}
        end

      {:error, :not_found} ->
        {:error, "Agent '#{agent_id}' not found in config."}
    end
  end

  @doc """
  Opens a session, optionally injecting the ExHub bus.

  Returns the session id and the exact `mcpServers` payload that was sent, so a
  caller can see whether the runtime accepted it.
  """
  @spec new_session(String.t(), keyword()) ::
          {:ok, String.t(), [map()]} | {:error, String.t()}
  def new_session(agent_id, opts \\ []) do
    cwd = Keyword.get(opts, :cwd) || File.cwd!()
    servers = build_mcp_servers(agent_id, opts)

    with {:ok, entry} <- fetch_agent(agent_id) do
      case ExMCP.ACP.Client.new_session(entry.client, cwd, mcp_servers: servers) do
        {:ok, result} ->
          case Map.get(result, "sessionId") do
            nil ->
              {:error, "Agent '#{agent_id}' returned no session id: #{inspect(result)}"}

            session_id ->
              Store.add_session(agent_id, session_id)
              {:ok, session_id, servers}
          end

        {:error, reason} ->
          {:error, "Failed to create session: #{inspect(reason)}"}
      end
    end
  end

  @doc """
  Runs a prompt to completion and returns the terminal digest plus the raw events.

  Blocks for up to `:timeout_ms` (default 5 min, max 570 s — the hub request
  budget). On timeout the prompt is cancelled and `status: "timeout"` returned.
  """
  @spec prompt(String.t(), String.t(), String.t(), keyword()) ::
          {:ok, %{digest: map(), events: list()}} | {:error, String.t()}
  def prompt(agent_id, session_id, content, opts \\ []) do
    timeout_ms = normalize_timeout(Keyword.get(opts, :timeout_ms, @default_prompt_timeout))

    with {:ok, entry} <- fetch_agent(agent_id) do
      # Anything already queued belongs to a previous turn.
      _ = Store.pop_events(agent_id, session_id)
      Store.mark_running(agent_id, session_id)
      Status.mirror(agent_id, "working")
      Store.set_status(agent_id, "working")

      started = System.monotonic_time(:millisecond)

      result =
        try do
          ExMCP.ACP.Client.prompt(entry.client, session_id, content, timeout: timeout_ms)
        catch
          :exit, {:timeout, _} -> {:error, :timeout}
          :exit, reason -> {:error, {:exit, reason}}
        end

      elapsed_ms = System.monotonic_time(:millisecond) - started

      {status, stop_reason, message} =
        case result do
          {:ok, response} ->
            {"complete", Map.get(response, "stopReason"), nil}

          {:error, :timeout} ->
            _ = safe_cancel(entry.client, session_id)

            {"timeout", nil, "prompt did not finish within #{timeout_ms} ms and was cancelled"}

          {:error, reason} ->
            {"error", nil, inspect(reason)}
        end

      events = drain(agent_id, session_id)
      Store.mark_state(agent_id, session_id, status)
      Status.mirror(agent_id, if(status == "complete", do: "idle", else: "error"))
      Store.set_status(agent_id, if(status == "complete", do: "idle", else: status))

      digest =
        events
        |> Events.digest(
          agent_id: agent_id,
          session_id: session_id,
          status: status,
          stop_reason: stop_reason,
          elapsed_ms: elapsed_ms,
          max_answer_bytes: Keyword.get(opts, :max_answer_bytes, 4_000)
        )
        |> maybe_put("message", message)

      {:ok, %{digest: digest, events: events}}
    end
  end

  @doc "Runs a fresh session end-to-end: spawn, open, prompt, digest."
  @spec run_task(String.t(), String.t(), keyword()) ::
          {:ok, map()} | {:error, String.t()}
  def run_task(agent_id, content, opts \\ []) do
    with {:ok, _entry} <- ensure_agent(agent_id),
         {:ok, session_id, _servers} <- new_session(agent_id, opts),
         {:ok, %{digest: digest}} <- prompt(agent_id, session_id, content, opts) do
      keep = Keyword.get(opts, :keep_session, false)
      unless keep, do: Store.remove_session(agent_id, session_id)

      {:ok,
       digest
       |> Map.put("agent_id", agent_id)
       |> Map.put("session_id", session_id)
       |> Map.put("session_kept", keep)}
    end
  end

  ## MCP servers

  @doc """
  Normalizes the `mcpServers` payload for an ACP `session/new`.

  ACP defines three transports, and the shape matters:

      %{"name" => "x", "command" => "cmd", "args" => [...], "env" => [%{"name" => "K", "value" => "V"}]}
      %{"name" => "x", "type" => "http", "url" => "https://...", "headers" => [%{"name" => "K", "value" => "V"}]}
      %{"name" => "x", "type" => "sse",  "url" => "https://...", "headers" => [...]}

  Older ExHub builds sent `%{"name", "uri"}`, which every spec-compliant agent
  rejects (`Unsupported MCP server transport: undefined`). `uri` is still
  accepted as an alias for `url`, and `headers`/`env` maps are converted to the
  array-of-{name,value} form the spec requires.
  """
  def build_mcp_servers(agent_id, opts \\ []) do
    extra =
      opts
      |> Keyword.get(:mcp_servers, [])
      |> List.wrap()
      |> Enum.map(&normalize_server/1)
      |> Enum.reject(&is_nil/1)

    if Keyword.get(opts, :expose_bus, false) do
      [bus_server(agent_id) | extra]
    else
      extra
    end
  end

  defp bus_server(agent_id) do
    port = Application.get_env(:exhub, :port, 9069)

    %{
      "name" => "exhub-bus",
      "type" => "http",
      "url" => "http://127.0.0.1:#{port}/agent-bus/mcp?as=#{URI.encode_www_form(agent_id)}",
      # pi's ACP client dereferences `headers` unconditionally; the spec allows
      # it to be absent, so always send an (empty) array.
      "headers" => []
    }
  end

  defp normalize_server(server) when is_map(server) do
    name = fetch(server, :name)

    cond do
      not is_binary(name) or name == "" ->
        nil

      command = fetch(server, :command) ->
        %{
          "name" => name,
          "command" => command,
          "args" => server |> fetch(:args) |> List.wrap(),
          "env" => normalize_pairs(fetch(server, :env))
        }

      url = fetch(server, :url) || fetch(server, :uri) ->
        %{
          "name" => name,
          "type" => fetch(server, :type) || "http",
          "url" => url,
          "headers" => normalize_pairs(fetch(server, :headers))
        }

      true ->
        nil
    end
  end

  defp normalize_server(_server), do: nil

  defp normalize_pairs(nil), do: []
  defp normalize_pairs(pairs) when is_list(pairs), do: Enum.map(pairs, &normalize_pair/1)

  defp normalize_pairs(map) when is_map(map) do
    Enum.map(map, fn {key, value} -> %{"name" => to_string(key), "value" => to_string(value)} end)
  end

  defp normalize_pairs(_), do: []

  defp normalize_pair(%{"name" => name, "value" => value}),
    do: %{"name" => to_string(name), "value" => to_string(value)}

  defp normalize_pair(%{name: name, value: value}),
    do: %{"name" => to_string(name), "value" => to_string(value)}

  defp normalize_pair({name, value}),
    do: %{"name" => to_string(name), "value" => to_string(value)}

  defp normalize_pair(other), do: other

  ## Internals

  defp fetch_agent(agent_id) do
    case Store.get(agent_id) do
      {:ok, entry} -> {:ok, entry}
      {:error, :not_found} -> {:error, "Agent '#{agent_id}' is not running."}
    end
  end

  # A single drain: the ACP client casts every session update before it replies
  # to the prompt call, so the queue already holds the full turn; the idle
  # window only catches stragglers.
  defp drain(agent_id, session_id) do
    case Store.collect_events(agent_id, session_id, @drain_idle_ms, @drain_idle_ms) do
      {:ok, events} -> events
      {:error, _} -> []
    end
  end

  defp safe_cancel(client, session_id) do
    ExMCP.ACP.Client.cancel(client, session_id)
  catch
    _kind, _reason -> :ok
  end

  defp normalize_timeout(ms) when is_integer(ms) and ms > 0 do
    ms |> min(@max_prompt_timeout) |> max(1_000)
  end

  defp normalize_timeout(_), do: @default_prompt_timeout

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)

  defp fetch(server, key) do
    Map.get(server, key) || Map.get(server, to_string(key))
  end
end
