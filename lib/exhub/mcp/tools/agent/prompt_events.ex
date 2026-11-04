defmodule Exhub.MCP.Tools.Agent.PromptEvents do
  alias Anubis.Server.Response
  alias Exhub.MCP.Agent.{Events, Store}
  use Anubis.Server.Component, type: :tool

  def name, do: "agent_prompt_events"

  @impl true
  def description do
    """
    Long-poll for events from an agent session.

    Prefer `agent_prompt`, which runs the prompt to completion and returns a
    digest in one call. Use this tool when you need the live stream, and note
    that every poll is one model turn: the defaults below are chosen to avoid
    returning cheap empty batches while the agent is still thinking.

    Collection: events are collected for up to `collect_ms` (default 30000),
    returning early when a terminal event (complete/error) arrives, or after
    `idle_ms` (default 15000) of silence *once at least one event has arrived*.
    A poll with nothing to report waits the full window instead of returning an
    empty batch early.

    Output is bounded and redacted by default: boilerplate session updates and
    reasoning chunks are dropped, tool output is capped to
    `max_output_bytes`, and the whole batch to `max_bytes`. The response always
    carries `state` (`running` / `complete` / `error`), aggregated `usage`
    (`used` / `size` / `cost`) and `dropped`/`truncated` flags.

    Parameters:
    - agent_id, session_id: required
    - collect_ms: max collection window in ms (default 30000)
    - idle_ms: return early after this much silence, once events exist (default 15000)
    - include_thoughts: keep agent_thought_chunk events (default false)
    - include_raw: keep rawInput/rawOutput/content payloads uncapped (default false)
    - max_output_bytes: per-tool-output cap (default 800)
    - max_bytes: whole-response cap (default 20000)
    - slim: set false to get the untouched event list (default true)
    """
  end

  schema do
    field(:agent_id, {:required, :string}, description: "ID of the agent")
    field(:session_id, {:required, :string}, description: "Session ID to poll")

    field(:collect_ms, :integer,
      description: "Max collection window in ms (default 30000)",
      default: 30_000
    )

    field(:idle_ms, :integer,
      description: "Return early after this much silence, once events exist (default 15000)",
      default: 15_000
    )

    field(:include_thoughts, :boolean,
      description: "Include thought events (default false)",
      default: false
    )

    field(:include_raw, :boolean,
      description: "Keep rawInput/rawOutput/tool content uncapped (default false)",
      default: false
    )

    field(:max_output_bytes, :integer,
      description: "Per-tool-output cap in bytes (default 800)",
      default: 800
    )

    field(:max_bytes, :integer,
      description: "Whole-response cap in bytes (default 20000)",
      default: 20_000
    )

    field(:slim, :boolean,
      description: "Set false for the untouched event list (default true)",
      default: true
    )
  end

  @impl true
  def execute(params, frame) do
    agent_id = Map.get(params, :agent_id)
    session_id = Map.get(params, :session_id)
    collect_ms = Map.get(params, :collect_ms, 30_000)
    idle_ms = Map.get(params, :idle_ms, 15_000)

    case Store.collect_events(agent_id, session_id, collect_ms, idle_ms) do
      {:ok, events} ->
        resp_body =
          events
          |> slim_events(params)
          |> Map.merge(%{
            session_id: session_id,
            agent_id: agent_id,
            state: Store.session_state(agent_id, session_id),
            usage: Events.usage(events)
          })

        resp = Response.tool() |> Response.text(Jason.encode!(resp_body))
        {:reply, resp, frame}

      {:error, :already_waiting} ->
        resp =
          Response.tool()
          |> Response.error(
            "another agent_prompt_events call is already waiting on session '#{session_id}'; " <>
              "wait for it to return instead of polling concurrently"
          )

        {:reply, resp, frame}

      {:error, reason} ->
        {:reply, Response.tool() |> Response.error(inspect(reason)), frame}
    end
  end

  defp slim_events(events, params) do
    if Map.get(params, :slim, true) do
      slim =
        Events.slim(events,
          include_thoughts: Map.get(params, :include_thoughts, false),
          include_raw: Map.get(params, :include_raw, false),
          max_output_bytes: Map.get(params, :max_output_bytes, 800),
          max_bytes: Map.get(params, :max_bytes, 20_000)
        )

      %{
        events: slim.events,
        event_count: length(slim.events),
        dropped: slim.dropped,
        truncated: slim.truncated
      }
    else
      filtered =
        if Map.get(params, :include_thoughts, false) do
          events
        else
          Enum.reject(events, fn event ->
            get_in(event, [:update, "sessionUpdate"]) == "agent_thought_chunk"
          end)
        end

      slim_events = Enum.map(filtered, &Map.drop(&1, [:session_id, :agent_id]))
      %{events: slim_events, event_count: length(slim_events), dropped: 0, truncated: false}
    end
  end
end
