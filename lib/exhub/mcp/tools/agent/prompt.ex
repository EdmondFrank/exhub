defmodule Exhub.MCP.Tools.Agent.Prompt do
  @moduledoc """
  MCP Tool: agent_prompt

  Runs a prompt to completion in one call. This is the tool an orchestrator
  should reach for: `agent_prompt_start` + `agent_prompt_events` costs one model
  turn per poll, and a busy agent produces many empty or partial batches.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Agent.{Events, Runner}
  use Anubis.Server.Component, type: :tool

  def name, do: "agent_prompt"

  @impl true
  def description do
    """
    Send a prompt and block until the agent finishes, then return a compact digest.

    The digest contains the final answer, `stop_reason`, token usage
    (`used` / `size` / `cost`), a tool activity trace (title, kind, status,
    path, capped output), changed files, errors and any permission requests.
    This is usually all an orchestrator needs — prefer it over the
    `agent_prompt_start` + `agent_prompt_events` loop.

    Parameters:
    - agent_id, session_id, content: as for agent_prompt_start
    - timeout_ms: max wait (default 300000, capped at 570000)
    - format: "digest" (default) or "full" (digest plus the raw event list)
    - max_answer_bytes: cap on the returned answer (default 4000)
    - include_thoughts: keep reasoning events in `format: "full"` (default false)

    Returns `status: "complete" | "timeout" | "error"`. A permission prompt
    from the agent blocks this call until it is resolved (see
    `agent_grant_permission`) or the timeout expires.
    """
  end

  schema do
    field(:agent_id, {:required, :string}, description: "ID of the agent")
    field(:session_id, {:required, :string}, description: "Session ID")

    field(:content, {:required, :string}, description: "Prompt text to send to the agent")

    field(:timeout_ms, :integer,
      description: "Max wait in ms (default 300000, capped at 570000)",
      default: 300_000
    )

    field(:format, {:enum, ["digest", "full"]},
      description: "What to return: the digest only, or digest plus raw events",
      default: "digest"
    )

    field(:max_answer_bytes, :integer,
      description: "Cap on the returned answer text (default 4000 bytes)",
      default: 4_000
    )

    field(:include_thoughts, :boolean,
      description: "Keep agent_thought_chunk events when format is \"full\"",
      default: false
    )
  end

  @impl true
  def execute(params, frame) do
    agent_id = Map.fetch!(params, :agent_id)
    session_id = Map.fetch!(params, :session_id)
    content = Map.fetch!(params, :content)

    opts = [
      timeout_ms: Map.get(params, :timeout_ms, 300_000),
      max_answer_bytes: Map.get(params, :max_answer_bytes, 4_000)
    ]

    case Runner.prompt(agent_id, session_id, content, opts) do
      {:ok, %{digest: digest, events: events}} ->
        payload =
          case Map.get(params, :format, "digest") do
            "full" ->
              slim =
                Events.slim(events, include_thoughts: Map.get(params, :include_thoughts, false))

              Map.put(digest, "recent_events", slim.events)

            _ ->
              digest
          end

        {:reply, Response.tool() |> Response.text(Jason.encode!(payload)), frame}

      {:error, reason} ->
        {:reply, Response.tool() |> Response.error(reason), frame}
    end
  end
end
