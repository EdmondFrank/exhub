defmodule Exhub.MCP.Tools.AgentBus.Await do
  @moduledoc """
  MCP Tool: agent_await

  Fan-in without polling. Blocks until the named agents stop working, need
  attention, or a message for the caller arrives.
  """

  alias Exhub.Agent.Registry
  alias Exhub.MCP.Tools.AgentBus.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "agent_await"

  @poll_ms 250
  @max_wait 120_000
  @settled ~w(idle done error offline blocked waiting_input)

  @impl true
  def description do
    """
    Wait for other agents to finish or need attention.

    Returns as soon as:
    - any/all (per `mode`) of `agents` are settled — idle, done, error, offline,
      blocked or waiting_input (the last two mean: they need a human or a peer); or
    - a message addressed to you arrives.

    Prefer this over repeatedly calling agent_list. `wait_ms` is capped at
    120000; on timeout the response lists what is still pending.
    """
  end

  schema do
    field(:agents, {:required, {:list, :string}}, description: "Agent ids to wait for")

    field(:mode, {:enum, ["all", "any"]},
      description: "Wait for every agent (all) or the first one (any)",
      default: "all"
    )

    field(:wait_ms, :integer,
      description: "Max wait in ms (default 30000, max 120000)",
      default: 30_000
    )

    field(:as, :string, description: "Caller id; optional when the bus URL carries `?as=<id>`")
  end

  @impl true
  def execute(params, frame) do
    with {:ok, caller} <- Helpers.require_caller(params, frame) do
      agents =
        params
        |> Map.get(:agents, [])
        |> List.wrap()
        |> Enum.map(fn agent -> agent |> to_string() |> String.trim_leading("@") end)
        |> Enum.reject(&(&1 == ""))

      mode = Map.get(params, :mode, "all")
      wait_ms = params |> Map.get(:wait_ms, 30_000) |> min(@max_wait) |> max(0)

      if agents == [] do
        Helpers.ok(frame, %{
          reason: "no_agents",
          waited_ms: 0,
          agents: [],
          reached: [],
          pending: []
        })
      else
        started = System.monotonic_time(:millisecond)
        result = loop(agents, mode, caller, started + wait_ms)

        Helpers.ok(
          frame,
          Map.put(result, :waited_ms, System.monotonic_time(:millisecond) - started)
        )
      end
    else
      {:error, reason} -> Helpers.error(frame, reason)
    end
  end

  defp loop(agents, mode, caller, deadline) do
    snapshot = snapshot(agents, caller)
    reached = Enum.filter(agents, fn agent -> settled?(snapshot.statuses[agent]) end)

    cond do
      snapshot.unread > 0 ->
        finish(snapshot, reached, "message")

      mode == "any" and reached != [] ->
        finish(snapshot, reached, "status")

      mode == "all" and length(reached) == length(agents) ->
        finish(snapshot, reached, "status")

      System.monotonic_time(:millisecond) >= deadline ->
        finish(snapshot, reached, "timeout")

      true ->
        Process.sleep(@poll_ms)
        loop(agents, mode, caller, deadline)
    end
  end

  defp finish(snapshot, reached, reason) do
    %{
      reason: reason,
      reached: reached,
      pending: Map.keys(snapshot.statuses) -- reached,
      statuses: snapshot.statuses,
      unread: snapshot.unread
    }
  end

  defp snapshot(agents, caller) do
    statuses =
      Map.new(agents, fn agent -> {agent, status_of(agent)} end)

    %{statuses: statuses, unread: unread(caller)}
  end

  defp status_of(agent) do
    running = Helpers.running?(agent)

    case Registry.get(agent) do
      {:ok, record} ->
        %{status: record.status, detail: record.detail, running: running, registered: true}

      {:error, :not_found} ->
        %{
          status: if(running, do: "working", else: "not_running"),
          detail: nil,
          running: running,
          registered: false
        }
    end
  end

  defp settled?(%{status: status}), do: status in @settled or status == "not_running"
  defp settled?(_), do: false

  defp unread(caller) do
    Exhub.Agent.Bus.unread(caller)
    |> Map.values()
    |> Enum.sum()
  rescue
    _ -> 0
  catch
    :exit, _ -> 0
  end
end
