defmodule Exhub.MCP.Tools.Plan do
  @moduledoc """
  MCP Tool for planning steps — a per-session plan journal.

  Each call appends the plan to a persistent list (see
  `Exhub.MCP.Tools.Scratchpad`) and returns the accumulated plans as a JSON
  envelope (`recorded` / `scratchpad` / `next`), so the model can revise an
  explicit plan over time instead of re-deriving it from context each turn.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Scratchpad

  use Anubis.Server.Component, type: :tool

  @assigns_key :plan_steps
  @invalid_placeholder "The tool call did not contain a valid plan."
  @nudge "Execute the next step of the recorded plan. If a step proves wrong, " <>
           "call plan again with the revised remaining steps rather than " <>
           "abandoning the plan."

  def name, do: "plan"

  @impl true
  def description do
    """
    Use the tool to plan your next steps. It will not obtain new information
    or change anything, but records the plan in a persistent journal and
    returns all plans so far. Break the task into ordered steps, record them,
    then work the steps in order; revise the plan explicitly when reality
    diverges from it.
    """
  end

  schema do
    field(:plan, {:required, :string}, description: "A plan of next steps.")
  end

  @impl true
  def execute(params, frame) do
    plan = Scratchpad.normalize(Map.get(params, :plan), @invalid_placeholder)

    {entries, frame} = Scratchpad.append(frame, @assigns_key, plan)

    resp = Response.tool() |> Response.json(Scratchpad.envelope(entries, @nudge))
    {:reply, resp, frame}
  end
end
