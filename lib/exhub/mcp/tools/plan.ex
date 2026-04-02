defmodule Exhub.MCP.Tools.Plan do
  @moduledoc """
  MCP Tool for planning steps.

  This tool allows the LLM to plan steps for complex reasoning
  or cache memory without obtaining new information or changing the database.
  """

  alias Anubis.Server.Response

  use Anubis.Server.Component, type: :tool

  def name, do: "plan"

  @impl true
  def description do
    """
    Use the tool to plan your steps. It will not obtain new information or change the database, but just append the thought to the log. Use it when complex reasoning or some cache memory is needed.
    """
  end

  schema do
    field(:plan, {:required, :string}, description: "A plan of next steps.")
  end

  @impl true
  def execute(params, frame) do
    plan = Map.get(params, :plan)

    resp = Response.tool() |> Response.text(plan)
    {:reply, resp, frame}
  end
end
