defmodule Exhub.MCP.Tools.Think do
  @moduledoc """
  MCP Tool for thinking about something.

  This tool allows the LLM to record thoughts for complex reasoning
  or cache memory without obtaining new information or changing the database.
  """

  alias Anubis.Server.Response

  use Anubis.Server.Component, type: :tool

  def name, do: "think"

  @impl true
  def description do
    """
    Use the tool to think about something. It will not obtain new information or change the database, but just append the thought to the log. Use it when complex reasoning or some cache memory is needed.
    """
  end

  schema do
    field(:thought, {:required, :string}, description: "A thought to think about.")
  end

  @impl true
  def execute(params, frame) do
    thought = Map.get(params, :thought)

    resp = Response.tool() |> Response.text(thought)
    {:reply, resp, frame}
  end
end
