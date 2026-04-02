defmodule Exhub.MCP.Tools.ArcheryGetWorkflowList do
  @moduledoc """
  MCP Tool for getting SQL workflow list from Archery.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Archery.Client

  use Anubis.Server.Component, type: :tool

  def name, do: "get_workflow_list"

  @impl true
  def description do
    """
    Get list of SQL workflows/tickets.
    """
  end

  schema do
    field(:status, :string, description: "Filter by status: pending, executing, finished, rejected, etc. (optional)")
    field(:start_date, :string, description: "Start date filter in YYYY-MM-DD format (optional)")
    field(:end_date, :string, description: "End date filter in YYYY-MM-DD format (optional)")
    field(:limit, :integer, description: "Maximum workflows to return (default 50)")
  end

  @impl true
  def execute(params, frame) do
    status = Map.get(params, :status, "")
    start_date = Map.get(params, :start_date, "")
    end_date = Map.get(params, :end_date, "")
    limit = Map.get(params, :limit, 50)

    client = Client.new()

    case Client.get_workflow_list(client, status, start_date, end_date, limit) do
      {:ok, workflows, _} ->
        response = %{
          "success" => true,
          "count" => length(workflows),
          "workflows" => workflows
        }
        resp = Response.tool() |> Response.text(Jason.encode!(response))
        {:reply, resp, frame}

      {:error, reason, _} ->
        response = %{
          "success" => false,
          "error" => reason
        }
        resp = Response.tool() |> Response.text(Jason.encode!(response))
        {:reply, resp, frame}
    end
  end
end
