defmodule Exhub.MCP.Tools.ArcheryGetWorkflowDetail do
  @moduledoc """
  MCP Tool for getting workflow details from Archery.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Archery.Client

  use Anubis.Server.Component, type: :tool

  def name, do: "get_workflow_detail"

  @impl true
  def description do
    """
    Get details of a specific workflow/ticket.
    """
  end

  schema do
    field(:workflow_id, {:required, :integer}, description: "Workflow ID (from get_workflow_list)")
  end

  @impl true
  def execute(params, frame) do
    workflow_id = Map.get(params, :workflow_id)

    client = Client.new()

    case Client.get_workflow_detail(client, workflow_id) do
      {:ok, workflow, _} ->
        response = %{
          "success" => true,
          "workflow" => workflow
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
