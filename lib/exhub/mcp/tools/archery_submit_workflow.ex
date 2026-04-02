defmodule Exhub.MCP.Tools.ArcherySubmitWorkflow do
  @moduledoc """
  MCP Tool for submitting SQL workflow in Archery.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Archery.Client

  use Anubis.Server.Component, type: :tool

  def name, do: "submit_workflow"

  @impl true
  def description do
    """
    Submit SQL workflow for review (creates a ticket). Use for DDL/DML operations that need approval. Call get_resource_groups() and get_group_instances() first.
    """
  end

  schema do
    field(:workflow_name, {:required, :string}, description: "Name/title for the workflow")
    field(:group_name, {:required, :string}, description: "Resource group name (from get_resource_groups)")
    field(:instance_name, {:required, :string}, description: "Instance name (from get_group_instances)")
    field(:db_name, {:required, :string}, description: "Target database name")
    field(:sql_content, {:required, :string}, description: "SQL content (DDL/DML statements, NOT SELECT)")
    field(:is_backup, :boolean, description: "Whether to backup before execution (default true)")
    field(:demand_url, :string, description: "Optional demand/ticket URL for reference")
  end

  @impl true
  def execute(params, frame) do
    workflow_name = Map.get(params, :workflow_name)
    group_name = Map.get(params, :group_name)
    instance_name = Map.get(params, :instance_name)
    db_name = Map.get(params, :db_name)
    sql_content = Map.get(params, :sql_content)
    is_backup = Map.get(params, :is_backup, true)
    demand_url = Map.get(params, :demand_url, "")

    client = Client.new()

    case Client.submit_workflow(client, workflow_name, group_name, instance_name, db_name, sql_content, is_backup, demand_url) do
      {:ok, result, _} ->
        response = %{
          "success" => true,
          "workflow_name" => workflow_name,
          "result" => result
        }
        resp = Response.tool() |> Response.text(Jason.encode!(response))
        {:reply, resp, frame}

      {:error, reason, _} ->
        response = %{
          "success" => false,
          "workflow_name" => workflow_name,
          "error" => reason
        }
        resp = Response.tool() |> Response.text(Jason.encode!(response))
        {:reply, resp, frame}
    end
  end
end
