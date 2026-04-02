defmodule Exhub.MCP.Tools.ArcherySqlReview do
  @moduledoc """
  MCP Tool for submitting SQL for audit review in Archery.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Archery.Client

  use Anubis.Server.Component, type: :tool

  def name, do: "sql_review"

  @impl true
  def description do
    """
    Submit SQL for audit review (creates a workflow/ticket). Use this for DDL/DML operations that need approval.
    """
  end

  schema do
    field(:sql_content, {:required, :string}, description: "SQL statement to submit for review")
    field(:instance_name, {:required, :string}, description: "Target instance name")
    field(:db_name, {:required, :string}, description: "Target database name")
    field(:workflow_name, :string, description: "Optional name for the workflow/ticket")
  end

  @impl true
  def execute(params, frame) do
    sql_content = Map.get(params, :sql_content)
    instance_name = Map.get(params, :instance_name)
    db_name = Map.get(params, :db_name)
    workflow_name = Map.get(params, :workflow_name, "")

    client = Client.new()

    case Client.sql_review(client, sql_content, instance_name, db_name, workflow_name) do
      {:ok, result, _} ->
        response = %{
          "success" => true,
          "review_result" => result
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
