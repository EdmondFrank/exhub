defmodule Exhub.MCP.Tools.ArcheryQueryExecute do
  @moduledoc """
  MCP Tool for executing read-only SQL queries in Archery.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Archery.Client

  use Anubis.Server.Component, type: :tool

  def name, do: "query_execute"

  @impl true
  def description do
    """
    Execute a read-only SQL query (SELECT only). For DDL/DML operations use submit_workflow.
    """
  end

  schema do
    field(:sql_content, {:required, :string}, description: "SQL SELECT statement to execute")
    field(:instance_name, {:required, :string}, description: "Target instance name (from get_instances)")
    field(:db_name, {:required, :string}, description: "Target database name (from get_databases)")
    field(:limit, :integer, description: "Maximum rows to return (default 100, max 1000)")
  end

  @impl true
  def execute(params, frame) do
    sql_content = Map.get(params, :sql_content)
    instance_name = Map.get(params, :instance_name)
    db_name = Map.get(params, :db_name)
    limit = min(Map.get(params, :limit, 100), 1000)

    client = Client.new()

    case Client.query_execute(client, sql_content, instance_name, db_name, limit) do
      {:ok, result, _} ->
        response = %{
          "success" => true,
          "instance_name" => instance_name,
          "db_name" => db_name,
          "result" => result
        }
        resp = Response.tool() |> Response.text(Jason.encode!(response))
        {:reply, resp, frame}

      {:error, reason, _} ->
        response = %{
          "success" => false,
          "instance_name" => instance_name,
          "db_name" => db_name,
          "error" => reason
        }
        resp = Response.tool() |> Response.text(Jason.encode!(response))
        {:reply, resp, frame}
    end
  end
end
