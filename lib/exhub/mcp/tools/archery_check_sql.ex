defmodule Exhub.MCP.Tools.ArcheryCheckSql do
  @moduledoc """
  MCP Tool for checking SQL before submitting a workflow in Archery.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Archery.Client

  use Anubis.Server.Component, type: :tool

  def name, do: "check_sql"

  @impl true
  def description do
    """
    Check SQL syntax and get audit result before submitting a workflow. Use this to validate DDL/DML statements.
    """
  end

  schema do
    field(:instance_name, {:required, :string}, description: "Instance name (from get_group_instances)")
    field(:db_name, {:required, :string}, description: "Target database name")
    field(:sql_content, {:required, :string}, description: "SQL content to check (DDL/DML)")
    field(:group_name, :string, description: "Resource group name (default: TiDB)")
  end

  @impl true
  def execute(params, frame) do
    instance_name = Map.get(params, :instance_name)
    db_name = Map.get(params, :db_name)
    sql_content = Map.get(params, :sql_content)
    group_name = Map.get(params, :group_name, "TiDB")

    client = Client.new()

    # First get instances to find the instance ID
    with {:ok, instances, client} <- Client.get_group_instances(client, group_name),
         {:ok, instance_id, client} <- find_instance_id(instances, instance_name, client) do

      case Client.check_sql_for_workflow(client, instance_id, db_name, sql_content) do
        {:ok, result, _} ->
          is_critical = result["is_critical"] || false
          error_count = trunc(result["error_count"] || 0)
          warning_count = trunc(result["warning_count"] || 0)

          response = %{
            "success" => true,
            "is_critical" => is_critical,
            "error_count" => error_count,
            "warning_count" => warning_count,
            "check_result" => result
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
    else
      {:error, reason, _} ->
        response = %{
          "success" => false,
          "error" => reason
        }
        resp = Response.tool() |> Response.text(Jason.encode!(response))
        {:reply, resp, frame}
    end
  end

  defp find_instance_id(instances, instance_name, client) do
    case Enum.find(instances, fn inst -> inst["instance_name"] == instance_name end) do
      nil ->
        {:error, "Instance '#{instance_name}' not found in group", client}
      inst ->
        id = trunc(inst["id"] || 0)
        if id > 0 do
          {:ok, id, client}
        else
          {:error, "Instance '#{instance_name}' has no valid ID", client}
        end
    end
  end
end
