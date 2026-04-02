defmodule Exhub.MCP.Tools.ArcherySqlCheck do
  @moduledoc """
  MCP Tool for checking SQL syntax in Archery.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Archery.Client

  use Anubis.Server.Component, type: :tool

  def name, do: "sql_check"

  @impl true
  def description do
    """
    Check SQL syntax and get optimization suggestions. Use this to validate SQL before submitting for review.
    """
  end

  schema do
    field(:sql_content, {:required, :string}, description: "SQL statement to check")
    field(:instance_name, {:required, :string}, description: "Target instance name")
    field(:db_name, :string, description: "Target database name (optional)")
  end

  @impl true
  def execute(params, frame) do
    sql_content = Map.get(params, :sql_content)
    instance_name = Map.get(params, :instance_name)
    db_name = Map.get(params, :db_name, "")

    client = Client.new()

    case Client.sql_check(client, sql_content, instance_name, db_name) do
      {:ok, result, _} ->
        response = %{
          "success" => true,
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
  end
end
