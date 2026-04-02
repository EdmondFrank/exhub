defmodule Exhub.MCP.Tools.ArcheryGetDatabases do
  @moduledoc """
  MCP Tool for getting databases from an Archery instance.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Archery.Client

  use Anubis.Server.Component, type: :tool

  def name, do: "get_databases"

  @impl true
  def description do
    """
    Get list of databases for a specific instance. Call this after get_instances() to see available databases.
    """
  end

  schema do
    field(:instance_name, {:required, :string}, description: "Name of the database instance (from get_instances)")
  end

  @impl true
  def execute(params, frame) do
    instance_name = Map.get(params, :instance_name)

    client = Client.new()

    case Client.get_databases(client, instance_name) do
      {:ok, databases, _} ->
        result = %{
          "success" => true,
          "instance_name" => instance_name,
          "count" => length(databases),
          "databases" => databases
        }
        resp = Response.tool() |> Response.text(Jason.encode!(result))
        {:reply, resp, frame}

      {:error, reason, _} ->
        result = %{
          "success" => false,
          "instance_name" => instance_name,
          "error" => reason
        }
        resp = Response.tool() |> Response.text(Jason.encode!(result))
        {:reply, resp, frame}
    end
  end
end
