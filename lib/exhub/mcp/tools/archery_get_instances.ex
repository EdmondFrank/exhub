defmodule Exhub.MCP.Tools.ArcheryGetInstances do
  @moduledoc """
  MCP Tool for getting database instances from Archery.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Archery.Client

  use Anubis.Server.Component, type: :tool

  def name, do: "get_instances"

  @impl true
  def description do
    """
    Get list of database instances registered in Archery. Call this FIRST to discover available instances before running queries.
    """
  end

  schema do
    field(:db_type, :string, description: "Filter by database type: mysql, mssql, oracle, pgsql, redis, mongo, etc. (optional)")
  end

  @impl true
  def execute(params, frame) do
    db_type = Map.get(params, :db_type, "")

    client = Client.new()

    unless Client.valid_config?(client) do
      resp = Response.tool() |> Response.error("Archery configuration is incomplete. Please set ARCHERY_URL, ARCHERY_USERNAME, ARCHERY_PASSWORD")
      return {:reply, resp, frame}
    end

    case Client.get_instances(client, db_type) do
      {:ok, instances, _} ->
        result = %{
          "success" => true,
          "count" => length(instances),
          "instances" => instances
        }
        resp = Response.tool() |> Response.text(Jason.encode!(result))
        {:reply, resp, frame}

      {:error, reason, _} ->
        result = %{
          "success" => false,
          "error" => reason
        }
        resp = Response.tool() |> Response.text(Jason.encode!(result))
        {:reply, resp, frame}
    end
  end

  defp return(value), do: value
end
