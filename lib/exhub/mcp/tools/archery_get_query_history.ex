defmodule Exhub.MCP.Tools.ArcheryGetQueryHistory do
  @moduledoc """
  MCP Tool for getting query execution history from Archery.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Archery.Client

  use Anubis.Server.Component, type: :tool

  def name, do: "get_query_history"

  @impl true
  def description do
    """
    Get SQL query execution history.
    """
  end

  schema do
    field(:instance_name, :string, description: "Filter by instance name (optional)")
    field(:db_name, :string, description: "Filter by database name (optional)")
    field(:limit, :integer, description: "Maximum records to return (default 50)")
  end

  @impl true
  def execute(params, frame) do
    instance_name = Map.get(params, :instance_name, "")
    db_name = Map.get(params, :db_name, "")
    limit = Map.get(params, :limit, 50)

    client = Client.new()

    case Client.get_query_history(client, instance_name, db_name, limit) do
      {:ok, history, _} ->
        response = %{
          "success" => true,
          "count" => length(history),
          "history" => history
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
