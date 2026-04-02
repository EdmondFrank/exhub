defmodule Exhub.MCP.Tools.ArcheryGetResourceGroups do
  @moduledoc """
  MCP Tool for getting resource groups from Archery.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Archery.Client

  use Anubis.Server.Component, type: :tool

  def name, do: "get_resource_groups"

  @impl true
  def description do
    """
    Get list of available resource groups for workflow submission. Call this FIRST when submitting a workflow.
    """
  end

  schema do
  end

  @impl true
  def execute(_params, frame) do
    client = Client.new()

    case Client.get_resource_groups(client) do
      {:ok, groups, _} ->
        response = %{
          "success" => true,
          "count" => length(groups),
          "groups" => groups
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
