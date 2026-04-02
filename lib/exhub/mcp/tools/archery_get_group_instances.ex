defmodule Exhub.MCP.Tools.ArcheryGetGroupInstances do
  @moduledoc """
  MCP Tool for getting group instances from Archery.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Archery.Client

  use Anubis.Server.Component, type: :tool

  def name, do: "get_group_instances"

  @impl true
  def description do
    """
    Get instances available for a specific resource group. Call after get_resource_groups().
    """
  end

  schema do
    field(:group_name, {:required, :string}, description: "Resource group name (from get_resource_groups)")
  end

  @impl true
  def execute(params, frame) do
    group_name = Map.get(params, :group_name)

    client = Client.new()

    case Client.get_group_instances(client, group_name) do
      {:ok, instances, _} ->
        response = %{
          "success" => true,
          "group_name" => group_name,
          "count" => length(instances),
          "instances" => instances
        }
        resp = Response.tool() |> Response.text(Jason.encode!(response))
        {:reply, resp, frame}

      {:error, reason, _} ->
        response = %{
          "success" => false,
          "group_name" => group_name,
          "error" => reason
        }
        resp = Response.tool() |> Response.text(Jason.encode!(response))
        {:reply, resp, frame}
    end
  end
end
