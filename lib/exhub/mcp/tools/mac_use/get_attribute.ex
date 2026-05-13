defmodule Exhub.MCP.Tools.MacUse.GetAttribute do
  @moduledoc """
  MCP Tool: get_attribute

  Get an attribute value from an element.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.MacUse.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "get_attribute"

  @impl true
  def description do
    """
    Get an attribute value from an element in a macOS application.

    Supported attributes: text, value, title, description, classes, position, size,
    enabled, focused, role, identifier, and other AX attributes.

    Parameters:
    - app: Application name (required unless pid is provided)
    - pid: Process ID (alternative to app)
    - selector: CSS-like selector for the target element (required)
    - attribute: Attribute name to retrieve (required)
    """
  end

  schema do
    field(:app, :string, description: "Application name")
    field(:pid, :integer, description: "Process ID")
    field(:selector, :string, description: "CSS-like selector for the target element", required: true)
    field(:attribute, :string, description: "Attribute name (text, value, title, classes, position, size, ...)", required: true)
  end

  @impl true
  def execute(params, frame) do
    args = ["get"] ++ Helpers.app_args(params)
    args = args ++ [Map.get(params, :attribute), Map.get(params, :selector)]

    case Helpers.run_axcli(args) do
      {:ok, output} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{attribute: Map.get(params, :attribute), value: output})

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end
end
