defmodule Exhub.MCP.Tools.MacUse.DblClick do
  @moduledoc """
  MCP Tool: dblclick

  Double-click an element in a macOS application (background-safe).
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.MacUse.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "dblclick"

  @impl true
  def description do
    """
    Double-click an element in a macOS application.

    Background-safe via CGEventPostToPid by default.

    Parameters:
    - app: Application name (required unless pid is provided)
    - pid: Process ID (alternative to app)
    - selector: CSS-like selector for the target element (required)
    - strategy: Click strategy: "auto" (default), "ax", "cg", "cg-pid"
    - no_visual_cursor: Disable the crosshair overlay
    """
  end

  schema do
    field(:app, :string, description: "Application name")
    field(:pid, :integer, description: "Process ID")
    field(:selector, :string, description: "CSS-like selector for the target element", required: true)
    field(:strategy, :string, description: "Click strategy: auto, ax, cg, cg-pid")
    field(:no_visual_cursor, :boolean, description: "Disable crosshair overlay", default: false)
  end

  @impl true
  def execute(params, frame) do
    args = ["dblclick"] ++ Helpers.app_args(params)
    args = args ++ Helpers.strategy_args(params)
    args = args ++ Helpers.cursor_args(params)
    args = args ++ [Map.get(params, :selector)]

    case Helpers.run_axcli(args) do
      {:ok, output} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{result: "double-clicked", detail: output})

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end
end
