defmodule Exhub.MCP.Tools.MacUse.Hover do
  @moduledoc """
  MCP Tool: hover

  Move the mouse cursor to an element's center.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.MacUse.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "hover"

  @impl true
  def description do
    """
    Move the mouse cursor to the center of an element in a macOS application.

    Useful for triggering hover-dependent UI (menus, tooltips) before clicking.

    Parameters:
    - app: Application name (required unless pid is provided)
    - pid: Process ID (alternative to app)
    - selector: CSS-like selector for the target element (required)
    - no_visual_cursor: Disable the crosshair overlay
    """
  end

  schema do
    field(:app, :string, description: "Application name")
    field(:pid, :integer, description: "Process ID")

    field(:selector, :string,
      description: "CSS-like selector for the target element",
      required: true
    )

    field(:no_visual_cursor, :boolean, description: "Disable crosshair overlay", default: false)
  end

  @impl true
  def execute(params, frame) do
    args = ["hover"] ++ Helpers.app_args(params)
    args = args ++ Helpers.cursor_args(params)
    args = args ++ [Map.get(params, :selector)]

    case Helpers.run_axcli(args) do
      {:ok, output} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{result: "hovered", detail: output})

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end
end
