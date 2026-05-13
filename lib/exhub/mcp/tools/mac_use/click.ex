defmodule Exhub.MCP.Tools.MacUse.Click do
  @moduledoc """
  MCP Tool: click

  Click an element in a macOS application (background-safe by default).
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.MacUse.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "click"

  @impl true
  def description do
    """
    Click an element in a macOS application.

    Background-safe by default (no focus steal, no cursor movement).
    Uses CGEventPostToPid to deliver events directly to the target process.

    Parameters:
    - app: Application name (required unless pid is provided)
    - pid: Process ID (alternative to app)
    - selector: CSS-like selector for the target element (required)
    - strategy: Click strategy: "auto" (default), "ax", "cg", "cg-pid"
    - hover: Pre-move cursor to trigger hover-gated UI
    - activate: Bring app to foreground before clicking
    - no_visual_cursor: Disable the crosshair overlay
    """
  end

  schema do
    field(:app, :string, description: "Application name")
    field(:pid, :integer, description: "Process ID")
    field(:selector, :string, description: "CSS-like selector for the target element", required: true)
    field(:strategy, :string, description: "Click strategy: auto, ax, cg, cg-pid")
    field(:hover, :boolean, description: "Pre-move cursor to trigger hover UI", default: false)
    field(:activate, :boolean, description: "Bring app to foreground", default: false)
    field(:no_visual_cursor, :boolean, description: "Disable crosshair overlay", default: false)
  end

  @impl true
  def execute(params, frame) do
    args = ["click"] ++ Helpers.app_args(params)
    args = args ++ Helpers.strategy_args(params)
    args = args ++ Helpers.cursor_args(params)

    args = if Map.get(params, :hover, false), do: args ++ ["--hover"], else: args
    args = if Map.get(params, :activate, false), do: args ++ ["--activate"], else: args
    args = args ++ [Map.get(params, :selector)]

    case Helpers.run_axcli(args) do
      {:ok, output} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{result: "clicked", detail: output})

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end
end
