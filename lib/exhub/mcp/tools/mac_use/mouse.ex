defmodule Exhub.MCP.Tools.MacUse.Mouse do
  @moduledoc """
  MCP Tool: mouse

  Global mouse control (ignores --app/--pid).
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.MacUse.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "mouse"

  @impl true
  def description do
    """
    Global mouse control — ignores application targeting.

    Subcommands:
    - pos: Print current cursor position
    - move: Move cursor to x,y coordinates
    - click: Click at x,y coordinates
    - dblclick: Double-click at x,y coordinates
    - scroll: Scroll at current position (x, y offsets)

    Parameters:
    - action: Subcommand: "pos", "move", "click", "dblclick", "scroll" (required)
    - x: X coordinate (for move, click, dblclick) or x offset (for scroll)
    - y: Y coordinate (for move, click, dblclick) or y offset (for scroll)
    """
  end

  schema do
    field(:action, :string, description: "Subcommand: pos, move, click, dblclick, scroll", required: true)
    field(:x, :integer, description: "X coordinate or scroll x offset")
    field(:y, :integer, description: "Y coordinate or scroll y offset")
  end

  @impl true
  def execute(params, frame) do
    action = Map.get(params, :action)

    args =
      case action do
        "pos" ->
          ["mouse", "pos"]

        "move" ->
          x = Map.get(params, :x)
          y = Map.get(params, :y)
          if x && y do
            ["mouse", "move", to_string(x), to_string(y)]
          else
            nil
          end

        "click" ->
          x = Map.get(params, :x)
          y = Map.get(params, :y)
          if x && y do
            ["mouse", "click", to_string(x), to_string(y)]
          else
            nil
          end

        "dblclick" ->
          x = Map.get(params, :x)
          y = Map.get(params, :y)
          if x && y do
            ["mouse", "dblclick", to_string(x), to_string(y)]
          else
            nil
          end

        "scroll" ->
          x = Map.get(params, :x, 0)
          y = Map.get(params, :y, 0)
          ["mouse", "scroll", to_string(x), to_string(y)]

        _ ->
          nil
      end

    case args do
      nil ->
        resp = Response.tool() |> Response.error("Invalid action or missing x,y coordinates")
        {:reply, resp, frame}

      args ->
        case Helpers.run_axcli(args) do
          {:ok, output} ->
            resp =
              Response.tool()
              |> Helpers.toon_response(%{action: action, result: output})

            {:reply, resp, frame}

          {:error, reason} ->
            resp = Response.tool() |> Response.error(reason)
            {:reply, resp, frame}
        end
    end
  end
end
