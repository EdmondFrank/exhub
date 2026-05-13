defmodule Exhub.MCP.Tools.MacUse.Scroll do
  @moduledoc """
  MCP Tool: scroll

  Scroll within an element in a macOS application.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.MacUse.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "scroll"

  @impl true
  def description do
    """
    Scroll within an element in a macOS application.

    Background-safe via CGEventPostToPid by default.

    Parameters:
    - app: Application name (required unless pid is provided)
    - pid: Process ID (alternative to app)
    - selector: CSS-like selector for the scrollable element (required)
    - direction: Scroll direction: "up", "down", "left", "right" (required)
    - amount: Scroll amount in pixels (required)
    - strategy: Scroll strategy: "auto" (default), "cg-pid", "cg"
    """
  end

  schema do
    field(:app, :string, description: "Application name")
    field(:pid, :integer, description: "Process ID")
    field(:selector, :string, description: "CSS-like selector for the scrollable element", required: true)
    field(:direction, :string, description: "Scroll direction: up, down, left, right", required: true)
    field(:amount, :integer, description: "Scroll amount in pixels", required: true)
    field(:strategy, :string, description: "Scroll strategy: auto, cg-pid, cg")
  end

  @impl true
  def execute(params, frame) do
    args = ["scroll"] ++ Helpers.app_args(params)
    args = args ++ Helpers.strategy_args(params)
    args = args ++ [Map.get(params, :selector), Map.get(params, :direction), to_string(Map.get(params, :amount))]

    case Helpers.run_axcli(args) do
      {:ok, output} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{result: "scrolled", detail: output})

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end
end
