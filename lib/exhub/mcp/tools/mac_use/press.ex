defmodule Exhub.MCP.Tools.MacUse.Press do
  @moduledoc """
  MCP Tool: press

  Press a key combination in a macOS application.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.MacUse.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "press"

  @impl true
  def description do
    """
    Press a key combination in a macOS application.

    Supports modifier keys: Command, Control, Option/Alt, Shift.
    Examples: "Enter", "Command+a", "Control+Shift+v", "Command+Space"

    Parameters:
    - app: Application name (required unless pid is provided)
    - pid: Process ID (alternative to app)
    - keys: Key combination to press (required)
    - strategy: Delivery strategy: "hid" (default, activates app) or "pid" (background)
    """
  end

  schema do
    field(:app, :string, description: "Application name")
    field(:pid, :integer, description: "Process ID")

    field(:keys, :string,
      description: "Key combination (e.g. 'Enter', 'Command+a')",
      required: true
    )

    field(:strategy, :string, description: "Delivery strategy: hid or pid")
  end

  @impl true
  def execute(params, frame) do
    args = ["press"] ++ Helpers.app_args(params)
    args = args ++ Helpers.strategy_args(params)
    args = args ++ [Map.get(params, :keys)]

    case Helpers.run_axcli(args) do
      {:ok, output} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{result: "pressed", detail: output})

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end
end
