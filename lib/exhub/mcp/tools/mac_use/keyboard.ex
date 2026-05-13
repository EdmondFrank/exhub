defmodule Exhub.MCP.Tools.MacUse.Keyboard do
  @moduledoc """
  MCP Tool: keyboard

  Global keyboard input (ignores --app/--pid).
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.MacUse.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "keyboard"

  @impl true
  def description do
    """
    Global keyboard input — ignores application targeting.

    Subcommands:
    - type: Type text string
    - press: Press key combination

    Parameters:
    - action: Subcommand: "type" or "press" (required)
    - text: Text to type (for type action) or key combo (for press action)
    """
  end

  schema do
    field(:action, :string, description: "Subcommand: type or press", required: true)
    field(:text, :string, description: "Text to type or key combo to press", required: true)
  end

  @impl true
  def execute(params, frame) do
    action = Map.get(params, :action)
    text = Map.get(params, :text)

    args =
      case action do
        "type" -> ["keyboard", "type", text]
        "press" -> ["keyboard", "press", text]
        _ -> nil
      end

    case args do
      nil ->
        resp = Response.tool() |> Response.error("Invalid action. Use 'type' or 'press'.")
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
