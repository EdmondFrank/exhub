defmodule Exhub.MCP.Tools.MacUse.Input do
  @moduledoc """
  MCP Tool: input

  Focus an element and type text (appends to existing content).
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.MacUse.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "input"

  @impl true
  def description do
    """
    Focus an element in a macOS application and type text.

    Appends to existing content. Use `fill` to replace all text.

    Parameters:
    - app: Application name (required unless pid is provided)
    - pid: Process ID (alternative to app)
    - selector: CSS-like selector for the target element (required)
    - text: The text to type (required)
    """
  end

  schema do
    field(:app, :string, description: "Application name")
    field(:pid, :integer, description: "Process ID")
    field(:selector, :string, description: "CSS-like selector for the target element", required: true)
    field(:text, :string, description: "Text to type", required: true)
  end

  @impl true
  def execute(params, frame) do
    args = ["input"] ++ Helpers.app_args(params)
    args = args ++ [Map.get(params, :selector), Map.get(params, :text)]

    case Helpers.run_axcli(args) do
      {:ok, output} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{result: "typed", detail: output})

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end
end
