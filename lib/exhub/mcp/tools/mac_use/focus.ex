defmodule Exhub.MCP.Tools.MacUse.Focus do
  @moduledoc """
  MCP Tool: focus

  Focus an element in a macOS application.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.MacUse.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "focus"

  @impl true
  def description do
    """
    Focus an element in a macOS application.

    Sets AXFocused attribute and falls back to click if needed.

    Parameters:
    - app: Application name (required unless pid is provided)
    - pid: Process ID (alternative to app)
    - selector: CSS-like selector for the target element (required)
    """
  end

  schema do
    field(:app, :string, description: "Application name")
    field(:pid, :integer, description: "Process ID")

    field(:selector, :string,
      description: "CSS-like selector for the target element",
      required: true
    )
  end

  @impl true
  def execute(params, frame) do
    args = ["focus"] ++ Helpers.app_args(params)
    args = args ++ [Map.get(params, :selector)]

    case Helpers.run_axcli(args) do
      {:ok, output} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{result: "focused", detail: output})

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end
end
