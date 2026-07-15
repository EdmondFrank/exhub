defmodule Exhub.MCP.Tools.MacUse.Fill do
  @moduledoc """
  MCP Tool: fill

  Clear a field then type text (Cmd+A, Delete, type).
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.MacUse.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "fill"

  @impl true
  def description do
    """
    Clear a field in a macOS application then type text.

    Performs Cmd+A, Delete, then types the new text. Replaces all existing content.

    Parameters:
    - app: Application name (required unless pid is provided)
    - pid: Process ID (alternative to app)
    - selector: CSS-like selector for the target element (required)
    - text: The text to type after clearing (required)
    """
  end

  schema do
    field(:app, :string, description: "Application name")
    field(:pid, :integer, description: "Process ID")

    field(:selector, :string,
      description: "CSS-like selector for the target element",
      required: true
    )

    field(:text, :string, description: "Text to type after clearing", required: true)
  end

  @impl true
  def execute(params, frame) do
    args = ["fill"] ++ Helpers.app_args(params)
    args = args ++ [Map.get(params, :selector), Map.get(params, :text)]

    case Helpers.run_axcli(args) do
      {:ok, output} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{result: "filled", detail: output})

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end
end
