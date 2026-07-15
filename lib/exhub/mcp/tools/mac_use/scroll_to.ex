defmodule Exhub.MCP.Tools.MacUse.ScrollTo do
  @moduledoc """
  MCP Tool: scroll_to

  Scroll an element into view.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.MacUse.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "scroll_to"

  @impl true
  def description do
    """
    Scroll an element into view using AXScrollToVisible.

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
    args = ["scroll-to"] ++ Helpers.app_args(params)
    args = args ++ [Map.get(params, :selector)]

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
