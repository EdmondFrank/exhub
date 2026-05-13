defmodule Exhub.MCP.Tools.MacUse.Activate do
  @moduledoc """
  MCP Tool: activate

  Bring a macOS application to the foreground.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.MacUse.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "activate"

  @impl true
  def description do
    """
    Bring a macOS application to the foreground.

    Activates (raises) the target application window.

    Parameters:
    - app: Application name (required unless pid is provided)
    - pid: Process ID (alternative to app)
    """
  end

  schema do
    field(:app, :string, description: "Application name")
    field(:pid, :integer, description: "Process ID")
  end

  @impl true
  def execute(params, frame) do
    args = ["activate"] ++ Helpers.app_args(params)

    case Helpers.run_axcli(args) do
      {:ok, output} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{result: "activated", detail: output})

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end
end
