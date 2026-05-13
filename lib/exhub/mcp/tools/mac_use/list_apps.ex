defmodule Exhub.MCP.Tools.MacUse.ListApps do
  @moduledoc """
  MCP Tool: list_apps

  List running macOS applications visible to the Accessibility API.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.MacUse.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "list_apps"

  @impl true
  def description do
    """
    List running macOS applications visible to the Accessibility API.

    Returns a list of application names and their process IDs.
    Use this to discover target apps for other Mac-use tools.
    """
  end

  schema do
  end

  @impl true
  def execute(_params, frame) do
    case Helpers.run_axcli(["list-apps"]) do
      {:ok, output} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{apps: parse_apps(output)})

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end

  defp parse_apps(output) do
    output
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      case Regex.run(~r/^(.+?)\s+\((\d+)\)\s*$/, line) do
        [_, name, pid] -> %{"name" => String.trim(name), "pid" => String.to_integer(pid)}
        _ -> %{"raw" => String.trim(line)}
      end
    end)
  end
end
