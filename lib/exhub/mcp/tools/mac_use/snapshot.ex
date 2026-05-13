defmodule Exhub.MCP.Tools.MacUse.Snapshot do
  @moduledoc """
  MCP Tool: snapshot

  Print the accessibility tree of an app or element.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.MacUse.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "snapshot"

  @impl true
  def description do
    """
    Print the accessibility tree of a macOS application or element.

    Shows the hierarchical structure of UI elements (roles, titles, values).
    Use this to explore an app's interface before interacting with elements.

    Parameters:
    - app: Application name (required unless pid is provided)
    - pid: Process ID (alternative to app name)
    - selector: Optional CSS-like selector to scope the snapshot
    - depth: Maximum tree depth to display (default: unlimited)
    - all: If true, show all matches instead of just the first
    """
  end

  schema do
    field(:app, :string, description: "Application name")
    field(:pid, :integer, description: "Process ID (alternative to app)")
    field(:selector, :string, description: "CSS-like selector to scope the snapshot")
    field(:depth, :integer, description: "Maximum tree depth")
    field(:all, :boolean, description: "Show all matches", default: false)
  end

  @impl true
  def execute(params, frame) do
    args = ["snapshot"]
    args = args ++ Helpers.app_args(params)

    args =
      if Map.get(params, :all, false) do
        args ++ ["--all"]
      else
        args
      end

    args = maybe_add(args, "--depth", Map.get(params, :depth))

    args =
      case Map.get(params, :selector) do
        nil -> args
        sel -> args ++ [sel]
      end

    case Helpers.run_axcli(args) do
      {:ok, output} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{tree: output})

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end

  defp maybe_add(args, _flag, nil), do: args
  defp maybe_add(args, flag, value), do: args ++ [flag, to_string(value)]
end
