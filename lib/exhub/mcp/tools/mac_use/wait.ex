defmodule Exhub.MCP.Tools.MacUse.Wait do
  @moduledoc """
  MCP Tool: wait

  Wait for an element to appear or sleep for a duration.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.MacUse.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "wait"

  @impl true
  def description do
    """
    Wait for an element to appear or sleep for a specified duration.

    When a selector is provided, polls until the element appears.
    When milliseconds is provided, sleeps for that duration.

    Parameters:
    - app: Application name (required unless pid is provided)
    - pid: Process ID (alternative to app)
    - selector: CSS-like selector to wait for (mutually exclusive with milliseconds)
    - milliseconds: Duration to sleep in ms (mutually exclusive with selector)
    - timeout: Maximum wait time in ms (default: 10000)
    """
  end

  schema do
    field(:app, :string, description: "Application name")
    field(:pid, :integer, description: "Process ID")
    field(:selector, :string, description: "CSS-like selector to wait for")
    field(:milliseconds, :integer, description: "Duration to sleep in ms")
    field(:timeout, :integer, description: "Maximum wait time in ms", default: 10_000)
  end

  @impl true
  def execute(params, frame) do
    selector = Map.get(params, :selector)
    milliseconds = Map.get(params, :milliseconds)

    cond do
      selector && milliseconds ->
        resp =
          Response.tool() |> Response.error("Provide either selector or milliseconds, not both")

        {:reply, resp, frame}

      selector ->
        args = ["wait"] ++ Helpers.app_args(params)
        args = args ++ [selector]

        timeout = Map.get(params, :timeout, 10_000)

        case Helpers.run_axcli(args, timeout: timeout + 5_000) do
          {:ok, output} ->
            resp =
              Response.tool()
              |> Helpers.toon_response(%{result: "element_found", detail: output})

            {:reply, resp, frame}

          {:error, reason} ->
            resp = Response.tool() |> Response.error(reason)
            {:reply, resp, frame}
        end

      milliseconds ->
        args = ["wait", to_string(milliseconds)]

        case Helpers.run_axcli(args, timeout: milliseconds + 5_000) do
          {:ok, output} ->
            resp =
              Response.tool()
              |> Helpers.toon_response(%{
                result: "slept",
                duration_ms: milliseconds,
                detail: output
              })

            {:reply, resp, frame}

          {:error, reason} ->
            resp = Response.tool() |> Response.error(reason)
            {:reply, resp, frame}
        end

      true ->
        resp = Response.tool() |> Response.error("Provide either selector or milliseconds")
        {:reply, resp, frame}
    end
  end
end
