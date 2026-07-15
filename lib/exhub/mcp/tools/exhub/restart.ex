defmodule Exhub.MCP.Tools.Exhub.Restart do
  @moduledoc """
  MCP Tool: exhub_restart

  Schedules a graceful VM restart after a short delay so the current MCP
  response can be delivered before shutdown begins.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "exhub_restart"

  @impl true
  def description do
    """
    Schedule a graceful restart of the Exhub BEAM VM.

    Use this tool when you need to restart the application after deploying a new
    release, or to recover from a degraded state. The restart is deferred so the
    current MCP response can be delivered before shutdown begins.

    Parameters:
    - mode: Restart strategy (default: "soft"):
      - "soft": Stops and restarts OTP applications within the running VM
      - "hard": Terminates the entire VM; requires an external supervisor (e.g., systemd, Docker) to bring it back
    - delay_ms: Milliseconds to wait before triggering restart (default: 3000)

    Use "soft" for routine restarts after code updates. Use "hard" only when the
    VM is unresponsive or when a clean slate is needed. After a hard restart,
    reconnect to the MCP endpoint once the external supervisor restarts the VM.

    Examples:
    - Routine restart: {"mode": "soft"}
    - Delayed hard restart: {"mode": "hard", "delay_ms": 5000}
    - Quick soft restart: {"mode": "soft", "delay_ms": 1000}
    """
  end

  schema do
    field(:mode, :string,
      description: "Restart mode: soft or hard",
      default: "soft"
    )

    field(:delay_ms, :integer,
      description: "Milliseconds to wait before restart",
      default: 3_000
    )
  end

  @impl true
  def execute(params, frame) do
    mode_str = Map.get(params, :mode, "soft")
    delay_ms = Map.get(params, :delay_ms, 3_000)

    case parse_mode(mode_str) do
      {:ok, mode} ->
        case Exhub.GracefulRestart.schedule_restart(mode, delay_ms) do
          {:ok, _pid} ->
            result = %{
              "scheduled" => true,
              "mode" => Atom.to_string(mode),
              "delay_ms" => delay_ms,
              "message" =>
                "#{mode} restart scheduled in #{delay_ms}ms. The VM will go down briefly; reconnect after restart."
            }

            resp = Response.tool() |> Helpers.toon_response(result)
            {:reply, resp, frame}
        end

      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end

  defp parse_mode("soft"), do: {:ok, :soft}
  defp parse_mode("hard"), do: {:ok, :hard}

  defp parse_mode(other),
    do:
      {:error, "Unknown restart mode: #{inspect(other)}. Valid values are \"soft\" or \"hard\"."}
end
