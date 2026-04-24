defmodule Exhub.MCP.Tools.Exhub.Release do
  @moduledoc """
  MCP Tool: exhub_release

  Delegates the full release pipeline to the connected Emacs client via
  WebSocket.  Emacs runs `mix compile && mix release --overwrite` in the
  source tree (derived from `exhub-backend-path`) and then performs a
  graceful soft restart once the build succeeds.

  This approach works correctly in both development and release builds
  because the shell commands are executed by Emacs, which always has Mix
  available in its PATH.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "exhub_release"

  @impl true
  def description do
    """
    Trigger the full Exhub release pipeline via the connected Emacs client.

    Emacs executes the following steps in sequence:
      1. `mix compile`             — compile changed modules
      2. `mix release --overwrite` — assemble a new OTP release on disk
      3. Graceful soft restart     — hot-swap the running VM into the new release

    Build output is written to the *exhub-release* buffer in Emacs.
    The tool returns immediately after delegating; the pipeline runs
    asynchronously in Emacs.

    Requires an active Emacs WebSocket connection.
    """
  end

  schema do
    # No parameters — env and restart behaviour are controlled by
    # `exhub-mix-env` and `exhub-graceful-restart-delay` in Emacs config.
  end

  @impl true
  def execute(_params, frame) do
    case emacs_connected?() do
      false ->
        resp =
          Response.tool()
          |> Response.error(
            "No Emacs client connected. " <>
              "Start Emacs and call `exhub-start' to establish the WebSocket connection."
          )

        {:reply, resp, frame}

      true ->
        Exhub.send_message("(exhub-release)")

        result = %{
          "delegated" => true,
          "message" =>
            "Release pipeline delegated to Emacs. " <>
              "Check the *exhub-release* buffer for build output."
        }

        resp = Response.tool() |> Helpers.toon_response(result)
        {:reply, resp, frame}
    end
  end

  # --------------------------------------------------------------------------
  # Helpers
  # --------------------------------------------------------------------------

  defp emacs_connected? do
    Registry.lookup(Exhub.Registry, "socket_handler") != []
  end
end
