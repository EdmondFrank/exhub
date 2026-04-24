defmodule Exhub.MCP.Tools.Exhub.GetVersion do
  @moduledoc """
  MCP Tool: exhub_get_version

  Returns version information for Exhub and its underlying platform.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "exhub_get_version"

  @impl true
  def description do
    """
    Get version information for Exhub and its runtime platform.

    Returns the Exhub application version, Elixir version, OTP release version,
    and ERTS (Erlang Runtime System) version.
    """
  end

  schema do
    # No parameters required
  end

  @impl true
  def execute(_params, frame) do
    exhub_version =
      case :application.get_key(:exhub, :vsn) do
        {:ok, vsn} -> List.to_string(vsn)
        _ -> "unknown"
      end

    result = %{
      exhub_version: exhub_version,
      elixir_version: System.version(),
      otp_version: System.otp_release(),
      erts_version: :erlang.system_info(:version) |> List.to_string()
    }

    resp = Response.tool() |> Helpers.toon_response(result)
    {:reply, resp, frame}
  end
end
