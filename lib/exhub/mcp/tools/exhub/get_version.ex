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

    Use this tool to verify the deployed version, check compatibility, or
    include version info in diagnostics and bug reports.

    Returns:
    - exhub_version: The Exhub application version (from mix.exs)
    - elixir_version: The Elixir language version
    - otp_version: The OTP release version (e.g., "27")
    - erts_version: The Erlang Runtime System version

    Useful for confirming a deployment succeeded or debugging version-specific
    issues. Compare with expected versions after running exhub_hot_reload or
    exhub_restart.

    No parameters required.
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
