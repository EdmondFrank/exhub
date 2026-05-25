defmodule Exhub.MCP.Tools.Exhub.ReloadKeys do
  @moduledoc """
  MCP Tool: exhub_reload_keys

  Reloads all API keys from SecretVault at runtime without restarting
  the Exhub application.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "exhub_reload_keys"

  @impl true
  def description do
    """
    Reload all API keys from SecretVault without restarting the VM.

    Re-reads secrets from the configured SecretVault and updates the
    application environment. This allows key rotation without downtime.
    """
  end

  schema do
    # No parameters required
  end

  @impl true
  def execute(_params, frame) do
    case Exhub.Router.Config.reload_from_scr() do
      :ok ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{
            "status" => "success",
            "summary" => "API keys reloaded from SecretVault successfully."
          })

        {:reply, resp, frame}

      {:error, reason} ->
        resp =
          Response.tool()
          |> Response.error("Failed to reload API keys: #{inspect(reason)}")

        {:reply, resp, frame}
    end
  end
end
