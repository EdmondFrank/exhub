defmodule Exhub.MCP.Tools.Exhub.HotReload do
  @moduledoc """
  MCP Tool: exhub_hot_reload

  Triggers a zero-downtime BEAM code hot-reload for the :exhub application.
  Uses the existing Exhub.HotReload module.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "exhub_hot_reload"

  @impl true
  def description do
    """
    Hot-reload all :exhub BEAM modules without restarting the VM.

    This is a zero-downtime operation: in-flight HTTP requests finish with
    the old code, and new requests immediately use the new code.

    Requires that a new release (or compiled modules) is already on disk.
    """
  end

  schema do
    # No parameters required
  end

  @impl true
  def execute(_params, frame) do
    result = Exhub.HotReload.reload()

    summary =
      if result.errors == 0 do
        "Hot reload complete: #{result.ok} modules reloaded successfully."
      else
        "Hot reload finished with issues: #{result.ok} ok, #{result.errors} errors (check server log)."
      end

    resp =
      Response.tool()
      |> Helpers.toon_response(%{
        "ok_count" => result.ok,
        "error_count" => result.errors,
        "summary" => summary
      })

    {:reply, resp, frame}
  end
end
