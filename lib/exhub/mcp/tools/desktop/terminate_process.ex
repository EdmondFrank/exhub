defmodule Exhub.MCP.Tools.Desktop.TerminateProcess do
  @moduledoc """
  MCP Tool: terminate_process

  Terminate a managed process started with start_process.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers
  alias Exhub.MCP.Desktop.ProcessStore

  use Anubis.Server.Component, type: :tool

  def name, do: "terminate_process"

  @impl true
  def description do
    """
    Terminate a managed process started with start_process.

    Sends SIGTERM to the process. If the process doesn't exit within the
    timeout, it can be force-killed with kill_process using the system PID.

    Parameters:
    - process_id: The process ID returned by start_process
    """
  end

  schema do
    field(:process_id, {:required, :string}, description: "The process ID returned by start_process")
  end

  @impl true
  def execute(params, frame) do
    process_id = Map.get(params, :process_id)

    case ProcessStore.kill_process(process_id) do
      {:ok, entry} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{
            "process_id" => process_id,
            "status" => to_string(entry.status)
          })

        {:reply, resp, frame}

      {:error, :not_found} ->
        resp =
          Response.tool()
          |> Response.error("Process not found: #{process_id}")

        {:reply, resp, frame}

      {:error, :no_pid} ->
        resp =
          Response.tool()
          |> Response.error("Process has no associated system PID")

        {:reply, resp, frame}
    end
  end
end
