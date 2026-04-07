defmodule Exhub.MCP.Tools.Desktop.InteractWithProcess do
  @moduledoc """
  MCP Tool: interact_with_process

  Send input to an interactive process's stdin.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers
  alias Exhub.MCP.Desktop.ProcessStore

  use Anubis.Server.Component, type: :tool

  def name, do: "interact_with_process"

  @impl true
  def description do
    """
    Send input to an interactive process's stdin.

    Use this to interact with processes started with start_process(interactive: true).
    The input is sent directly to the process's stdin. Use this for interactive
    commands, REPLs, or any process that accepts user input.

    Parameters:
    - process_id: The ID returned by start_process
    - input: The text to send to the process's stdin
    """
  end

  schema do
    field(:process_id, {:required, :string}, description: "The ID returned by start_process")
    field(:input, {:required, :string}, description: "The text to send to the process's stdin")
  end

  @impl true
  def execute(params, frame) do
    process_id = Map.get(params, :process_id)
    input = Map.get(params, :input)

    case ProcessStore.send_input(process_id, input) do
      :ok ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{
            "success" => true,
            "process_id" => process_id,
            "input_sent" => input
          })

        {:reply, resp, frame}

      {:error, :not_found} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{
            "success" => false,
            "error" => "Process not found: #{process_id}"
          })

        {:reply, resp, frame}

      {:error, :not_interactive} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{
            "success" => false,
            "error" => "Process #{process_id} is not interactive. Start with interactive: true."
          })

        {:reply, resp, frame}

      {:error, :no_port} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{
            "success" => false,
            "error" => "Process #{process_id} has no port reference"
          })

        {:reply, resp, frame}

      {:error, :process_not_running} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{
            "success" => false,
            "error" => "Process #{process_id} is not running"
          })

        {:reply, resp, frame}

      {:error, reason} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{
            "success" => false,
            "error" => "Failed to send input: #{reason}"
          })

        {:reply, resp, frame}
    end
  end
end
