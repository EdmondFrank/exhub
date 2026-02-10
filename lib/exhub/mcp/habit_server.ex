defmodule Exhub.MCP.HabitServer do
  @moduledoc """
  MCP Server for managing user habit configuration and environment information.

  This server exposes two tools via the Model Context Protocol:
  1. read_habits - Query user habits and environment settings
  2. update_habits - Modify habits that are marked as modifiable

  The server uses HTTP transport and can be accessed at the /mcp endpoint.

  ## Protected Keys

  By default, keys containing sensitive information are protected from modification:
  - user_id, email, api_key, password, secret, token

  Users can configure which keys are modifiable by setting the `modifiable`
  metadata when creating habits.
  """

  use Hermes.Server,
    name: "exhub-habit-server",
    version: "1.0.0",
    capabilities: [:tools]

  # Register the tool components
  component Exhub.MCP.Tools.ReadHabits
  component Exhub.MCP.Tools.UpdateHabits

  @impl true
  def init(client_info, frame) do
    # Initialize with some default habits if the store is empty
    defaults = %{
      "environment.timezone" => %{
        "value" => "CST",
        "description" => "User's preferred timezone",
        "category" => "environment",
        "modifiable" => true
      }
    }

    # Initialize defaults in the background
    Task.start(fn ->
      Exhub.MCP.HabitStore.init_defaults(defaults)
    end)

    # You can also access client info to customize initialization
    # client_info contains: %{"name" => "client-name", "version" => "x.y.z"}
    _ = client_info

    {:ok, frame}
  end
end
