defmodule Exhub.MCP.Tools.Desktop.CreateDirectory do
  @moduledoc """
  MCP Tool: create_directory

  Create a directory (and any missing parent directories) on the filesystem.
  """

  alias Anubis.Server.Response

  use Anubis.Server.Component, type: :tool

  def name, do: "create_directory"

  @impl true
  def description do
    """
    Create a directory on the filesystem.

    Creates the directory and all missing parent directories (equivalent to `mkdir -p`).
    Succeeds silently if the directory already exists.

    Parameters:
    - path: Absolute path of the directory to create
    """
  end

  schema do
    field(:path, {:required, :string}, description: "Absolute path of the directory to create")
  end

  @impl true
  def execute(params, frame) do
    path = Map.get(params, :path)

    case File.mkdir_p(path) do
      :ok ->
        resp =
          Response.tool()
          |> Response.structured(%{
            "success" => true,
            "path" => path,
            "message" => "Directory created successfully."
          })

        {:reply, resp, frame}

      {:error, :eacces} ->
        resp = Response.tool() |> Response.error("Permission denied: #{path}")
        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to create directory: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end
end
