defmodule Exhub.MCP.Tools.Toonflow.CreateProject do
  @moduledoc """
  MCP Tool: `toonflow_create_project` — scaffold a Toonflow project workspace.
  """

  alias Anubis.Server.Response
  alias Exhub.Toonflow.{Store, Workspace}

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_create_project"

  @impl true
  def description do
    """
    Create a Toonflow project: a local workspace directory (`workspaces/<name>/`)
    with its own SQLite database and a registry entry.

    `name` must be a lowercase slug (letters, digits, dot, dash or underscore;
    starts alphanumeric; max 64 characters) because it becomes a directory name.

    This only scaffolds the project. Run the pipeline tools next (add novel,
    extract events, generate script, ...).
    """
  end

  schema do
    field(:name, {:required, :string},
      description:
        ~s(Project name — a lowercase slug used as the directory name, e.g. "my-drama".)
    )

    field(:description, :string,
      description: "Optional short description stored in the project's metadata.",
      default: ""
    )
  end

  @impl true
  def execute(params, frame) do
    name = Map.get(params, :name)
    description = Map.get(params, :description, "")

    opts =
      if is_binary(description) and description != "",
        do: [description: description],
        else: []

    case Store.create_project(name, opts) do
      {:ok, project} ->
        resp = Response.tool() |> Response.structured(%{"success" => true, "project" => project})
        {:reply, resp, frame}

      {:error, {:invalid_name, bad}} ->
        suggestion = Workspace.slugify(to_string(bad))

        resp =
          Response.tool()
          |> Response.error(
            "Invalid project name #{inspect(bad)}. Use a lowercase slug " <>
              "(letters, digits, dot, dash, underscore; max 64 chars). " <>
              "Suggested: #{inspect(suggestion)}"
          )

        {:reply, resp, frame}

      {:error, :already_exists} ->
        resp =
          Response.tool()
          |> Response.error("A project named #{inspect(name)} already exists.")

        {:reply, resp, frame}

      {:error, :registry_unavailable} ->
        resp =
          Response.tool()
          |> Response.error("Toonflow registry is unavailable (SQLite not open).")

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to create project: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end
end
