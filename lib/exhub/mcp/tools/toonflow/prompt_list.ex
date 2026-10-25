defmodule Exhub.MCP.Tools.Toonflow.PromptList do
  @moduledoc """
  MCP Tool: `toonflow_prompt_list` — the project's prompt overrides.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Prompts

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_prompt_list"

  @impl true
  def description do
    """
    List the prompt templates this project overrides: name, latest version,
    how many versions exist, the latest note, and whether a shipped template of
    that name exists. Templates without an override are simply absent (the
    shipped file is used). Set one with `toonflow_prompt_set`.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
  end

  @impl true
  def execute(params, frame) do
    case Prompts.list(Map.get(params, :project)) do
      {:ok, overrides} ->
        summary = %{
          "count" => length(overrides),
          "overrides" => overrides,
          "enabled" => Prompts.overrides_enabled?()
        }

        {:reply, Response.tool() |> Response.structured(summary), frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
