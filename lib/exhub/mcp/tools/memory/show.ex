defmodule Exhub.MCP.Tools.Memory.Show do
  @moduledoc "MCP Tool: memory_show — full memory body with evidence and provenance."

  alias Exhub.Memory.Store
  alias Exhub.MCP.Tools.Memory.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "memory_show"

  @impl true
  def description do
    "Return one memory in full: frontmatter (status, kind, applicability, " <>
      "project, evaluation, evidence, supersede links) plus its lesson body."
  end

  schema do
    field(:memory_id, {:required, :string}, description: "The memory id, e.g. memory_ab12…")
  end

  @impl true
  def execute(params, frame) do
    memory_id = Map.get(params, :memory_id)

    case Store.read(memory_id) do
      {:ok, record} ->
        Helpers.json(frame, %{
          "memory_id" => record.meta["memory_id"],
          "meta" => record.meta,
          "body" => record.body,
          "file" => record.file
        })

      {:error, :not_found} ->
        Helpers.error(frame, "memory not found: #{memory_id}")
    end
  end
end
