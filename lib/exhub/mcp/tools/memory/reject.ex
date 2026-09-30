defmodule Exhub.MCP.Tools.Memory.Reject do
  @moduledoc "MCP Tool: memory_reject — reject a memory candidate."

  alias Exhub.Memory.Review
  alias Exhub.MCP.Tools.Memory.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "memory_reject"

  @impl true
  def description do
    "Reject a memory candidate that holds no transferable lesson (routine work, " <>
      "too specific to one moment, or a fix that was reverted)."
  end

  schema do
    field(:memory_id, {:required, :string}, description: "The candidate memory id")
    field(:reason, :string, description: "Why it is not reusable")
  end

  @impl true
  def execute(params, frame) do
    case Review.reject(Map.get(params, :memory_id), Map.get(params, :reason)) do
      {:ok, meta} ->
        Helpers.json(frame, %{"memory_id" => meta["memory_id"], "status" => meta["status"]})

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
