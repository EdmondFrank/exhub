defmodule Exhub.MCP.Tools.UpdateHabits do
  @moduledoc """
  MCP Tool for updating user's habit configuration and environment information.

  This tool allows modifying habit values that are marked as modifiable.
  Protected keys (those containing sensitive information like passwords,
  api keys, tokens, etc.) cannot be modified through this tool.
  """

  alias Exhub.MCP.HabitStore
  alias Hermes.Server.Response

  use Hermes.Server.Component, type: :tool

  def name, do: "update_habits"

  def description do
    """
    Update user's configuration and environment information.

    This tool allows you to modify stored user preferences, habits, and environment settings.
    Only entries marked as 'modifiable' can be updated. Protected keys (containing sensitive
    information like passwords, api keys, tokens, user_id, email) cannot be modified.

    You can:
    - Update existing entries (if modifiable)
    - Create new entries (automatically marked as modifiable)
    - Delete entries (if modifiable)
    - Batch update multiple entries at once

    The operation will fail with an error if you attempt to modify protected keys.
    """
  end

  schema do
    field :operation, {:required, :string}, description: "The operation to perform: 'set' to create/update a single entry, 'delete' to remove a entry, 'batch_set' to update multiple entries at once."
    field :key, :string, description: "The entry key to set or delete. Required for 'set' and 'delete' operations."
    field :value, :any, description: "The value to set for the entry. Required for 'set' operation. Can be any JSON-serializable value."

    field :metadata, description: "Optional metadata for the entry when creating new entries." do
      field :description, :string, description: "Human-readable description of the entry."
      field :category, :string, description: "Category for grouping entries (e.g., 'editor', 'workflow', 'environment')."
    end

    embeds_many :habits, description: "Array of entry updates for 'batch_set' operation. Each item should have 'key' and 'value' fields." do
      field :key, :string
      field :value, :any

      field :metadata, description: "Optional metadata for the entry." do
        field :description, :string
        field :category, :string
      end
    end
  end

  @impl true
  def execute(params, frame) do
    operation = Map.get(params, :operation, "set")

    result =
      case operation do
        "set" ->
          do_set(params)

        "delete" ->
          do_delete(params)

        "batch_set" ->
          do_batch_set(params)

        _ ->
          {:error, "Unknown operation: #{operation}"}
      end

    case result do
      {:ok, response} ->
        resp = Response.tool() |> Response.structured(response)
        {:reply, resp, frame}

      {:error, :not_modifiable} ->
        key = Map.get(params, :key, "unknown")
        resp = Response.tool() |> Response.error("Cannot modify protected key: #{key}. This key contains sensitive information and can only be changed by the user directly.")
        {:reply, resp, frame}

      {:error, :not_found} ->
        key = Map.get(params, :key, "unknown")
        resp = Response.tool() |> Response.error("Habit not found: #{key}")
        {:reply, resp, frame}

      {:error, reason} when is_binary(reason) ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Operation failed: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end

  # Private functions

  defp do_set(params) do
    key = Map.get(params, :key)
    value = Map.get(params, :value)
    metadata = Map.get(params, :metadata, %{}) || %{}

    cond do
      is_nil(key) or key == "" ->
        {:error, "Key is required for 'set' operation"}

      is_nil(value) ->
        {:error, "Value is required for 'set' operation"}

      true ->
        case HabitStore.set(key, value) do
          :ok ->
            # If metadata provided, update it
            if metadata != %{} do
              HabitStore.set_with_metadata(key, value, %{
                description: Map.get(metadata, :description, ""),
                category: Map.get(metadata, :category, "general")
              })
            end

            {:ok, %{
              "success" => true,
              "operation" => "set",
              "key" => key,
              "message" => "Habit '#{key}' updated successfully"
            }}

          {:error, :not_modifiable} ->
            {:error, :not_modifiable}

          error ->
            error
        end
    end
  end

  defp do_delete(params) do
    key = Map.get(params, :key)

    cond do
      is_nil(key) or key == "" ->
        {:error, "Key is required for 'delete' operation"}

      true ->
        case HabitStore.delete(key) do
          :ok ->
            {:ok, %{
              "success" => true,
              "operation" => "delete",
              "key" => key,
              "message" => "Habit '#{key}' deleted successfully"
            }}

          {:error, :not_modifiable} ->
            {:error, :not_modifiable}

          {:error, :not_found} ->
            {:error, :not_found}

          error ->
            error
        end
    end
  end

  defp do_batch_set(params) do
    habits = Map.get(params, :habits, [])

    if not is_list(habits) or length(habits) == 0 do
      {:error, "'habits' array is required for 'batch_set' operation"}
    else
      results =
        Enum.map(habits, fn habit ->
          key = Map.get(habit, :key)
          value = Map.get(habit, :value)
          metadata = Map.get(habit, :metadata, %{}) || %{}

          cond do
            is_nil(key) or key == "" ->
              %{"key" => key, "success" => false, "error" => "Missing key"}

            is_nil(value) ->
              %{"key" => key, "success" => false, "error" => "Missing value"}

            true ->
              case HabitStore.set(key, value) do
                :ok ->
                  # If metadata provided, update it
                  if metadata != %{} do
                    HabitStore.set_with_metadata(key, value, %{
                      description: Map.get(metadata, "description", ""),
                      category: Map.get(metadata, "category", "general")
                    })
                  end

                  %{"key" => key, "success" => true}

                {:error, :not_modifiable} ->
                  %{"key" => key, "success" => false, "error" => "Not modifiable"}

                {:error, reason} ->
                  %{"key" => key, "success" => false, "error" => inspect(reason)}
              end
          end
        end)

      success_count = Enum.count(results, & &1["success"])
      failure_count = length(results) - success_count

      {:ok, %{
        "success" => true,
        "operation" => "batch_set",
        "total" => length(results),
        "succeeded" => success_count,
        "failed" => failure_count,
        "results" => results
      }}
    end
  end
end
