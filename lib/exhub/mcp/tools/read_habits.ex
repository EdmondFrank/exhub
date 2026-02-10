defmodule Exhub.MCP.Tools.ReadHabits do
  @moduledoc """
  MCP Tool for reading user's habit configuration and environment information.

  This tool allows querying the habit store for:
  - All configured habits
  - Specific habit values by key
  - Filtering by category
  """

  alias Exhub.MCP.HabitStore
  alias Hermes.Server.Response

  use Hermes.Server.Component, type: :tool

  def name, do: "read_habits"

  def description do
    """
    Read user's configuration and environment information.

    This tool allows you to query stored user preferences, habits, and environment settings.
    You can retrieve all information, get specific information by key, or filter information by category.

    Returns a list of entries with their values and metadata including:
    - key: The identifier
    - value: The stored value
    - modifiable: Whether the model can modify this entry
    - description: Human-readable description of the entry
    - category: Grouping category for the entry
    """
  end

  schema do
    field :key, :string, description: "Specific entry key to retrieve. If not provided, it returns all information."
    field :category, :string, description: "Filter entries by category (e.g., 'editor', 'workflow', 'environment'). Only used when key is not specified."
    field :include_metadata, :boolean, description: "Whether to include metadata (modifiable status, description, category) in the response. Defaults to true.", default: true
  end

  @impl true
  def execute(params, frame) do
    key = Map.get(params, :key)
    category = Map.get(params, :category)
    include_metadata = Map.get(params, :include_metadata, true)

    result =
      cond do
        key != nil && key != "" ->
          get_single_habit(key)

        category != nil && category != "" ->
          get_habits_by_category(category)

        true ->
          get_all_habits()
      end

    case result do
      {:ok, habits} when is_list(habits) ->
        response = format_habits_response(habits, include_metadata)
        resp = Response.tool() |> Response.structured(response)
        {:reply, resp, frame}

      {:ok, habit} when is_map(habit) ->
        response = format_single_habit(habit, include_metadata)
        resp = Response.tool() |> Response.structured(response)
        {:reply, resp, frame}

      {:error, :not_found} ->
        resp = Response.tool() |> Response.error("Habit not found: #{key}")
        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to read habits: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end

  # Private functions

  defp get_single_habit(key) do
    HabitStore.get(key)
  end

  defp get_all_habits do
    HabitStore.get_all()
  end

  defp get_habits_by_category(category) do
    case HabitStore.get_all() do
      {:ok, habits} ->
        filtered = Enum.filter(habits, fn habit ->
          String.downcase(habit["category"] || "") == String.downcase(category)
        end)
        {:ok, filtered}

      error ->
        error
    end
  end

  defp format_habits_response(habits, true) do
    %{
      "habits" => habits,
      "count" => length(habits),
      "modifiable_count" => Enum.count(habits, & &1["modifiable"]),
      "protected_count" => Enum.count(habits, &(not &1["modifiable"]))
    }
  end

  defp format_habits_response(habits, false) do
    simplified =
      Enum.map(habits, fn habit ->
        %{
          "key" => habit["key"],
          "value" => habit["value"]
        }
      end)

    %{
      "habits" => simplified,
      "count" => length(habits)
    }
  end

  defp format_single_habit(habit, true) do
    %{
      "habit" => habit
    }
  end

  defp format_single_habit(habit, false) do
    %{
      "key" => habit["key"],
      "value" => habit["value"]
    }
  end
end
