defmodule Exhub.MCP.Tools.TodoUpdateItem do
  @moduledoc """
  MCP Tool: update_item_completion

  Updates the `completed` flag of one or more todo items for a given tenant.
  Accepts either a single item (`name` + `completed`) or a batch (`items`), so
  several items can be modified in one call.
  """

  alias Exhub.MCP.TodoStore
  alias Anubis.Server.Response

  use Anubis.Server.Component, type: :tool

  def name, do: "update_item_completion"

  @impl true
  def description do
    """
    Mark todo items completed (or reopen them). Call this right after finishing
    each step to keep the list current.

    Update several items in one call with `items`; for a single item use `name` +
    `completed`. Pass one form or the other, not both. Item names must match the
    list exactly. Requires an existing list (`set_items` first).

    Returns the full updated list. Names not present are reported in `not_found`.
    """
  end

  schema do
    field(:tenant_id, {:required, :string},
      description: "Stable task/conversation id; must match the value used with set_items."
    )

    field(:name, :string, description: "Exact name of the item to update (single-item form).")

    field(:completed, :boolean,
      description: "New status for `name`: true = done, false = reopen."
    )

    embeds_many :items,
      description: "Batch form: items to update in one call, instead of `name` + `completed`." do
      field(:name, {:required, :string}, description: "Exact name of an existing item.")

      field(:completed, {:required, :boolean},
        description: "New status: true = done, false = reopen."
      )
    end
  end

  @impl true
  def execute(params, frame) do
    tenant_id = Map.get(params, :tenant_id)

    case normalise_updates(params) do
      {:ok, updates} ->
        reply(tenant_id, updates, frame)

      {:error, message} ->
        resp = Response.tool() |> Response.error(message)
        {:reply, resp, frame}
    end
  end

  defp reply(tenant_id, updates, frame) do
    case TodoStore.update_items(tenant_id, updates) do
      {:ok, entry, missing} ->
        updated = updates |> Enum.map(&elem(&1, 0)) |> Enum.uniq() |> Kernel.--(missing)

        resp =
          Response.tool()
          |> Response.structured(%{
            "success" => true,
            "tenant_id" => tenant_id,
            "updated" => updated,
            "not_found" => missing,
            "items" => Enum.map(entry.items, &item_to_map/1),
            "count" => length(entry.items)
          })

        {:reply, resp, frame}

      {:error, :not_found} ->
        resp =
          Response.tool()
          |> Response.error(
            "No todo list found for tenant '#{tenant_id}'. Create one first with set_items."
          )

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to update items: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end

  defp normalise_updates(params) do
    items = Map.get(params, :items)
    name = Map.get(params, :name)
    completed = Map.get(params, :completed)

    cond do
      is_list(items) and items != [] and is_binary(name) ->
        {:error, "Provide either `name` + `completed`, or a non-empty `items` array — not both."}

      is_list(items) and items != [] ->
        normalise_batch(items)

      is_binary(name) and is_boolean(completed) ->
        {:ok, [{name, completed}]}

      is_binary(name) ->
        {:error, "`completed` is required when updating a single item by `name`."}

      true ->
        {:error, "Provide either `name` + `completed`, or a non-empty `items` array."}
    end
  end

  defp normalise_batch(items) do
    Enum.reduce_while(items, {:ok, []}, fn item, {:ok, acc} ->
      item_name = value(item, :name)
      item_completed = value(item, :completed)

      cond do
        not is_binary(item_name) or item_name == "" ->
          {:halt, {:error, "Every entry in `items` needs a non-empty string `name`."}}

        not is_boolean(item_completed) ->
          {:halt, {:error, "Every entry in `items` needs a boolean `completed` value."}}

        true ->
          {:cont, {:ok, acc ++ [{item_name, item_completed}]}}
      end
    end)
  end

  defp value(map, key, default \\ nil)

  defp value(map, key, default) when is_map(map),
    do: Map.get(map, key, Map.get(map, to_string(key), default))

  defp value(_map, _key, default), do: default

  defp item_to_map(%{name: name, completed: completed}),
    do: %{"name" => name, "completed" => completed}

  defp item_to_map(item), do: item
end
