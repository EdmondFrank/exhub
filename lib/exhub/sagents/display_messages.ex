defmodule Exhub.Sagents.DisplayMessages do
  @moduledoc """
  `Sagents.DisplayMessagePersistence` over `Exhub.Sagents.Store` (SQLite).

  Sagents calls these callbacks from inside the AgentServer process, so each
  turn produces exactly one set of rows no matter how many browsers watch it.
  Rows come from `Sagents.Message.DisplayHelpers.extract_display_items/1`:
  text and thinking become plain rows, tool calls and tool results become
  rows with a denormalized `call_id` so status updates are an indexed lookup.
  """

  @behaviour Sagents.DisplayMessagePersistence

  alias Exhub.Sagents.Store
  alias Sagents.Message.DisplayHelpers

  @impl true
  def save_message(_scope, %LangChain.Message{} = message, context) do
    with {:ok, conversation_id} <- conversation_id(context) do
      case DisplayHelpers.extract_display_items(message) do
        [] ->
          {:ok, []}

        items ->
          attrs =
            items
            |> Enum.with_index()
            |> Enum.map(fn {item, index} -> to_attrs(item, index) end)

          Store.append_messages(conversation_id, attrs)
      end
    else
      _ -> {:ok, []}
    end
  end

  def save_message(_scope, _message, _context), do: {:ok, []}

  @impl true
  def update_tool_status(_scope, status, tool_info, context) when is_map(tool_info) do
    case call_id_of(tool_info) do
      nil ->
        {:error, :not_found}

      call_id ->
        with {:ok, conversation_id} <- conversation_id(context) do
          Store.update_tool_status(
            conversation_id,
            call_id,
            Atom.to_string(status),
            status_metadata(status, tool_info)
          )
        else
          _ -> {:error, :not_found}
        end
    end
  end

  def update_tool_status(_scope, _status, _tool_info, _context), do: {:error, :not_found}

  @impl true
  def resolve_tool_result(_scope, tool_call_id, result_content, context) do
    with {:ok, conversation_id} <- conversation_id(context) do
      Store.resolve_tool_result(conversation_id, tool_call_id, result_content)
    else
      _ -> {:error, :not_found}
    end
  end

  @impl true
  def save_synthetic_message(_scope, attrs, context) do
    with {:ok, conversation_id} <- conversation_id(context) do
      attrs = [
        %{
          message_type: text(attrs, :message_type) || "system",
          content_type: text(attrs, :content_type) || "text",
          content: Map.get(attrs, :content) || Map.get(attrs, "content") || %{},
          sequence: 0
        }
      ]

      case Store.append_messages(conversation_id, attrs) do
        {:ok, [message | _]} -> {:ok, message}
        {:ok, []} -> {:error, :not_persisted}
        {:error, reason} -> {:error, reason}
      end
    else
      _ -> {:error, :no_conversation}
    end
  end

  # --- helpers ---

  defp to_attrs(item, index) do
    content = item.content
    type = item.type

    %{
      message_type: Atom.to_string(item.message_type),
      content_type: Atom.to_string(type),
      content: content,
      sequence: index,
      status: if(type == :tool_call, do: "pending"),
      call_id: content_call_id(type, content)
    }
  end

  # Tool calls carry `call_id`; tool results carry `tool_call_id`. Both land in
  # the same indexed column so status updates and resolves stay simple.
  defp content_call_id(:tool_call, content), do: Map.get(content, "call_id")
  defp content_call_id(:tool_result, content), do: Map.get(content, "tool_call_id")
  defp content_call_id(_type, _content), do: nil

  defp status_metadata(:completed, tool_info) do
    %{"result" => Map.get(tool_info, :result)}
    |> maybe_put("display_text", Map.get(tool_info, :display_text))
  end

  defp status_metadata(:failed, tool_info), do: %{"error" => Map.get(tool_info, :error)}

  defp status_metadata(:interrupted, tool_info),
    do: %{"display_text" => Map.get(tool_info, :display_text)}

  defp status_metadata(_status, _tool_info), do: %{}

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)

  defp call_id_of(tool_info) do
    Map.get(tool_info, :call_id) || Map.get(tool_info, "call_id")
  end

  defp text(attrs, key), do: Map.get(attrs, key) || Map.get(attrs, to_string(key))

  defp conversation_id(%{conversation_id: id}) when is_binary(id), do: {:ok, id}
  defp conversation_id(_context), do: {:error, :no_conversation}
end
