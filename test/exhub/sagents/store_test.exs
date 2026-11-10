defmodule Exhub.Sagents.StoreTest do
  use ExUnit.Case, async: true

  alias Exhub.Sagents.Store

  setup do
    path = Path.join(System.tmp_dir!(), "sagents_store_#{System.unique_integer([:positive])}.db")
    name = :"sagents_store_#{System.unique_integer([:positive])}"

    start_supervised!(%{id: name, start: {Store, :start_link, [[path: path, name: name]]}})

    on_exit(fn ->
      File.rm(path)
      File.rm(path <> "-wal")
      File.rm(path <> "-shm")
    end)

    %{store: name}
  end

  defp message_attrs(attrs), do: Enum.into(attrs, %{})

  test "creates, fetches, and lists conversations", %{store: store} do
    assert {:ok, conversation} = Store.create_conversation("coder", [title: "First"], store)
    assert conversation.agent_name == "coder"
    assert conversation.title == "First"
    assert conversation.status == "active"
    assert conversation.last_message_at == nil

    assert {:ok, ^conversation} = Store.get_conversation(conversation.id, store)
    assert {:error, :not_found} = Store.get_conversation("conv_missing", store)

    assert {:ok, [listed]} = Store.list_conversations([], store)
    assert listed.id == conversation.id
  end

  test "lists most recently active conversation first", %{store: store} do
    {:ok, older} = Store.create_conversation("coder", [title: "Older"], store)
    {:ok, newer} = Store.create_conversation("coder", [title: "Newer"], store)
    {:ok, other} = Store.create_conversation("researcher", [title: "Other"], store)

    {:ok, _} =
      Store.append_messages(
        older.id,
        [message_attrs(message_type: "user", content: %{"text" => "hi"})],
        store
      )

    assert {:ok, [first | rest]} = Store.list_conversations([], store)
    assert first.id == older.id
    assert MapSet.new(Enum.map(rest, & &1.id)) == MapSet.new([newer.id, other.id])

    assert {:ok, [_only]} = Store.list_conversations([agent_name: "researcher"], store)
  end

  test "appends messages in order and moves last_message_at", %{store: store} do
    {:ok, conversation} = Store.create_conversation("coder", [], store)

    attrs = [
      message_attrs(message_type: "user", content: %{"text" => "hello"}),
      message_attrs(
        message_type: "assistant",
        content_type: "tool_call",
        content: %{"call_id" => "call_1", "name" => "read_file", "arguments" => %{}},
        call_id: "call_1",
        status: "pending",
        sequence: 1
      ),
      message_attrs(message_type: "assistant", content: %{"text" => "done"}, sequence: 2)
    ]

    assert {:ok, [one, two, three]} = Store.append_messages(conversation.id, attrs, store)
    assert one.content["text"] == "hello"
    assert two.content_type == "tool_call"
    assert two.status == "pending"
    assert three.content["text"] == "done"

    assert {:ok, messages} = Store.list_messages(conversation.id, store)
    assert Enum.map(messages, & &1.id) == [one.id, two.id, three.id]

    assert {:ok, reloaded} = Store.get_conversation(conversation.id, store)
    assert reloaded.last_message_at != nil
  end

  test "tool status updates merge metadata", %{store: store} do
    {:ok, conversation} = Store.create_conversation("coder", [], store)

    {:ok, [_]} =
      Store.append_messages(
        conversation.id,
        [
          message_attrs(
            message_type: "assistant",
            content_type: "tool_call",
            content: %{"call_id" => "call_2", "name" => "write_file"},
            call_id: "call_2",
            status: "pending"
          )
        ],
        store
      )

    assert {:ok, updated} =
             Store.update_tool_status(
               conversation.id,
               "call_2",
               "completed",
               %{"result" => "ok"},
               store
             )

    assert updated.status == "completed"
    assert updated.content["result"] == "ok"

    assert {:error, :not_found} =
             Store.update_tool_status(conversation.id, "nope", "completed", %{}, store)
  end

  test "tool call ids are unique per conversation", %{store: store} do
    {:ok, conversation} = Store.create_conversation("coder", [], store)

    attrs =
      message_attrs(
        message_type: "assistant",
        content_type: "tool_call",
        content: %{"call_id" => "dup", "name" => "read_file"},
        call_id: "dup",
        status: "pending"
      )

    assert {:ok, [_]} = Store.append_messages(conversation.id, [attrs], store)
    assert {:error, _reason} = Store.append_messages(conversation.id, [attrs], store)
  end

  test "resolves an interrupted tool result", %{store: store} do
    {:ok, conversation} = Store.create_conversation("coder", [], store)

    {:ok, [_]} =
      Store.append_messages(
        conversation.id,
        [
          message_attrs(
            message_type: "tool",
            content_type: "tool_result",
            content: %{"tool_call_id" => "call_3", "is_interrupt" => true, "content" => "waiting"},
            call_id: "call_3"
          )
        ],
        store
      )

    assert {:ok, resolved} =
             Store.resolve_tool_result(conversation.id, "call_3", "final answer", store)

    assert resolved.content["content"] == "final answer"
    assert resolved.content["is_interrupt"] == false
  end

  test "saves, loads, and deletes agent state", %{store: store} do
    {:ok, conversation} = Store.create_conversation("coder", [], store)
    state = %{"version" => 1, "state" => %{"messages" => [], "todos" => [], "metadata" => %{}}}

    assert :ok = Store.save_state(conversation.id, state, store)
    assert {:ok, ^state} = Store.load_state(conversation.id, store)

    assert :ok = Store.delete_state(conversation.id, store)
    assert {:error, :not_found} = Store.load_state(conversation.id, store)
  end

  test "deleting a conversation removes its messages and state", %{store: store} do
    {:ok, conversation} = Store.create_conversation("coder", [], store)

    {:ok, _} =
      Store.append_messages(
        conversation.id,
        [message_attrs(message_type: "user", content: %{"text" => "hi"})],
        store
      )

    :ok = Store.save_state(conversation.id, %{"version" => 1, "state" => %{}}, store)

    assert :ok = Store.delete_conversation(conversation.id, store)
    assert {:error, :not_found} = Store.get_conversation(conversation.id, store)
    assert {:ok, []} = Store.list_messages(conversation.id, store)
    assert {:error, :not_found} = Store.load_state(conversation.id, store)
  end

  test "clear_messages keeps the conversation", %{store: store} do
    {:ok, conversation} = Store.create_conversation("coder", [], store)

    {:ok, _} =
      Store.append_messages(
        conversation.id,
        [message_attrs(message_type: "user", content: %{"text" => "hi"})],
        store
      )

    assert :ok = Store.clear_messages(conversation.id, store)
    assert {:ok, []} = Store.list_messages(conversation.id, store)
    assert {:ok, _conversation} = Store.get_conversation(conversation.id, store)
  end

  test "update_conversation changes title and status", %{store: store} do
    {:ok, conversation} = Store.create_conversation("coder", [], store)

    assert {:ok, renamed} =
             Store.update_conversation(conversation.id, %{title: "Renamed"}, store)

    assert renamed.title == "Renamed"

    assert {:ok, interrupted} =
             Store.update_conversation(conversation.id, %{status: "interrupted"}, store)

    assert interrupted.status == "interrupted"
  end
end
