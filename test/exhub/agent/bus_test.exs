defmodule Exhub.Agent.BusTest do
  use ExUnit.Case, async: false

  alias Exhub.Agent.Bus

  setup do
    home = tmp_home("bus")
    start_bus(home)
    %{home: home}
  end

  defp tmp_home(prefix) do
    unique = System.unique_integer([:positive])
    path = Path.join(System.tmp_dir!(), "exhub-#{prefix}-#{unique}")
    File.mkdir_p!(path)
    on_exit(fn -> File.rm_rf(path) end)
    path
  end

  defp start_bus(home) do
    {:ok, pid} = Bus.start_link(home: home)

    on_exit(fn ->
      if Process.alive?(pid), do: GenServer.stop(pid)
    end)

    pid
  end

  defp send_dm(from, to, body, attrs \\ %{}) do
    Bus.send_message(Map.merge(%{"from" => from, "to" => to, "body" => body}, attrs))
  end

  describe "direct messages" do
    test "a message is durable on send and delivered once" do
      assert {:ok, message, :created} =
               send_dm("coder", "reviewer", "please review PR 12", %{"kind" => "ask"})

      assert message.kind == "ask"
      assert message.seq == 1
      assert message.stream == "agent:reviewer"

      assert {:ok, payload} = Bus.inbox("reviewer")
      assert [delivered] = payload.messages
      assert delivered.id == message.id
      assert payload.unread == 0

      assert {:ok, empty} = Bus.inbox("reviewer")
      assert empty.messages == []
    end

    test "peek does not advance the read position" do
      {:ok, _, :created} = send_dm("coder", "reviewer", "hello")

      assert {:ok, first} = Bus.inbox("reviewer", peek: true)
      assert length(first.messages) == 1

      assert {:ok, second} = Bus.inbox("reviewer")
      assert length(second.messages) == 1
    end

    test "rejects an empty body and an unknown kind" do
      assert {:error, :empty_body} = send_dm("coder", "reviewer", "   ")

      assert {:error, {:invalid_kind, "shout"}} =
               send_dm("coder", "reviewer", "hi", %{"kind" => "shout"})
    end

    test "accepts @-prefixed recipient ids" do
      assert {:ok, message, :created} = send_dm("coder", "@reviewer", "hi")
      assert message.stream == "agent:reviewer"
    end
  end

  describe "idempotency" do
    test "reusing a key with the same body returns the original message" do
      attrs = %{"idempotency_key" => "k1"}
      assert {:ok, first, :created} = send_dm("coder", "reviewer", "once", attrs)
      assert {:ok, second, :replayed} = send_dm("coder", "reviewer", "once", attrs)
      assert first.id == second.id
    end

    test "reusing a key with a different body is an error" do
      attrs = %{"idempotency_key" => "k1"}
      {:ok, _, :created} = send_dm("coder", "reviewer", "once", attrs)
      assert {:error, :idempotency_key_reused} = send_dm("coder", "reviewer", "twice", attrs)
    end
  end

  describe "long polling" do
    test "inbox waits for a message instead of returning empty" do
      task = Task.async(fn -> Bus.inbox("reviewer", wait_ms: 2_000) end)
      Process.sleep(100)
      {:ok, _, :created} = send_dm("coder", "reviewer", "ping")

      assert {:ok, payload} = Task.await(task, 3_000)
      assert [%{body: "ping"}] = payload.messages
    end

    test "inbox returns empty after the wait expires" do
      assert {:ok, payload} = Bus.inbox("reviewer", wait_ms: 150)
      assert payload.messages == []
      assert payload.unread == 0
    end
  end

  describe "rooms" do
    test "a post is visible to every member and replayable by seq" do
      {:ok, _room} = Bus.open_room("build", by: "reviewer")

      assert {:ok, _message, :created} =
               Bus.post(%{"from" => "coder", "room" => "build", "body" => "tests are green"})

      # posting joins the sender
      assert [%{name: "build", members: members}] = Bus.rooms()
      assert "coder" in members

      assert {:ok, payload} = Bus.inbox("reviewer")
      assert [%{body: "tests are green", seq: 1}] = payload.messages

      assert {:ok, history} = Bus.history("room:build", since_seq: 0)
      assert length(history.messages) == 1

      assert {:ok, after_seq} = Bus.history("room:build", since_seq: 1)
      assert after_seq.messages == []
    end

    test "an agent outside the room does not receive its posts" do
      {:ok, _} = Bus.open_room("build", by: "reviewer")

      {:ok, _, :created} =
        Bus.post(%{"from" => "reviewer", "room" => "build", "body" => "for members"})

      assert {:ok, payload} = Bus.inbox("outsider")
      assert payload.messages == []
    end

    test "rejects an invalid room name" do
      assert {:error, :invalid_room_name} = Bus.open_room("Not A Room", by: "coder")
    end
  end

  describe "persistence" do
    test "messages survive a restart and are redelivered", %{home: home} do
      {:ok, _, :created} = send_dm("coder", "reviewer", "durable")

      GenServer.stop(Process.whereis(Bus))
      start_bus(home)

      assert {:ok, payload} = Bus.inbox("reviewer")
      assert [%{body: "durable"}] = payload.messages
    end

    test "read positions survive a restart", %{home: home} do
      {:ok, _, :created} = send_dm("coder", "reviewer", "read once")
      {:ok, _} = Bus.inbox("reviewer")

      GenServer.stop(Process.whereis(Bus))
      start_bus(home)

      assert {:ok, payload} = Bus.inbox("reviewer")
      assert payload.messages == []
    end

    test "rooms survive a restart", %{home: home} do
      {:ok, _} = Bus.open_room("build", by: "coder")

      GenServer.stop(Process.whereis(Bus))
      start_bus(home)

      assert [%{name: "build"}] = Bus.rooms()
    end
  end
end
