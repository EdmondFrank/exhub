defmodule Exhub.MCP.Tools.AgentBusTest do
  use ExUnit.Case, async: false

  alias Anubis.Server.{Context, Frame, Response}
  alias Exhub.Agent.{Bus, Registry}

  alias Exhub.MCP.Tools.AgentBus.{
    Identity,
    Inbox,
    List,
    Post,
    Register,
    RoomHistory,
    RoomOpen,
    Send,
    Status
  }

  setup do
    home = tmp_home("agent-bus-tools")

    {:ok, registry} = Registry.start_link(home: Path.join(home, "agents"))
    {:ok, bus} = Bus.start_link(home: Path.join(home, "bus"))

    on_exit(fn ->
      if Process.alive?(registry), do: GenServer.stop(registry)
      if Process.alive?(bus), do: GenServer.stop(bus)
      File.rm_rf(home)
    end)

    :ok
  end

  defp tmp_home(prefix) do
    path = Path.join(System.tmp_dir!(), "exhub-#{prefix}-#{System.unique_integer([:positive])}")
    File.mkdir_p!(path)
    path
  end

  defp frame(headers \\ %{}) do
    %Frame{context: %Context{session_id: "session-1", headers: headers}}
  end

  defp scoped_frame(agent_id) do
    frame(%{"x-exhub-agent-id" => agent_id})
  end

  defp payload(%Response{isError: false, content: [%{"text" => json}]}), do: Jason.decode!(json)
  defp payload(%Response{isError: true, content: [%{"text" => text}]}), do: {:error, text}

  describe "agent_register" do
    test "creates the record and reports a memory path inside the configured home" do
      {:reply, response, _frame} =
        Register.execute(%{id: "coder", name: "Coder", capabilities: ["elixir"]}, frame())

      body = payload(response)
      assert body["agent"]["id"] == "coder"
      assert File.exists?(body["memory_path"])
      assert body["agent"]["status"] == "idle"
    end
  end

  describe "agent_identity" do
    test "auto-registers the caller identified by the scoped URL header" do
      {:reply, response, _frame} = Identity.execute(%{}, scoped_frame("reviewer"))
      body = payload(response)

      assert body["agent"]["id"] == "reviewer"
      assert body["auto_registered"] == true
      assert body["unread"] == %{}
    end

    test "errors when the caller cannot be resolved" do
      {:reply, response, _frame} = Identity.execute(%{}, frame())
      assert {:error, message} = payload(response)
      assert message =~ "unknown caller"
    end
  end

  describe "send and inbox" do
    test "a message sent by one agent is read by another" do
      {:reply, _, _} = Register.execute(%{id: "coder"}, frame())
      {:reply, _, _} = Register.execute(%{id: "reviewer"}, frame())

      {:reply, response, _} =
        Send.execute(
          %{to: "reviewer", body: "please review #42", kind: "ask", idempotency_key: "k1"},
          scoped_frame("coder")
        )

      assert %{"status" => "created", "message" => %{"id" => message_id}} = payload(response)

      {:reply, inbox_response, _} =
        Inbox.execute(%{as: "reviewer"}, frame())

      assert %{"messages" => [%{"id" => ^message_id, "from" => "coder", "kind" => "ask"}]} =
               payload(inbox_response)
    end

    test "a retried send with the same key is replayed, not duplicated" do
      params = %{to: "reviewer", body: "retry me", idempotency_key: "same"}

      {:reply, first, _} = Send.execute(params, scoped_frame("coder"))
      {:reply, second, _} = Send.execute(params, scoped_frame("coder"))

      assert payload(first)["message"]["id"] == payload(second)["message"]["id"]
      assert payload(second)["status"] == "replayed"
    end
  end

  describe "rooms" do
    test "members read a shared post and can replay it" do
      {:reply, _, _} = RoomOpen.execute(%{room: "build", as: "coder"}, frame())
      {:reply, _, _} = RoomOpen.execute(%{room: "build", as: "reviewer"}, frame())

      {:reply, response, _} =
        Post.execute(%{room: "build", body: "suite is green", as: "coder"}, frame())

      assert %{"message" => %{"seq" => 1}} = payload(response)

      {:reply, inbox_response, _} = Inbox.execute(%{as: "reviewer"}, frame())
      assert %{"messages" => [%{"body" => "suite is green"}]} = payload(inbox_response)

      {:reply, history_response, _} = RoomHistory.execute(%{room: "build"}, frame())
      assert %{"messages" => [%{"body" => "suite is green"}]} = payload(history_response)
    end
  end

  describe "listing and status" do
    test "agent_list shows status and unread counts" do
      {:reply, _, _} =
        Register.execute(%{id: "coder", status: "working", detail: "editing"}, frame())

      {:reply, _, _} = Send.execute(%{to: "coder", body: "ping"}, scoped_frame("reviewer"))

      {:reply, response, _} = List.execute(%{}, frame())

      assert %{
               "agents" => [
                 %{"id" => "coder", "status" => "working", "unread" => %{"agent:coder" => 1}}
               ]
             } =
               payload(response)
    end

    test "agent_bus_status is one snapshot of agents and rooms" do
      {:reply, _, _} = Register.execute(%{id: "coder"}, frame())
      {:reply, _, _} = RoomOpen.execute(%{room: "build", as: "coder"}, frame())

      {:reply, response, _} = Status.execute(%{}, frame())
      body = payload(response)

      assert body["protocol"] == "agent-bus/1"
      assert [%{"id" => "coder"}] = body["agents"]
      assert [%{"name" => "build", "members" => ["coder"]}] = body["rooms"]
    end
  end
end
