defmodule Exhub.Sagents.IntegrationTest do
  use ExUnit.Case, async: false

  alias Exhub.Sagents.Hub

  @moduletag :integration

  describe "Agent Hub end-to-end" do
    test "list agents returns all registered agents" do
      agents = Hub.list_agents()
      assert is_list(agents)
      assert Enum.any?(agents, &(&1.name == "assistant"))
      assert Enum.any?(agents, &(&1.name == "coder"))
      assert Enum.any?(agents, &(&1.name == "researcher"))
    end

    test "start and stop agent lifecycle" do
      {:ok, _pid} = Hub.start_agent("assistant")
      assert Hub.agent_running?("assistant")

      :ok = Hub.stop_agent("assistant")
      refute Hub.agent_running?("assistant")
    end

    test "get_status returns status for non-running agent" do
      status = Hub.get_status("nonexistent")
      assert status.status == :not_running
    end

    test "start_agent returns error for unknown agent" do
      assert {:error, :not_found} = Hub.start_agent("nonexistent-agent")
    end

    test "reset is idempotent for non-running agent" do
      assert :ok = Hub.reset("nonexistent")
    end

    test "stop_agent is idempotent for non-running agent" do
      assert :ok = Hub.stop_agent("nonexistent")
    end
  end
end
