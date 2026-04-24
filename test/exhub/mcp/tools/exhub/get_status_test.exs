defmodule Exhub.MCP.Tools.Exhub.GetStatusTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Exhub.GetStatus

  describe "execute/2" do
    test "returns runtime statistics" do
      frame = %{}
      assert {:reply, resp, ^frame} = GetStatus.execute(%{}, frame)

      assert resp.isError == false
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      {:ok, result} = Toon.decode(text)

      assert result["uptime_ms"] >= 0
      assert result["memory_bytes"] > 0
      assert result["process_count"] > 0
      assert result["atom_count"] > 0
      assert result["port_count"] >= 0
    end
  end
end
