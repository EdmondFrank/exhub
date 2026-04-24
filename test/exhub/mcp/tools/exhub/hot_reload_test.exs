defmodule Exhub.MCP.Tools.Exhub.HotReloadTest do
  use ExUnit.Case, async: false

  alias Exhub.MCP.Tools.Exhub.HotReload

  describe "execute/2" do
    test "returns reload statistics" do
      frame = %{}
      assert {:reply, resp, ^frame} = HotReload.execute(%{}, frame)

      assert resp.isError == false
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      {:ok, result} = Toon.decode(text)

      assert is_integer(result["ok_count"])
      assert is_integer(result["error_count"])
      assert is_binary(result["summary"])
      assert result["error_count"] >= 0
    end
  end
end
