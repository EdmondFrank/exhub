defmodule Exhub.MCP.Tools.Exhub.RestartTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Exhub.Restart

  describe "execute/2" do
    test "schedules a soft restart by default" do
      frame = %{}
      assert {:reply, resp, ^frame} = Restart.execute(%{}, frame)

      assert resp.isError == false
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      {:ok, result} = Toon.decode(text)

      assert result["scheduled"] == true
      assert result["mode"] == "soft"
      assert is_integer(result["delay_ms"])
      assert is_binary(result["message"])
    end

    test "schedules a hard restart when requested" do
      frame = %{}
      assert {:reply, resp, ^frame} = Restart.execute(%{mode: "hard"}, frame)

      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      {:ok, result} = Toon.decode(text)

      assert result["scheduled"] == true
      assert result["mode"] == "hard"
    end
  end
end
