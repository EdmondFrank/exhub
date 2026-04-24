defmodule Exhub.MCP.Tools.Exhub.CompileTest do
  use ExUnit.Case, async: false

  alias Exhub.MCP.Tools.Exhub.Compile

  describe "execute/2" do
    test "returns compile result or error depending on Mix availability" do
      frame = %{}
      assert {:reply, resp, ^frame} = Compile.execute(%{}, frame)

      if Code.ensure_loaded?(Mix) do
        # Mix is available — we expect a compile result
        assert resp.isError == false
        text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
        {:ok, result} = Toon.decode(text)
        assert is_integer(result["exit_code"])
      else
        # Mix is not available — expect error
        assert resp.isError == true
        text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
        assert text =~ "Mix is not available"
      end
    end
  end
end
