defmodule Exhub.MCP.Tools.Exhub.GetVersionTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Exhub.GetVersion

  describe "execute/2" do
    test "returns version information" do
      frame = %{}
      assert {:reply, resp, ^frame} = GetVersion.execute(%{}, frame)

      assert resp.isError == false
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      {:ok, result} = Toon.decode(text)

      assert is_binary(result["exhub_version"])
      assert is_binary(result["elixir_version"])
      assert is_binary(result["otp_version"])
      assert is_binary(result["erts_version"])
    end
  end
end
