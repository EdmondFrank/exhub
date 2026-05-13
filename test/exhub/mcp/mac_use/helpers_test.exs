defmodule Exhub.MCP.MacUse.HelpersTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.MacUse.Helpers

  describe "app_args/1" do
    test "returns --app for string key" do
      assert Helpers.app_args(%{"app" => "Safari"}) == ["--app", "Safari"]
    end

    test "returns --app for atom key" do
      assert Helpers.app_args(%{app: "Safari"}) == ["--app", "Safari"]
    end

    test "returns --pid for string key" do
      assert Helpers.app_args(%{"pid" => 1234}) == ["--pid", "1234"]
    end

    test "returns --pid for atom key" do
      assert Helpers.app_args(%{pid: 1234}) == ["--pid", "1234"]
    end

    test "returns empty list when no app or pid" do
      assert Helpers.app_args(%{}) == []
    end

    test "returns empty list for unrelated params" do
      assert Helpers.app_args(%{selector: ".button"}) == []
    end
  end

  describe "strategy_args/1" do
    test "returns --strategy for string key" do
      assert Helpers.strategy_args(%{"strategy" => "cg-pid"}) == ["--strategy", "cg-pid"]
    end

    test "returns --strategy for atom key" do
      assert Helpers.strategy_args(%{strategy: "ax"}) == ["--strategy", "ax"]
    end

    test "returns empty list when no strategy" do
      assert Helpers.strategy_args(%{}) == []
    end
  end

  describe "cursor_args/1" do
    test "returns --no-visual-cursor for string key" do
      assert Helpers.cursor_args(%{"no_visual_cursor" => true}) == ["--no-visual-cursor"]
    end

    test "returns --no-visual-cursor for atom key" do
      assert Helpers.cursor_args(%{no_visual_cursor: true}) == ["--no-visual-cursor"]
    end

    test "returns empty list when no_visual_cursor is false" do
      assert Helpers.cursor_args(%{no_visual_cursor: false}) == []
    end

    test "returns empty list when no_visual_cursor is absent" do
      assert Helpers.cursor_args(%{}) == []
    end
  end

  describe "toon_response/2" do
    test "encodes map data" do
      resp = Anubis.Server.Response.tool()
      result = Helpers.toon_response(resp, %{"key" => "value"})
      assert %Anubis.Server.Response{} = result
      assert result.isError == false
      text = result.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert is_binary(text)
      assert text =~ "key"
    end

    test "passes through binary data" do
      resp = Anubis.Server.Response.tool()
      result = Helpers.toon_response(resp, "plain text")
      text = result.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text == "plain text"
    end

    test "inspects non-map, non-binary data" do
      resp = Anubis.Server.Response.tool()
      result = Helpers.toon_response(resp, [:a, :b])
      text = result.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ ":a"
    end
  end

  describe "run_axcli/2" do
    test "returns ok for valid command or error if not found" do
      # axcli may or may not be installed in the test environment
      result = Helpers.run_axcli(["--version"])

      case result do
        {:ok, output} -> assert is_binary(output)
        {:error, msg} -> assert is_binary(msg)
      end
    end

    test "returns error for invalid subcommand" do
      result = Helpers.run_axcli(["--nonexistent-flag-xyz"])
      assert {:error, _msg} = result
    end
  end
end
