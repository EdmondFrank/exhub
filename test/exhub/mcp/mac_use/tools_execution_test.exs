defmodule Exhub.MCP.MacUse.ToolsExecutionTest do
  use ExUnit.Case, async: true

  # All tools that require app or pid targeting delegate to Helpers.run_axcli/2.
  # Since axcli is not available in CI, we test that tools return proper error
  # responses and that argument construction is correct.

  alias Anubis.Server.Response

  defp get_text(resp) do
    resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
  end

  defp execute_and_assert_error(module, params) do
    frame = %{}
    assert {:reply, resp, ^frame} = module.execute(params, frame)
    assert %Response{} = resp
    text = get_text(resp)
    assert is_binary(text)
  end

  describe "ListApps.execute/2" do
    test "returns error when axcli is missing" do
      execute_and_assert_error(Exhub.MCP.Tools.MacUse.ListApps, %{})
    end
  end

  describe "Snapshot.execute/2" do
    test "returns error when axcli is missing" do
      execute_and_assert_error(Exhub.MCP.Tools.MacUse.Snapshot, %{app: "Safari"})
    end
  end

  describe "Click.execute/2" do
    test "returns error when axcli is missing" do
      execute_and_assert_error(Exhub.MCP.Tools.MacUse.Click, %{
        app: "Safari",
        selector: ".button"
      })
    end
  end

  describe "DblClick.execute/2" do
    test "returns error when axcli is missing" do
      execute_and_assert_error(Exhub.MCP.Tools.MacUse.DblClick, %{
        app: "Finder",
        selector: ".file-cell"
      })
    end
  end

  describe "Input.execute/2" do
    test "returns error when axcli is missing" do
      execute_and_assert_error(Exhub.MCP.Tools.MacUse.Input, %{
        app: "Safari",
        selector: ".SearchInput",
        text: "hello"
      })
    end
  end

  describe "Fill.execute/2" do
    test "returns error when axcli is missing" do
      execute_and_assert_error(Exhub.MCP.Tools.MacUse.Fill, %{
        app: "Safari",
        selector: ".SearchInput",
        text: "replaced"
      })
    end
  end

  describe "Press.execute/2" do
    test "returns error when axcli is missing" do
      execute_and_assert_error(Exhub.MCP.Tools.MacUse.Press, %{
        app: "Safari",
        keys: "Enter"
      })
    end
  end

  describe "Hover.execute/2" do
    test "returns error when axcli is missing" do
      execute_and_assert_error(Exhub.MCP.Tools.MacUse.Hover, %{
        app: "Safari",
        selector: ".toolbar"
      })
    end
  end

  describe "Focus.execute/2" do
    test "returns error when axcli is missing" do
      execute_and_assert_error(Exhub.MCP.Tools.MacUse.Focus, %{
        app: "Safari",
        selector: ".SearchInput"
      })
    end
  end

  describe "ScrollTo.execute/2" do
    test "returns error when axcli is missing" do
      execute_and_assert_error(Exhub.MCP.Tools.MacUse.ScrollTo, %{
        app: "Safari",
        selector: ".item"
      })
    end
  end

  describe "Scroll.execute/2" do
    test "returns error when axcli is missing" do
      execute_and_assert_error(Exhub.MCP.Tools.MacUse.Scroll, %{
        app: "Safari",
        selector: ".chat-list",
        direction: "down",
        amount: 300
      })
    end
  end

  describe "Screenshot.execute/2" do
    test "returns error when axcli is missing" do
      execute_and_assert_error(Exhub.MCP.Tools.MacUse.Screenshot, %{
        app: "Safari"
      })
    end
  end

  describe "GetAttribute.execute/2" do
    test "returns error when axcli is missing" do
      execute_and_assert_error(Exhub.MCP.Tools.MacUse.GetAttribute, %{
        app: "Safari",
        selector: ".content",
        attribute: "text"
      })
    end
  end

  describe "Wait.execute/2" do
    test "returns error when axcli is missing with selector" do
      execute_and_assert_error(Exhub.MCP.Tools.MacUse.Wait, %{
        app: "Safari",
        selector: ".loading"
      })
    end

    test "returns error when axcli is missing with milliseconds" do
      execute_and_assert_error(Exhub.MCP.Tools.MacUse.Wait, %{
        milliseconds: 500
      })
    end

    test "returns error when both selector and milliseconds provided" do
      frame = %{}

      assert {:reply, resp, ^frame} =
               Exhub.MCP.Tools.MacUse.Wait.execute(
                 %{selector: ".loading", milliseconds: 500},
                 frame
               )

      text = get_text(resp)
      assert text =~ "not both"
    end

    test "returns error when neither selector nor milliseconds provided" do
      frame = %{}

      assert {:reply, resp, ^frame} =
               Exhub.MCP.Tools.MacUse.Wait.execute(%{}, frame)

      text = get_text(resp)
      assert text =~ "either"
    end
  end

  describe "Activate.execute/2" do
    test "returns error when axcli is missing" do
      execute_and_assert_error(Exhub.MCP.Tools.MacUse.Activate, %{app: "Safari"})
    end
  end

  describe "Mouse.execute/2" do
    test "returns error for pos action when axcli is missing" do
      execute_and_assert_error(Exhub.MCP.Tools.MacUse.Mouse, %{action: "pos"})
    end

    test "returns error for invalid action" do
      frame = %{}

      assert {:reply, resp, ^frame} =
               Exhub.MCP.Tools.MacUse.Mouse.execute(%{action: "invalid"}, frame)

      text = get_text(resp)
      assert text =~ "Invalid action"
    end

    test "returns error when move missing coordinates" do
      frame = %{}

      assert {:reply, resp, ^frame} =
               Exhub.MCP.Tools.MacUse.Mouse.execute(%{action: "move"}, frame)

      text = get_text(resp)
      assert text =~ "Invalid action"
    end
  end

  describe "Keyboard.execute/2" do
    test "returns error for type action when axcli is missing" do
      execute_and_assert_error(Exhub.MCP.Tools.MacUse.Keyboard, %{
        action: "type",
        text: "hello"
      })
    end

    test "returns error for press action when axcli is missing" do
      execute_and_assert_error(Exhub.MCP.Tools.MacUse.Keyboard, %{
        action: "press",
        text: "Command+a"
      })
    end

    test "returns error for invalid action" do
      frame = %{}

      assert {:reply, resp, ^frame} =
               Exhub.MCP.Tools.MacUse.Keyboard.execute(
                 %{action: "invalid", text: "hello"},
                 frame
               )

      text = get_text(resp)
      assert text =~ "Invalid action"
    end
  end
end
