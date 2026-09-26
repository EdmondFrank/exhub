defmodule Exhub.BrowserAgent.ScrollTest do
  use ExUnit.Case, async: true

  alias Exhub.BrowserAgent.Scroll

  describe "script/1" do
    test "moves the element that actually scrolls, not the window" do
      script = Scroll.script(:down)

      assert script =~ "document.scrollingElement"
      assert script =~ "querySelectorAll"
      assert script =~ "el.scrollTop += amount"
      refute script =~ "window.scrollBy"
    end

    test "prefers the main content pane over a scrolling sidebar" do
      script = Scroll.script(:down)

      assert script =~ ~s|querySelectorAll("main, [role=main], article")|
      assert script =~ "querySelectorAll(\"div, section\")"
    end

    test "scrolls up by a negative viewport and down by a positive one" do
      assert Scroll.script(:up) =~ "-window.innerHeight"
      refute Scroll.script(:down) =~ "-window.innerHeight"
    end

    test "returns a string so the HTTP backend need not encode the result" do
      assert Scroll.script(:down) =~ "return String(el.scrollTop)"
    end
  end
end
