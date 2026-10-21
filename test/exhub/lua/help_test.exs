defmodule Exhub.Lua.HelpTest do
  use ExUnit.Case, async: true

  alias Exhub.Lua.Help

  @tools [
    %{
      "server" => "demo",
      "name" => "echo",
      "description" => "Echo a message back\n\nSecond paragraph is not indexed.",
      "inputSchema" => %{
        "type" => "object",
        "properties" => %{
          "msg" => %{"type" => "string", "description" => "the message"},
          "times" => %{"type" => "integer"}
        },
        "required" => ["msg"]
      }
    },
    %{
      "server" => "web-tools",
      "name" => "fetch",
      "description" => "Fetch a URL",
      "inputSchema" => %{"type" => "object", "properties" => %{"url" => %{"type" => "string"}}}
    },
    %{"server" => "demo", "name" => "quiet", "description" => "", "inputSchema" => %{}},
    %{"name" => "orphan", "description" => "no server"}
  ]

  describe "index/1" do
    test "one line per tool, sorted, first description line only" do
      text = Help.index(@tools)

      [first, second, third | _rest] = String.split(text, "\n")

      assert first == "demo.echo - Echo a message back"
      assert second == "demo.quiet - (no description)"
      assert third == "web-tools.fetch - Fetch a URL"
      refute text =~ "Second paragraph"
      # Entries without a server are skipped, not rendered malformed.
      refute text =~ "orphan"
      assert text =~ "help(\"server.tool\")"
    end

    test "reports an empty set" do
      assert Help.index([]) == "No tools are visible."
      assert Help.index(nil) == "No tools are visible."
    end
  end

  describe "render/2" do
    test "renders one tool's description, parameters and call hint" do
      text = Help.render(@tools, "demo.echo")

      assert text =~ "# demo.echo"
      assert text =~ "Echo a message back"
      assert text =~ "Parameters:"
      assert text =~ "msg: string (required) - the message"
      assert text =~ "times: integer"
      refute text =~ "times: integer (required)"
      assert text =~ ~s|Call: demo.echo({...}) or tools["demo__echo"]({...})|
    end

    test "sanitizes nested names in the call hint" do
      text = Help.render(@tools, "web-tools.fetch")

      assert text =~ ~s|Call: web_tools.fetch({...}) or tools["web-tools__fetch"]({...})|
    end

    test "accepts the flat server__tool form and a bare name" do
      assert Help.render(@tools, "demo__echo") =~ "# demo.echo"
      assert Help.render(@tools, "echo") =~ "# demo.echo"
      assert Help.render(@tools, "tools.demo.echo") =~ "# demo.echo"
    end

    test "a blank path falls back to the index" do
      assert Help.render(@tools, nil) == Help.index(@tools)
      assert Help.render(@tools, "") == Help.index(@tools)
    end

    test "an unknown path reports UnknownTool with a discovery hint" do
      text = Help.render(@tools, "nope__nope")

      assert text =~ "UnknownTool"
      assert text =~ "help()"
      assert text =~ "retrieve_tools"
    end

    test "an ambiguous bare name is reported instead of guessing" do
      tools = [
        %{"server" => "a", "name" => "read"},
        %{"server" => "b", "name" => "read"}
      ]

      text = Help.render(tools, "read")

      assert text =~ "AmbiguousTool"
      assert text =~ "a.read"
      assert text =~ "b.read"
    end
  end

  describe "resolve/2" do
    test "returns the tool for a known path and an error otherwise" do
      assert {:ok, {"demo.echo", %{"name" => "echo"}}} = Help.resolve(@tools, "demo.echo")
      assert {:error, message} = Help.resolve(@tools, "nope")
      assert message =~ "UnknownTool"
      assert {:error, _} = Help.resolve(@tools, nil)
    end
  end
end
