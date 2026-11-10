defmodule Exhub.MCP.Hub.InstructionsTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Hub.Instructions

  describe "from_servers/2 (hub-wide)" do
    test "renders the connection preamble plus one section per enabled server" do
      text =
        Instructions.from_servers([
          %{name: "chrome", enabled: true, instructions: "Act in batches."},
          %{name: "deepwiki", enabled: true, instructions: nil}
        ])

      assert text =~ "retrieve_tools"
      assert text =~ "## chrome\nAct in batches."
      # A server with no guidance contributes nothing at all.
      refute text =~ "## deepwiki"
    end

    test "skips disabled servers" do
      text =
        Instructions.from_servers([
          %{name: "chrome", enabled: false, instructions: "Act in batches."}
        ])

      refute text =~ "## chrome"
    end

    test "orders sections by server name so the text is stable" do
      text =
        Instructions.from_servers([
          %{name: "zeta", enabled: true, instructions: "z"},
          %{name: "alpha", enabled: true, instructions: "a"}
        ])

      assert :binary.match(text, "## alpha") < :binary.match(text, "## zeta")
    end

    test "is bounded" do
      text =
        Instructions.from_servers([
          %{name: "big", enabled: true, instructions: String.duplicate("x", 20_000)}
        ])

      assert String.length(text) <= 8_000
    end
  end

  describe "from_servers/2 (single server)" do
    test "renders that server's section" do
      assert Instructions.from_servers(
               [%{name: "chrome", enabled: true, instructions: "Act in batches."}],
               "chrome"
             ) == "## chrome\nAct in batches."
    end

    test "returns nil when the server publishes nothing or is absent" do
      assert Instructions.from_servers([%{name: "chrome", enabled: true}], "chrome") == nil
      assert Instructions.from_servers([], "chrome") == nil
    end
  end

  describe "compose/0" do
    test "still returns the preamble without a running ClientManager" do
      assert Instructions.compose() =~ "retrieve_tools"
    end
  end
end
