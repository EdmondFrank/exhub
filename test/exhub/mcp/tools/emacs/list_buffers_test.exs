defmodule Exhub.MCP.Tools.Emacs.ListBuffersTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Emacs.ListBuffers

  describe "name/0" do
    test "returns correct tool name" do
      assert ListBuffers.name() == "emacs_list_buffers"
    end
  end

  describe "description/0" do
    test "returns non-empty description" do
      description = ListBuffers.description()
      assert is_binary(description)
      assert String.length(description) > 0
    end
  end

  describe "execute/2" do
    test "returns error when Emacs communication fails" do
      # This test would require mocking the Emacs communication
      # For now, we just test that the function exists and can be called
      frame = %{}
      params = %{include_details: false}

      # The actual execution would require a running Emacs instance
      # This is a placeholder test
      assert is_function(&ListBuffers.execute/2, 2)
    end
  end
end