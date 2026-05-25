defmodule Exhub.MCP.EmacsServerTest do
  use ExUnit.Case, async: true

  describe "module loading" do
    test "EmacsServer module exists" do
      assert is_atom(Exhub.MCP.EmacsServer)
      # Check that the module has the expected functions
      functions = Exhub.MCP.EmacsServer.__info__(:functions)
      assert {:init, 2} in functions
      assert {:handle_request, 2} in functions
    end

    test "ListBuffers module exists" do
      assert is_atom(Exhub.MCP.Tools.Emacs.ListBuffers)
      functions = Exhub.MCP.Tools.Emacs.ListBuffers.__info__(:functions)
      assert {:name, 0} in functions
      assert {:description, 0} in functions
      assert {:execute, 2} in functions
    end

    test "ReadBuffer module exists" do
      assert is_atom(Exhub.MCP.Tools.Emacs.ReadBuffer)
      functions = Exhub.MCP.Tools.Emacs.ReadBuffer.__info__(:functions)
      assert {:name, 0} in functions
      assert {:description, 0} in functions
      assert {:execute, 2} in functions
    end

    test "WriteBuffer module exists" do
      assert is_atom(Exhub.MCP.Tools.Emacs.WriteBuffer)
      functions = Exhub.MCP.Tools.Emacs.WriteBuffer.__info__(:functions)
      assert {:name, 0} in functions
      assert {:description, 0} in functions
      assert {:execute, 2} in functions
    end

    test "CloseBuffer module exists" do
      assert is_atom(Exhub.MCP.Tools.Emacs.CloseBuffer)
      functions = Exhub.MCP.Tools.Emacs.CloseBuffer.__info__(:functions)
      assert {:name, 0} in functions
      assert {:description, 0} in functions
      assert {:execute, 2} in functions
    end

    test "Helpers module exists" do
      assert is_atom(Exhub.MCP.Emacs.Helpers)
      functions = Exhub.MCP.Emacs.Helpers.__info__(:functions)
      assert {:send_command, 1} in functions
      assert {:send_command, 2} in functions
      assert {:parse_buffer_list, 1} in functions
    end
  end

  describe "tool names" do
    test "ListBuffers has correct name" do
      assert Exhub.MCP.Tools.Emacs.ListBuffers.name() == "emacs_list_buffers"
    end

    test "ReadBuffer has correct name" do
      assert Exhub.MCP.Tools.Emacs.ReadBuffer.name() == "emacs_read_buffer"
    end

    test "WriteBuffer has correct name" do
      assert Exhub.MCP.Tools.Emacs.WriteBuffer.name() == "emacs_write_buffer"
    end

    test "CloseBuffer has correct name" do
      assert Exhub.MCP.Tools.Emacs.CloseBuffer.name() == "emacs_close_buffer"
    end
  end

  describe "tool descriptions" do
    test "ListBuffers has description" do
      description = Exhub.MCP.Tools.Emacs.ListBuffers.description()
      assert is_binary(description)
      assert String.length(description) > 0
      assert description =~ "buffer"
    end

    test "ReadBuffer has description" do
      description = Exhub.MCP.Tools.Emacs.ReadBuffer.description()
      assert is_binary(description)
      assert String.length(description) > 0
      assert description =~ "buffer"
    end

    test "WriteBuffer has description" do
      description = Exhub.MCP.Tools.Emacs.WriteBuffer.description()
      assert is_binary(description)
      assert String.length(description) > 0
      assert description =~ "buffer"
    end

    test "CloseBuffer has description" do
      description = Exhub.MCP.Tools.Emacs.CloseBuffer.description()
      assert is_binary(description)
      assert String.length(description) > 0
      assert description =~ "buffer"
    end
  end
end