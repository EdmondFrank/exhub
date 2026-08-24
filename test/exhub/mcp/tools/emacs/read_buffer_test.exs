defmodule Exhub.MCP.Tools.Emacs.ReadBufferTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Emacs.ReadBuffer

  describe "build_read_command/4" do
    test "forward mode without lines reads the whole buffer" do
      cmd = ReadBuffer.build_read_command("*scratch*", nil, nil, false)

      assert cmd == """
             (with-current-buffer "*scratch*"
               (buffer-substring-no-properties (point-min) (point-max)))
             """
    end

    test "forward mode with range counts from the top" do
      cmd = ReadBuffer.build_read_command("file.el", 10, 20, false)

      assert cmd =~ "(goto-char (point-min))"
      assert cmd =~ "(forward-line (1- 10))"
      assert cmd =~ "(- 20 10)"
      refute cmd =~ "line-number-at-pos"
    end

    test "reverse mode with end_line only returns a tail command" do
      cmd = ReadBuffer.build_read_command("*exhub*", nil, 50, true)

      assert cmd =~ "(goto-char (point-max))"
      assert cmd =~ "(line-number-at-pos (point))"
      assert cmd =~ "(or nil 1)"
      assert cmd =~ "(or 50 total)"
      assert cmd =~ "(max 1 (+ 1 (- total rev-e)))"
      assert cmd =~ "(min total (+ 1 (- total rev-s)))"
    end

    test "reverse mode with both bounds" do
      cmd = ReadBuffer.build_read_command("*exhub*", 10, 20, true)

      assert cmd =~ "(or 10 1)"
      assert cmd =~ "(or 20 total)"
    end

    test "reverse mode without bounds falls back to full range" do
      cmd = ReadBuffer.build_read_command("*exhub*", nil, nil, true)

      assert cmd =~ "(or nil 1)"
      assert cmd =~ "(or nil total)"
    end

    test "escapes buffer names for elisp" do
      name = ~s(we"ird) <> "\n" <> "buffer"
      cmd = ReadBuffer.build_read_command(name, nil, nil, false)

      # `"` becomes \" and a real newline becomes the two-char \n sequence
      assert cmd == """
             (with-current-buffer "we\\"ird\\nbuffer"
               (buffer-substring-no-properties (point-min) (point-max)))
             """
    end
  end
end
