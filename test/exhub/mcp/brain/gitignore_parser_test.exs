defmodule Exhub.MCP.Brain.GitignoreParserTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Brain.GitignoreParser

  describe "parse/1" do
    test "parses simple patterns" do
      content = """
      *.log
      *.tmp
      """
      
      patterns = GitignoreParser.parse(content)
      assert length(patterns) == 2
    end
    
    test "handles comments and blank lines" do
      content = """
      # This is a comment
      *.log
      
      *.tmp
      """
      
      patterns = GitignoreParser.parse(content)
      assert length(patterns) == 2
    end
    
    test "handles negation patterns" do
      content = """
      *.log
      !important.log
      """
      
      patterns = GitignoreParser.parse(content)
      assert length(patterns) == 2
      assert Enum.any?(patterns, & &1.negate)
    end
  end
  
  describe "ignored?/2" do
    test "matches simple wildcard patterns" do
      content = "*.log"
      patterns = GitignoreParser.parse(content)
      
      assert GitignoreParser.ignored?(patterns, "debug.log")
      # According to gitignore spec, *.log without slash only matches in same directory
      refute GitignoreParser.ignored?(patterns, "logs/debug.log")
      refute GitignoreParser.ignored?(patterns, "debug.txt")
    end
    
    test "matches directory patterns" do
      content = "build/"
      patterns = GitignoreParser.parse(content)
      
      assert GitignoreParser.ignored?(patterns, "build")
      assert GitignoreParser.ignored?(patterns, "build/output")
      refute GitignoreParser.ignored?(patterns, "build.txt")
    end
    
    test "handles negation patterns" do
      content = """
      *.log
      !important.log
      """
      patterns = GitignoreParser.parse(content)
      
      assert GitignoreParser.ignored?(patterns, "debug.log")
      refute GitignoreParser.ignored?(patterns, "important.log")
    end
    
    test "handles patterns with path separators" do
      content = "docs/*.pdf"
      patterns = GitignoreParser.parse(content)
      
      assert GitignoreParser.ignored?(patterns, "docs/manual.pdf")
      refute GitignoreParser.ignored?(patterns, "manual.pdf")
      refute GitignoreParser.ignored?(patterns, "docs/sub/manual.pdf")
    end
    
    test "handles patterns with double asterisks" do
      content = "**/temp"
      patterns = GitignoreParser.parse(content)
      
      assert GitignoreParser.ignored?(patterns, "temp")
      assert GitignoreParser.ignored?(patterns, "a/b/temp")
      assert GitignoreParser.ignored?(patterns, "temp/file.txt")
    end
  end
end