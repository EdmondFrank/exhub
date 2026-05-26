defmodule Exhub.MCP.Brain.GitignoreParser do
  @moduledoc """
  Parses .gitignore files and checks if paths match ignore patterns.
  
  Implements a subset of the gitignore specification sufficient for
  typical Obsidian vault usage patterns.
  """
  
  @type pattern :: %{
    raw: String.t(),
    regex: Regex.t(),
    negate: boolean(),
    directory_only: boolean()
  }
  
  @doc """
  Parses gitignore content into a list of patterns.
  """
  @spec parse(String.t()) :: [pattern()]
  def parse(content) do
    content
    |> String.split("\n")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(String.starts_with?(&1, "#") or &1 == ""))
    |> Enum.map(&parse_pattern/1)
    |> Enum.reverse()  # Patterns are evaluated in reverse order
  end
  
  @doc """
  Checks if a path should be ignored based on gitignore patterns.
  
  The path should be relative to the gitignore file location.
  """
  @spec ignored?([pattern()], String.t()) :: boolean()
  def ignored?(patterns, path) do
    # Use Enum.reduce_while for early termination
    patterns
    |> Enum.reduce_while(false, fn pattern, ignored? ->
      if matches?(pattern, path) do
        {:halt, not pattern.negate}
      else
        {:cont, ignored?}
      end
    end)
  end
  
  defp parse_pattern(line) do
    {negate, line} = if String.starts_with?(line, "!") do
      {true, String.slice(line, 1..-1//1)}
    else
      {false, line}
    end
    
    {directory_only, line} = if String.ends_with?(line, "/") do
      {true, String.trim_trailing(line, "/")}
    else
      {false, line}
    end
    
    regex = glob_to_regex(line)
    
    %{
      raw: line,
      regex: regex,
      negate: negate,
      directory_only: directory_only
    }
  end
  
  defp glob_to_regex(glob) do
    # Use placeholders for regex parts to avoid conflicts
    {glob, placeholders} = 
      if String.contains?(glob, "**") do
        glob
        |> String.replace("**/", "§DOUBLE_STAR_SLASH§")
        |> String.replace("**", "§DOUBLE_STAR§")
        |> then(fn g -> {g, [{"§DOUBLE_STAR_SLASH§", "(.*/)*"}, {"§DOUBLE_STAR§", ".*"}]} end)
      else
        {glob, []}
      end
    
    # Escape dots and handle single asterisks
    glob = glob
    |> String.replace(".", "\\.")
    |> String.replace("*", "[^/]*")
    |> String.replace("?", "[^/]")
    
    # Restore placeholders
    glob = Enum.reduce(placeholders, glob, fn {placeholder, replacement}, acc ->
      String.replace(acc, placeholder, replacement)
    end)
    
    # Add anchors
    "^" <> glob <> "($|/)"
    |> Regex.compile!()
  end
  
  defp matches?(pattern, path) do
    if pattern.directory_only do
      # For directory patterns, match the directory itself or paths within it
      String.match?(path, pattern.regex) or String.starts_with?(path, pattern.raw <> "/")
    else
      String.match?(path, pattern.regex)
    end
  end
end