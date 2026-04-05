defmodule Exhub.MCP.Tools.Desktop.EditBlock do
  @moduledoc """
  MCP Tool: edit_block

  Perform a targeted find-and-replace edit within a text file.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "edit_block"

  @impl true
  def description do
    """
    Perform a targeted find-and-replace edit within a text file.

    Finds the exact occurrence of `old_string` in the file and replaces it with
    `new_string`. The match is exact (case-sensitive). By default, exactly one
    replacement is expected; set `expected_replacements` to allow more.

    Use this tool for surgical edits — prefer it over rewriting the whole file
    when only a small section needs to change.

    Parameters:
    - file_path: Absolute path to the file to edit
    - old_string: The exact text to find and replace
    - new_string: The replacement text
    - expected_replacements: Number of replacements expected (default 1)
    """
  end

  schema do
    field(:file_path, {:required, :string}, description: "Absolute path to the file to edit")
    field(:old_string, {:required, :string}, description: "The exact text to find and replace")
    field(:new_string, {:required, :string}, description: "The replacement text")
    field(:expected_replacements, :integer, description: "Number of replacements expected (default 1)", default: 1)
  end

  @impl true
  def execute(params, frame) do
    file_path = Map.get(params, :file_path) |> Helpers.expand_path()
    old_string = Map.get(params, :old_string)
    new_string = Map.get(params, :new_string)
    expected = Map.get(params, :expected_replacements, 1)

    case perform_edit(file_path, old_string, new_string, expected) do
      {:ok, replacements} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{
            "success" => true,
            "file_path" => file_path,
            "replacements_made" => replacements,
            "message" => "Edit applied successfully."
          })

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Edit failed: #{reason}")
        {:reply, resp, frame}
    end
  end

  defp perform_edit(file_path, old_string, new_string, expected) do
    with :ok <- validate_inputs(old_string, new_string),
         {:ok, content} <- read_file(file_path) do
      count = count_occurrences(content, old_string)

      cond do
        count == 0 ->
          {:error, "String not found in file. The old_string must match exactly (case-sensitive)."}

        count != expected ->
          {:error,
           "Expected #{expected} replacement(s) but found #{count} occurrence(s). " <>
             "Adjust expected_replacements or make old_string more specific."}

        true ->
          new_content = String.replace(content, old_string, new_string)

          case File.write(file_path, new_content) do
            :ok -> {:ok, count}
            {:error, :eacces} -> {:error, "Permission denied: #{file_path}"}
            {:error, reason} -> {:error, inspect(reason)}
          end
      end
    end
  end

  defp read_file(path) do
    case File.read(path) do
      {:ok, content} -> {:ok, content}
      {:error, :enoent} -> {:error, "File not found: #{path}"}
      {:error, :eacces} -> {:error, "Permission denied: #{path}"}
      {:error, reason} -> {:error, inspect(reason)}
    end
  end

  defp count_occurrences(content, pattern) do
    content
    |> String.split(pattern)
    |> length()
    |> Kernel.-(1)
  end

  defp validate_inputs(old_string, _new_string) when old_string == "" do
    {:error, "old_string cannot be empty"}
  end

  defp validate_inputs(_old_string, _new_string), do: :ok
end
