defmodule Exhub.MCP.Tools.Lsp.Definition do
  @moduledoc "MCP Tool: `lsp_definition` — go-to-definition for any file."

  alias Exhub.LspBridge.Headless
  alias Exhub.MCP.Tools.Lsp.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "lsp_definition"

  @impl true
  def description do
    """
    Go-to-definition at a position, for a file in any project on this machine.

    Positions are 1-based: the first character of a file is `1:1` (so an
    editor's `line 42, column 9` is `line: 42, character: 9`). The file is
    re-read from disk and resolved against its own project root, so the
    definition can point into another package in the same workspace.

    Returns `file:line:character` for every definition, or a message when the
    server knows none. Prefer this over grep when you need the real target
    (overloads, generated code, cross-package jumps).
    """
  end

  schema do
    field(:file_path, {:required, :string}, description: "Path to the file.")

    field(:line, {:required, :integer}, description: "1-based line of the symbol.")

    field(:character, {:required, :integer}, description: "1-based column of the symbol.")

    field(:server, :string, description: "Pin a language server by name.")

    field(:project_path, :string, description: "Authoritative project root override.")
  end

  @impl true
  def execute(params, frame) do
    with {:ok, position} <- Helpers.position(params),
         file_path when is_binary(file_path) <- Map.get(params, :file_path) do
      params_map = %{"position" => position}

      case Headless.request(
             file_path,
             "textDocument/definition",
             params_map,
             Helpers.opts(params)
           ) do
        {:ok, result} ->
          text =
            case Helpers.format_locations(result) do
              {:ok, nil, 0} -> "No definition found."
              {:ok, rendered, count} -> "Found #{count} definition(s):\n#{rendered}"
            end

          Helpers.ok(frame, Helpers.append_failures(text, result))

        {:error, reason} ->
          Helpers.error(frame, "lsp_definition failed: #{inspect(reason)}")
      end
    else
      {:error, message} -> Helpers.error(frame, message)
      _ -> Helpers.error(frame, "`file_path` is required.")
    end
  end
end
