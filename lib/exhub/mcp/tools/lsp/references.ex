defmodule Exhub.MCP.Tools.Lsp.References do
  @moduledoc "MCP Tool: `lsp_references` — find every reference to a symbol."

  alias Exhub.LspBridge.Headless
  alias Exhub.MCP.Tools.Lsp.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "lsp_references"

  @impl true
  def description do
    """
    Every reference to the symbol at a position, workspace-wide, for a file in
    any project on this machine.

    Positions are 1-based (`1:1` is the first character). `include_declaration`
    (default true) adds the definition itself. The file is re-read from disk
    and resolved against its own project root, so references from other
    packages in the same workspace are found too.

    Use this before renaming or changing a signature — it is exact
    (type-aware), unlike a textual grep. Returns one `file:line:character` per
    reference, capped by `max`.
    """
  end

  schema do
    field(:file_path, {:required, :string}, description: "Path to the file.")

    field(:line, {:required, :integer}, description: "1-based line of the symbol.")

    field(:character, {:required, :integer}, description: "1-based column of the symbol.")

    field(:include_declaration, :boolean,
      description: "Include the declaration itself (default true).",
      default: true
    )

    field(:max, :integer,
      description: "Maximum references to report (default 200).",
      default: 200
    )

    field(:server, :string, description: "Pin a language server by name.")

    field(:project_path, :string, description: "Authoritative project root override.")
  end

  @impl true
  def execute(params, frame) do
    with {:ok, position} <- Helpers.position(params),
         file_path when is_binary(file_path) <- Map.get(params, :file_path) do
      params_map = %{
        "position" => position,
        "context" => %{"includeDeclaration" => Map.get(params, :include_declaration, true)}
      }

      case Headless.request(
             file_path,
             "textDocument/references",
             params_map,
             Helpers.opts(params)
           ) do
        {:ok, result} ->
          limit = Map.get(params, :max) || 200

          text =
            case Helpers.format_locations(result, limit) do
              {:ok, nil, 0} -> "No references found."
              {:ok, rendered, count} -> "Found #{count} reference(s):\n#{rendered}"
            end

          Helpers.ok(frame, Helpers.append_failures(text, result))

        {:error, reason} ->
          Helpers.error(frame, "lsp_references failed: #{inspect(reason)}")
      end
    else
      {:error, message} -> Helpers.error(frame, message)
      _ -> Helpers.error(frame, "`file_path` is required.")
    end
  end
end
