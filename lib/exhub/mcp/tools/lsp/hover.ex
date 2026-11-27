defmodule Exhub.MCP.Tools.Lsp.Hover do
  @moduledoc "MCP Tool: `lsp_hover` — type signature and docs for a position."

  alias Exhub.LspBridge.Headless
  alias Exhub.MCP.Tools.Lsp.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "lsp_hover"

  @impl true
  def description do
    """
    The compiler's own view of the symbol at a position: inferred type,
    signature, and doc comment, for a file in any project on this machine.

    Positions are 1-based (`1:1` is the first character). The file is re-read
    from disk and resolved against its own project root.

    Use it when a type is not obvious from the source (generics, inference,
    macros, trait resolution) — it is far cheaper than reading the whole
    definition chain.
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
      case Headless.request(
             file_path,
             "textDocument/hover",
             %{"position" => position},
             Helpers.opts(params)
           ) do
        {:ok, result} ->
          case Headless.hovers(result) do
            [] ->
              Helpers.ok(
                frame,
                Helpers.append_failures("No hover information at this position.", result)
              )

            hovers ->
              text =
                hovers
                |> Enum.map(fn {server, content} -> "[#{server}]\n#{content}" end)
                |> Enum.join("\n\n")

              Helpers.ok(frame, Helpers.append_failures(text, result))
          end

        {:error, reason} ->
          Helpers.error(frame, "lsp_hover failed: #{inspect(reason)}")
      end
    else
      {:error, message} -> Helpers.error(frame, message)
      _ -> Helpers.error(frame, "`file_path` is required.")
    end
  end
end
