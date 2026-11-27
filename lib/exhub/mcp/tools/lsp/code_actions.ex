defmodule Exhub.MCP.Tools.Lsp.CodeActions do
  @moduledoc "MCP Tool: `lsp_code_actions` — quick fixes and refactors at a range."

  alias Exhub.LspBridge.Headless
  alias Exhub.MCP.Tools.Lsp.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "lsp_code_actions"

  @impl true
  def description do
    """
    Code actions (`textDocument/codeAction`) for a range of a file in any
    project on this machine: quick fixes the compiler suggests, import
    additions, refactors the server knows about.

    Positions are 1-based. `line`/`character` start the range; `end_line`/
    `end_character` extend it (a single position when omitted). `kinds`
    filters by action kind prefix, e.g. `["quickfix"]` or
    `["refactor.rewrite"]`.

    Read-only: this returns the actions and the edits they would make; it does
    not apply anything. Nothing is written to disk.
    """
  end

  schema do
    field(:file_path, {:required, :string}, description: "Path to the file.")

    field(:line, {:required, :integer}, description: "1-based start line.")

    field(:character, {:required, :integer}, description: "1-based start column.")

    field(:end_line, :integer, description: "1-based end line (defaults to `line`).")

    field(:end_character, :integer, description: "1-based end column (defaults to `character`).")

    field(:kinds, {:list, :string},
      description: "Action kind filters, e.g. `[\"quickfix\"]`.",
      default: []
    )

    field(:server, :string, description: "Pin a language server by name.")

    field(:project_path, :string, description: "Authoritative project root override.")
  end

  @impl true
  def execute(params, frame) do
    with {:ok, position} <- Helpers.position(params),
         file_path when is_binary(file_path) <- Map.get(params, :file_path) do
      params_map = %{
        "range" => %{
          "start" => position,
          "end" => %{
            "line" => range_value(Map.get(params, :end_line), position["line"]),
            "character" => range_value(Map.get(params, :end_character), position["character"])
          }
        },
        "context" => %{"diagnostics" => [], "triggerKind" => 1}
      }

      params_map =
        case List.wrap(Map.get(params, :kinds, [])) do
          [] -> params_map
          kinds -> Map.put(params_map, "context", Map.put(params_map["context"], "only", kinds))
        end

      case Headless.request(
             file_path,
             "textDocument/codeAction",
             params_map,
             Helpers.opts(params)
           ) do
        {:ok, result} ->
          actions = collect_actions(result.results)

          text =
            case actions do
              [] ->
                "No code actions at this range."

              actions ->
                "#{length(actions)} code action(s):\n" <>
                  Enum.map_join(actions, "\n", &format_action/1)
            end

          Helpers.ok(frame, Helpers.append_failures(text, result))

        {:error, reason} ->
          Helpers.error(frame, "lsp_code_actions failed: #{inspect(reason)}")
      end
    else
      {:error, message} -> Helpers.error(frame, message)
      _ -> Helpers.error(frame, "`file_path` is required.")
    end
  end

  defp range_value(nil, default), do: default
  defp range_value(value, _default) when is_integer(value) and value >= 1, do: value - 1
  defp range_value(_value, default), do: default

  defp collect_actions(results) do
    results
    |> Enum.flat_map(fn
      {_server, {:ok, list}} when is_list(list) -> list
      _ -> []
    end)
    |> Enum.uniq_by(& &1["title"])
  end

  defp format_action(action) do
    changes = get_in(action, ["edit", "changes"]) || %{}
    edits = changes |> Enum.map(fn {_uri, edits} -> length(edits) end) |> Enum.sum()
    files = map_size(changes)
    kind = if action["kind"], do: " [#{action["kind"]}]", else: ""
    command = if action["command"], do: " (command: #{action["command"]["command"]})", else: ""

    edit_note =
      if edits > 0, do: " → #{edits} edit(s) in #{files} file(s)", else: ""

    "- #{action["title"]}#{kind}#{edit_note}#{command}"
  end
end
