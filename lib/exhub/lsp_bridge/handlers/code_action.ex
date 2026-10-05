defmodule Exhub.LspBridge.Handlers.CodeAction do
  @moduledoc """
  `textDocument/codeAction` — port of `core/handler/code_action.py`.

  Builds the `range` + `context` (diagnostics overlapping the range, plus an
  optional `only` filter) and passes the server's `Command | CodeAction[]`
  through. The elisp front end prompts for an action and applies its edit or
  executes its command.
  """

  @behaviour Exhub.LspBridge.Handler

  @impl true
  def name, do: "code-action"

  @impl true
  def method, do: "textDocument/codeAction"

  @impl true
  def cancel_on_change?, do: true

  @impl true
  def provider, do: "code_action"

  @impl true
  def request_params(%{"range" => range} = args, ctx) do
    context = %{"diagnostics" => diagnostics_in_range(ctx, range)}

    context =
      case Map.get(args, "only") do
        kind when is_binary(kind) and kind != "" -> Map.put(context, "only", [kind])
        kinds when is_list(kinds) and kinds != [] -> Map.put(context, "only", kinds)
        _ -> context
      end

    %{"range" => range, "context" => context}
  end

  def request_params(_args, _ctx), do: %{}

  @impl true
  def process_response(actions, ctx) when is_list(actions) and actions != [] do
    {:code_actions, ctx.path, actions}
  end

  def process_response(_other, _ctx), do: {:message, "No code actions available."}

  # -- diagnostics in range --------------------------------------------------

  defp diagnostics_in_range(ctx, range) do
    ctx
    |> Map.get(:diagnostics, [])
    |> Enum.filter(&overlaps?(&1, range))
  end

  defp overlaps?(%{"range" => diagnostic_range}, range) do
    not (before?(Map.get(range, "end", %{}), Map.get(diagnostic_range, "start", %{})) or
           before?(Map.get(diagnostic_range, "end", %{}), Map.get(range, "start", %{})))
  end

  defp overlaps?(_diagnostic, _range), do: false

  defp before?(a, b), do: position_key(a) < position_key(b)

  defp position_key(%{"line" => line, "character" => character}), do: {line, character}
  defp position_key(_other), do: {0, 0}
end
