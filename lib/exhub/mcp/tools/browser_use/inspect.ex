defmodule Exhub.MCP.Tools.BrowserUse.Inspect do
  @moduledoc """
  MCP Tool for page inspection.

  Wraps kuri-agent commands: snap, text, eval, shot.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.BrowserUse.Helper

  use Anubis.Server.Component, type: :tool

  def name, do: "browser_inspect"

  @impl true
  def description do
    """
    Inspect the current page content in the attached Chrome tab.

    Commands:
    - `snap`  — Take an accessibility (a11y) snapshot of the page. Returns a JSON tree with @eN element refs used by `browser_interact`. Use `interactive: true` to show only clickable elements, `text: true` for plain-text output, `depth` to limit tree depth.
    - `text`  — Get all visible text from the page, or from a specific CSS selector.
    - `eval`  — Evaluate a JavaScript expression and return the result.
    - `shot`  — Take a screenshot. Saved to ~/.kuri/screenshots/<timestamp>.png by default.

    Always run `snap` before using `browser_interact` — it saves the @eN element refs to the session.
    """
  end

  schema do
    field(:command, {:required, :string},
      description: "One of: snap, text, eval, shot"
    )

    field(:interactive, :boolean,
      description: "(`snap` only) Return only interactive elements. Default false."
    )

    field(:text_mode, :boolean,
      description: "(`snap` only) Return plain-text output instead of JSON. Default false."
    )

    field(:depth, :integer,
      description: "(`snap` only) Limit the a11y tree depth. Example: 3"
    )

    field(:selector, :string,
      description: "(`text` only) CSS selector to scope text extraction. Example: \"#main\""
    )

    field(:expression, :string,
      description: "(`eval` only) JavaScript expression to evaluate. Example: \"document.title\""
    )

    field(:out, :string,
      description: "(`shot` only) Output file path for the screenshot. Example: /tmp/shot.png"
    )
  end

  @impl true
  def execute(params, frame) do
    command = Map.get(params, :command)

    case command do
      "snap" ->
        argv =
          ["snap"]
          |> maybe_flag("--interactive", Map.get(params, :interactive))
          |> maybe_flag("--text", Map.get(params, :text_mode))
          |> maybe_opt("--depth", Map.get(params, :depth))

        Helper.run(argv, frame)

      "text" ->
        argv =
          case Map.get(params, :selector) do
            nil -> ["text"]
            sel -> ["text", sel]
          end

        Helper.run(argv, frame)

      "eval" ->
        expr = Map.get(params, :expression)

        if is_nil(expr) or expr == "" do
          resp =
            Response.tool() |> Response.error("`expression` is required for the `eval` command")

          {:reply, resp, frame}
        else
          Helper.run(["eval", expr], frame)
        end

      "shot" ->
        argv =
          case Map.get(params, :out) do
            nil -> ["shot"]
            path -> ["shot", "--out", path]
          end

        Helper.run(argv, frame)

      _ ->
        resp =
          Response.tool()
          |> Response.error("Unknown command: #{inspect(command)}. Valid: snap, text, eval, shot")

        {:reply, resp, frame}
    end
  end

  defp maybe_flag(argv, _flag, nil), do: argv
  defp maybe_flag(argv, _flag, false), do: argv
  defp maybe_flag(argv, flag, true), do: argv ++ [flag]

  defp maybe_opt(argv, _opt, nil), do: argv
  defp maybe_opt(argv, opt, value), do: argv ++ [opt, to_string(value)]
end
