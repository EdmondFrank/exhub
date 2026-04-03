defmodule Exhub.MCP.Tools.BrowserUse.Interact do
  @moduledoc """
  MCP Tool for interacting with page elements.

  Wraps kuri-agent commands: click, type, fill, select, hover, focus, scroll.
  Requires a prior `browser_inspect` snap to populate @eN element refs.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.BrowserUse.Helper

  use Anubis.Server.Component, type: :tool

  def name, do: "browser_interact"

  @impl true
  def description do
    """
    Interact with elements on the current page using @eN refs from a prior `browser_inspect` snap.

    Commands:
    - `click`  — Click an element by its ref (e.g. e3 or @e3).
    - `type`   — Type text into an element.
    - `fill`   — Fill a form field with a value (clears first).
    - `select` — Select an option in a <select> element.
    - `hover`  — Hover the mouse over an element.
    - `focus`  — Focus an element.
    - `scroll` — Scroll the page.

    Always run `browser_inspect` with command `snap` before using this tool to get valid element refs.
    """
  end

  schema do
    field(:command, {:required, :string},
      description: "One of: click, type, fill, select, hover, focus, scroll"
    )

    field(:ref, :string,
      description:
        "Element ref from a prior snap (e.g. \"e3\" or \"@e3\"). Required for all commands except `scroll`."
    )

    field(:value, :string,
      description:
        "Text or value to input. Required for `type`, `fill`, and `select` commands."
    )
  end

  @impl true
  def execute(params, frame) do
    command = Map.get(params, :command)
    ref = Map.get(params, :ref)
    value = Map.get(params, :value)

    case command do
      "scroll" ->
        Helper.run(["scroll"], frame)

      cmd when cmd in ["click", "hover", "focus"] ->
        if is_nil(ref) or ref == "" do
          resp =
            Response.tool()
            |> Response.error("`ref` is required for the `#{cmd}` command")

          {:reply, resp, frame}
        else
          Helper.run([cmd, ref], frame)
        end

      cmd when cmd in ["type", "fill", "select"] ->
        cond do
          is_nil(ref) or ref == "" ->
            resp =
              Response.tool()
              |> Response.error("`ref` is required for the `#{cmd}` command")

            {:reply, resp, frame}

          is_nil(value) or value == "" ->
            resp =
              Response.tool()
              |> Response.error("`value` is required for the `#{cmd}` command")

            {:reply, resp, frame}

          true ->
            Helper.run([cmd, ref, value], frame)
        end

      _ ->
        resp =
          Response.tool()
          |> Response.error(
            "Unknown command: #{inspect(command)}. Valid: click, type, fill, select, hover, focus, scroll"
          )

        {:reply, resp, frame}
    end
  end
end
