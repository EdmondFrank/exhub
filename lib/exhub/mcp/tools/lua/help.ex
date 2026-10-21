defmodule Exhub.MCP.Tools.Lua.Help do
  @moduledoc """
  MCP tool: `help` (server `lua`)

  Reads tool documentation without executing code: the index of every tool the
  `code_mode` sandbox bridges, or one tool's description and parameters. The
  content comes from `Exhub.Lua.Help` over the same visible tool set as
  `code_mode` (`Exhub.MCP.Tools.Lua.VisibleTools`), and the same docs are
  available in-sandbox as the `help` global.
  """

  alias Anubis.Server.Response
  alias Exhub.Lua.Help, as: Doc
  alias Exhub.MCP.Tools.Lua.VisibleTools

  use Anubis.Server.Component, type: :tool

  def name, do: "help"

  @impl true
  def description do
    """
    Documentation for the `code_mode` sandbox's bridged tools.

    Call with no argument for the index — one line per tool. Pass `tool` as
    `"server.tool"` (or `"server__tool"`) for that tool's full description and
    parameters. The same docs are reachable in a snippet as `help()` /
    `help("server.tool")`.
    """
  end

  schema do
    field(:tool, :string,
      description:
        "Tool to document, as `server.tool` or `server__tool`. Omit for the full index.",
      required: false
    )
  end

  @impl true
  def execute(params, frame) do
    text = Doc.render(VisibleTools.list(frame), Map.get(params, :tool))

    {:reply, Response.tool() |> Response.text(text), frame}
  end
end
