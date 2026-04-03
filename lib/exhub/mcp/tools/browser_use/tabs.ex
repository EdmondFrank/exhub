defmodule Exhub.MCP.Tools.BrowserUse.Tabs do
  @moduledoc """
  MCP Tool for Chrome tab discovery and session management.

  Wraps kuri-agent commands: tabs, use, status.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.BrowserUse.Helper

  use Anubis.Server.Component, type: :tool

  def name, do: "browser_tabs"

  @impl true
  def description do
    """
    Manage Chrome tabs and session attachment for browser automation.

    Commands:
    - `tabs`   — List all open Chrome tabs. Returns JSON array with id, url, and ws (WebSocket URL) for each tab.
    - `use`    — Attach to a specific Chrome tab by its WebSocket URL and save the session to ~/.kuri/session.json.
    - `status` — Show the currently attached tab session.

    Always call `tabs` first to discover available tabs, then `use` to attach before running any other browser commands.
    """
  end

  schema do
    field(:command, {:required, :string},
      description: "One of: tabs, use, status"
    )

    field(:ws_url, :string,
      description: "WebSocket URL of the tab to attach to (required for `use` command). Example: ws://127.0.0.1:9222/devtools/page/ABC..."
    )

    field(:port, :integer,
      description: "Chrome DevTools port (optional, default 9222). Used with `tabs` command."
    )
  end

  @impl true
  def execute(params, frame) do
    command = Map.get(params, :command)

    case command do
      "tabs" ->
        argv =
          case Map.get(params, :port) do
            port when port > 0 -> ["tabs", "--port", to_string(port)]
            _ -> ["tabs"]
          end

        Helper.run(argv, frame)

      "use" ->
        ws_url = Map.get(params, :ws_url)

        if is_nil(ws_url) or ws_url == "" do
          resp = Response.tool() |> Response.error("`ws_url` is required for the `use` command")
          {:reply, resp, frame}
        else
          Helper.run(["use", ws_url], frame)
        end

      "status" ->
        Helper.run(["status"], frame)

      _ ->
        resp =
          Response.tool()
          |> Response.error("Unknown command: #{inspect(command)}. Valid: tabs, use, status")

        {:reply, resp, frame}
    end
  end
end
