defmodule Exhub.MCP.Tools.BrowserUse.Navigate do
  @moduledoc """
  MCP Tool for browser navigation.

  Wraps kuri-agent commands: go, back, forward, reload.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.BrowserUse.Helper

  use Anubis.Server.Component, type: :tool

  def name, do: "browser_navigate"

  @impl true
  def description do
    """
    Navigate the attached Chrome tab.

    Commands:
    - `go`      — Navigate to a URL.
    - `back`    — Go back in browser history.
    - `forward` — Go forward in browser history.
    - `reload`  — Reload the current page.

    Requires an active session (use `browser_tabs` with `use` first).
    """
  end

  schema do
    field(:command, {:required, :string},
      description: "One of: go, back, forward, reload"
    )

    field(:url, :string,
      description: "URL to navigate to (required for `go` command). Example: https://example.com"
    )
  end

  @impl true
  def execute(params, frame) do
    command = Map.get(params, :command)

    case command do
      "go" ->
        url = Map.get(params, :url)

        if is_nil(url) or url == "" do
          resp = Response.tool() |> Response.error("`url` is required for the `go` command")
          {:reply, resp, frame}
        else
          Helper.run(["go", url], frame)
        end

      cmd when cmd in ["back", "forward", "reload"] ->
        Helper.run([cmd], frame)

      _ ->
        resp =
          Response.tool()
          |> Response.error(
            "Unknown command: #{inspect(command)}. Valid: go, back, forward, reload"
          )

        {:reply, resp, frame}
    end
  end
end
