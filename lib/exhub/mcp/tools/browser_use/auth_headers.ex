defmodule Exhub.MCP.Tools.BrowserUse.AuthHeaders do
  @moduledoc """
  MCP Tool for managing persistent authentication headers.

  Wraps kuri-agent commands: set-header, show-headers, clear-headers.
  Headers are persisted across commands via CDP Network.setExtraHTTPHeaders.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.BrowserUse.Helper

  use Anubis.Server.Component, type: :tool

  def name, do: "browser_auth_headers"

  @impl true
  def description do
    """
    Manage persistent HTTP headers for the current browser session.

    Headers set with `set_header` are automatically applied via CDP
    Network.setExtraHTTPHeaders on every subsequent connection, making
    them available to `browser_security` fetch and probe commands.

    Commands:
    - `set_header`    — Add or update a persistent request header (e.g. Authorization, X-Custom-Auth).
    - `show_headers`  — Print all currently stored headers.
    - `clear_headers` — Remove all stored headers.

    Example — set a Bearer token once, then all fetch/probe/go commands use it:
      set_header name="Authorization" value="Bearer eyJ..."
    """
  end

  schema do
    field(:command, {:required, :string},
      description: "One of: set_header, show_headers, clear_headers"
    )

    field(:name, :string,
      description:
        "Header name (required for `set_header`). Example: Authorization"
    )

    field(:value, :string,
      description:
        "Header value (required for `set_header`). Example: Bearer eyJ..."
    )
  end

  @impl true
  def execute(params, frame) do
    command = Map.get(params, :command)

    case command do
      "set_header" ->
        name = Map.get(params, :name)
        value = Map.get(params, :value)

        cond do
          is_nil(name) or name == "" ->
            resp =
              Response.tool()
              |> Response.error("`name` is required for the `set_header` command")

            {:reply, resp, frame}

          is_nil(value) or value == "" ->
            resp =
              Response.tool()
              |> Response.error("`value` is required for the `set_header` command")

            {:reply, resp, frame}

          true ->
            Helper.run(["set-header", name, value], frame)
        end

      "show_headers" ->
        Helper.run(["show-headers"], frame)

      "clear_headers" ->
        Helper.run(["clear-headers"], frame)

      _ ->
        resp =
          Response.tool()
          |> Response.error(
            "Unknown command: #{inspect(command)}. Valid: set_header, show_headers, clear_headers"
          )

        {:reply, resp, frame}
    end
  end
end
