defmodule Exhub.MCP.Hub.Instructions do
  @moduledoc """
  Composes the `instructions` string the MCP Hub publishes in its handshake.

  An MCP `initialize` result may carry `instructions` — free-form guidance a
  client shows the model before it calls any tool. Two things get in the way in
  the hub:

    * the hub aggregates many servers, so the text has to be composed from all
      of them rather than forwarded from one;
    * upstream instructions are dropped. `Anubis.Client` keeps only
      `serverInfo`/`capabilities` from the upstream handshake, so a server like
      Browser MCP — whose usage discipline ("act in batches", "re-snapshot after
      navigating") decides whether a browser session works or flails — cannot
      contribute it through the client API at all.

  The text therefore comes from two places:

    1. a built-in server's `server_instructions/0`, read through
       `Exhub.MCP.Hub.BuiltInRegistry`;
    2. the optional `"instructions"` field of an entry in `mcp_servers.json`
       (see `Exhub.MCP.Hub.ServerConfig`), where an upstream server's own
       guidance is copied by hand.

  `compose/0` renders the hub-wide text (preamble plus every enabled server);
  `compose/1` renders one server for a virtual route (`/chrome/mcp`). Both are
  bounded so a chatty config cannot flood every client's context.
  """

  @max_chars 8_000

  @preamble """
  ExHub MCP Hub aggregates the built-in ExHub servers (`desktop`, `brain`,
  `web-tools`, `browser-agent`, ...) and the upstream servers configured in
  `mcp_servers.json` (e.g. `chrome`, `github`, `context7`).

  - Hub tool names are `<server>__<tool>`. Use `retrieve_tools` to find a tool by
    intent (Smart Decide filters the candidates) and `call_tools` to run one by
    `server_name` plus the bare `tool_name`.
  - A virtual route such as `/chrome/mcp` exposes a single server's tools under
    their original, unprefixed names.
  - An upstream server only appears while it is connected; `/mcp-hub/servers`
    (or `exhub_get_status`) shows what is live.
  """

  @doc """
  Hub-wide instructions: the connection preamble plus one section per enabled
  server that publishes guidance.
  """
  @spec compose() :: String.t()
  def compose, do: from_servers(servers(), :hub)

  @doc """
  Instructions for one server, or `nil` when it publishes none.

  Used by the virtual-route proxy, which synthesizes the handshake itself and
  would otherwise drop the guidance a direct connection would deliver.
  """
  @spec compose(String.t()) :: String.t() | nil
  def compose(server_name) when is_binary(server_name), do: from_servers(servers(), server_name)

  @doc """
  `compose/0` and `compose/1` over an explicit server list.

  `:hub` (or `nil`) renders the preamble plus every enabled server; a server
  name renders that one section, or `nil` when it has none. Kept public so the
  composition rules can be tested without a running `ClientManager`.
  """
  @spec from_servers([map()], :hub | String.t() | nil) :: String.t() | nil
  def from_servers(servers, target \\ :hub) when is_list(servers) do
    case target do
      hub when hub in [:hub, nil] ->
        sections = sections(servers, &enabled?/1)

        Enum.join([String.trim_trailing(@preamble)] ++ sections, "\n\n")
        |> String.slice(0, @max_chars)

      name when is_binary(name) ->
        case sections(servers, &(&1.name == name)) do
          [section | _] -> String.slice(section, 0, @max_chars)
          [] -> nil
        end
    end
  end

  # --- internals ---

  # A short, defensive call: the hub publishes instructions while a session
  # initializes, and `ClientManager` is also the process that talks to upstream
  # servers, so a failure here must degrade to "no instructions", never to a
  # failed handshake.
  defp servers do
    Exhub.MCP.Hub.ClientManager.list_servers()
  rescue
    _ -> []
  catch
    _, _ -> []
  end

  defp sections(servers, filter) do
    servers
    |> Enum.filter(filter)
    |> Enum.sort_by(& &1.name)
    |> Enum.flat_map(fn server ->
      case instructions_for(server) do
        text when is_binary(text) and text != "" ->
          ["## #{server.name}\n" <> String.trim_trailing(text)]

        _ ->
          []
      end
    end)
  end

  defp enabled?(%{enabled: false}), do: false
  defp enabled?(_server), do: true

  # Config first (an operator can override anything), then the built-in's own
  # callback.
  defp instructions_for(server) do
    case Map.get(server, :instructions) do
      text when is_binary(text) and text != "" -> text
      _ -> Exhub.MCP.Hub.BuiltInRegistry.server_instructions(server.name)
    end
  end
end
