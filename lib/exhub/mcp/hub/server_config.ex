defmodule Exhub.MCP.Hub.ServerConfig do
  @moduledoc """
  Server configuration struct for MCP Hub upstream servers.

  Defines the configuration structure for connecting to upstream MCP servers
  via various transports (stdio, sse, streamable_http, websocket).

  ## Configuration Format

  ### stdio transport
      %{
        "name" => "desktop-commander",
        "transport" => "stdio",
        "command" => "npx",
        "args" => ["-y", "@wonderwhy/desktop-commander"],
        "env" => %{},
        "enabled" => true,
        "expose_route" => "desktop-commander"
      }

  ### SSE transport
      %{
        "name" => "fetch-server",
        "transport" => "sse",
        "url" => "http://localhost:3001/sse",
        "headers" => %{},
        "enabled" => true
      }

  ### Streamable HTTP transport
      %{
        "name" => "remote-mcp",
        "transport" => "streamable_http",
        "url" => "http://api.example.com/mcp",
        "headers" => %{"Authorization" => "Bearer token"},
        "enabled" => false
      }
  """

  @type transport_type :: :stdio | :sse | :streamable_http | :websocket

  @type t :: %__MODULE__{
    name: String.t(),
    transport: transport_type(),
    enabled: boolean(),
    command: String.t() | nil,
    args: [String.t()] | nil,
    env: map() | nil,
    url: String.t() | nil,
    headers: map() | nil,
    expose_route: String.t() | nil,
    created_at: DateTime.t(),
    updated_at: DateTime.t()
  }

  defstruct [
    :name, :transport, :enabled,
    :command, :args, :env,
    :url, :headers, :expose_route,
    :created_at, :updated_at
  ]

  @doc """
  Creates a ServerConfig struct from JSON/map data.
  """
  @spec from_json(map() | keyword()) :: t()
  def from_json(%{} = data) do
    transport = parse_transport(data["transport"] || data[:transport])

    struct(__MODULE__,
      name: data["name"] || data[:name],
      transport: transport,
      enabled: parse_boolean(data["enabled"]) || parse_boolean(data[:enabled]) || true,
      command: data["command"] || data[:command],
      args: data["args"] || data[:args] || [],
      env: data["env"] || data[:env] || %{},
      url: data["url"] || data[:url],
      headers: data["headers"] || data[:headers] || %{},
      expose_route: data["expose_route"] || data[:expose_route],
      created_at: parse_datetime(data["created_at"]),
      updated_at: parse_datetime(data["updated_at"])
    )
  end

  @doc """
  Converts a ServerConfig to Anubis.Client child_spec options.
  """
  @spec to_anubis_client_opts(t()) :: keyword()
  def to_anubis_client_opts(%__MODULE__{transport: :stdio} = config) do
    opts = [command: config.command, args: config.args || []]
    opts = if config.env, do: Keyword.put(opts, :env, config.env), else: opts

    [
      name: client_name(config.name),
      transport_name: transport_name(config.name),
      transport: {:stdio, opts},
      client_info: %{"name" => "exhub-mcp-hub", "version" => "1.0.0"},
      capabilities: %{}
    ]
  end

  def to_anubis_client_opts(%__MODULE__{transport: :sse} = config) do
    [
      name: client_name(config.name),
      transport_name: transport_name(config.name),
      transport: {:sse, server: [base_url: config.url]},
      protocol_version: "2024-11-05",
      client_info: %{"name" => "exhub-mcp-hub", "version" => "1.0.0"},
      capabilities: %{}
    ]
  end

  def to_anubis_client_opts(%__MODULE__{transport: :streamable_http} = config) do
    [
      name: client_name(config.name),
      transport_name: transport_name(config.name),
      transport: {:streamable_http, base_url: config.url},
      client_info: %{"name" => "exhub-mcp-hub", "version" => "1.0.0"},
      capabilities: %{}
    ]
  end

  def to_anubis_client_opts(%__MODULE__{transport: :websocket} = config) do
    [
      name: client_name(config.name),
      transport_name: transport_name(config.name),
      transport: {:websocket, url: config.url},
      client_info: %{"name" => "exhub-mcp-hub", "version" => "1.0.0"},
      capabilities: %{}
    ]
  end

  @doc """
  Converts a ServerConfig to a JSON-serializable map.
  """
  @spec to_json(t()) :: map()
  def to_json(%__MODULE__{} = config) do
    %{
      "name" => config.name,
      "transport" => to_string(config.transport),
      "enabled" => config.enabled,
      "command" => config.command,
      "args" => config.args,
      "env" => config.env,
      "url" => config.url,
      "headers" => config.headers,
      "expose_route" => config.expose_route,
      "created_at" => DateTime.to_iso8601(config.created_at),
      "updated_at" => DateTime.to_iso8601(config.updated_at)
    }
  end

  @doc """
  Checks if the server has a virtual route exposed.
  """
  @spec exposed?(t()) :: boolean()
  def exposed?(%__MODULE__{expose_route: route}) when is_binary(route) and route != "", do: true
  def exposed?(_), do: false

  @doc """
  Validates a server configuration.
  """
  @spec validate(t()) :: :ok | {:error, atom()}
  def validate(%__MODULE__{name: nil}), do: {:error, :name_required}
  def validate(%__MODULE__{name: ""}), do: {:error, :name_required}
  def validate(%__MODULE__{transport: :stdio, command: nil}), do: {:error, :command_required_for_stdio}
  def validate(%__MODULE__{transport: :stdio, command: ""}), do: {:error, :command_required_for_stdio}
  def validate(%__MODULE__{transport: transport, url: nil})
      when transport in [:sse, :streamable_http, :websocket] do
    {:error, :url_required}
  end
  def validate(%__MODULE__{transport: transport, url: ""})
      when transport in [:sse, :streamable_http, :websocket] do
    {:error, :url_required}
  end
  def validate(_), do: :ok

  defp parse_transport("stdio"), do: :stdio
  defp parse_transport("sse"), do: :sse
  defp parse_transport("http"), do: :streamable_http
  defp parse_transport("streamable_http"), do: :streamable_http
  defp parse_transport("websocket"), do: :websocket
  defp parse_transport(other) when is_atom(other), do: other
  defp parse_transport(_), do: :stdio

  defp parse_boolean(nil), do: nil
  defp parse_boolean(true), do: true
  defp parse_boolean(false), do: false
  defp parse_boolean("true"), do: true
  defp parse_boolean("false"), do: false
  defp parse_boolean(_), do: nil

  defp parse_datetime(nil), do: DateTime.utc_now()
  defp parse_datetime(%DateTime{} = dt), do: dt
  defp parse_datetime(str) when is_binary(str) do
    case DateTime.from_iso8601(str) do
      {:ok, dt, _} -> dt
      _ -> DateTime.utc_now()
    end
  end

  @doc """
  Returns the Registry via-tuple name for a client process.
  """
  @spec client_name(String.t()) :: {:via, Registry, term()}
  def client_name(name), do: {:via, Registry, {Exhub.Registry, {:mcp_hub_client, name}}}

  defp transport_name(name), do: {:via, Registry, {Exhub.Registry, {:mcp_hub_transport, name}}}
end
