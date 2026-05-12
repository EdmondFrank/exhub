defmodule Exhub.MCP.Hub.Store do
  @moduledoc """
  GenServer that owns and manages ETS tables for the MCP Hub.

  Tables:
  - `:mcp_hub_search_index` - Stores the tool search index built from upstream servers.
  - `:mcp_hub_proxy_sessions` - Stores proxy session data for the ProxyPlug.
  """

  use GenServer
  require Logger

  @search_index_table :mcp_hub_search_index
  @session_table :mcp_hub_proxy_sessions

  # Client API

  @doc """
  Starts the Store GenServer.
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Inserts a search index into the ETS table.
  """
  @spec put_search_index(term()) :: true
  def put_search_index(index) do
    :ets.insert(@search_index_table, {:index, index})
  end

  @doc """
  Looks up the current search index from the ETS table.
  """
  @spec get_search_index() :: [term()]
  def get_search_index do
    :ets.lookup(@search_index_table, :index)
  end

  @doc """
  Stores a proxy session in the ETS table.
  """
  @spec store_session(binary(), map()) :: true
  def store_session(session_id, data) when is_binary(session_id) do
    :ets.insert(@session_table, {session_id, data})
  end

  @doc """
  Looks up a proxy session by session_id.
  """
  @spec get_session(binary()) :: [term()]
  def get_session(session_id) when is_binary(session_id) do
    :ets.lookup(@session_table, session_id)
  end

  @doc """
  Deletes a proxy session by session_id.
  """
  @spec delete_session(binary()) :: true
  def delete_session(session_id) when is_binary(session_id) do
    :ets.delete(@session_table, session_id)
  end

  # Server Callbacks

  @impl true
  def init(_opts) do
    # Create search index table
    case :ets.new(@search_index_table, [:named_table, :public, :set, read_concurrency: true]) do
      @search_index_table ->
        Logger.debug("[MCP Hub Store] Created ETS table #{@search_index_table}")

      other ->
        Logger.warning("[MCP Hub Store] Unexpected ETS table creation result: #{inspect(other)}")
    end

    # Create proxy sessions table
    case :ets.new(@session_table, [:named_table, :public, :set, read_concurrency: true]) do
      @session_table ->
        Logger.debug("[MCP Hub Store] Created ETS table #{@session_table}")

      other ->
        Logger.warning("[MCP Hub Store] Unexpected ETS table creation result: #{inspect(other)}")
    end

    {:ok, %{}}
  end
end
