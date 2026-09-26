defmodule Exhub.BrowserAgent.Store do
  @moduledoc """
  In-memory registry of running `Exhub.BrowserAgent.Agent` sessions.

  The MCP `browser_agent` tool is stateless per request, so interactive control
  (`start`/`step`/`status`/`stop`) keeps loop state here between calls. Entries
  are keyed by a generated session id and are removed on `stop`.
  """

  use GenServer

  @type session :: %{id: String.t(), agent: Exhub.BrowserAgent.Agent.t(), updated_at: integer()}

  @doc "Starts the store."
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, %{}, Keyword.put_new(opts, :name, __MODULE__))
  end

  @doc "Generates a new session id."
  @spec new_id() :: String.t()
  def new_id, do: "ba_" <> Base.encode16(:crypto.strong_rand_bytes(6), case: :lower)

  @doc "Stores `agent` under `id`."
  @spec put(String.t(), Exhub.BrowserAgent.Agent.t()) :: :ok
  def put(id, agent), do: GenServer.call(__MODULE__, {:put, id, agent})

  @doc "Fetches a session by id."
  @spec get(String.t()) :: {:ok, session()} | {:error, :not_found}
  def get(id), do: GenServer.call(__MODULE__, {:get, id})

  @doc "Lists all sessions, most recently updated first."
  @spec list() :: [session()]
  def list, do: GenServer.call(__MODULE__, :list)

  @doc "Removes a session."
  @spec delete(String.t()) :: :ok
  def delete(id), do: GenServer.call(__MODULE__, {:delete, id})

  @impl true
  def init(state), do: {:ok, state}

  @impl true
  def handle_call({:put, id, agent}, _from, state) do
    {:reply, :ok, Map.put(state, id, %{id: id, agent: agent, updated_at: now()})}
  end

  def handle_call({:get, id}, _from, state) do
    case Map.fetch(state, id) do
      {:ok, session} -> {:reply, {:ok, session}, state}
      :error -> {:reply, {:error, :not_found}, state}
    end
  end

  def handle_call(:list, _from, state) do
    sessions =
      state
      |> Map.values()
      |> Enum.sort_by(& &1.updated_at, :desc)

    {:reply, sessions, state}
  end

  def handle_call({:delete, id}, _from, state) do
    {:reply, :ok, Map.delete(state, id)}
  end

  defp now, do: System.monotonic_time(:millisecond)
end
