defmodule Exhub.Agent.Registry do
  @moduledoc """
  Persistent participant records for agents that talk over the agent bus.

  An agent in the bus sense is an *identity*, not a process: a name, a
  description, a capability list, a status, and a home directory. The identity
  outlives any ACP session — a restarted subagent keeps its inbox, its memory
  file and its room memberships.

  Records live at `~/.exhub/agents/<id>/agent.json` (override the root with the
  `:agent_home` application env, or the `home:` start option). Reads go through
  a named ETS table; writes are serialised by the GenServer so two concurrent
  registrations cannot clobber each other.
  """

  use GenServer

  require Logger

  @table :exhub_agent_registry
  @default_home "~/.exhub/agents"

  @statuses ~w(starting idle working blocked waiting_input done error offline)
  @detail_limit 200
  @name_regex ~r/^[a-z0-9][a-z0-9._-]{0,63}$/

  @memory_seed """
  # Agent memory

  This file is yours: rewrite it when something you learned should survive a
  restart. Keep it short — it is prepended to your context, not an archive.

  ## Bus etiquette

  - `agent_send` with kind `ask` when you need an answer; `tell` when you don't.
  - Answer with kind `reply` and set `reply_to` to the message id you answered.
  - Before handing work over, say what changed, where it is, what proves it,
    what is still uncertain, and what the next owner should do.
  - `agent_await` instead of polling `agent_list` in a loop.
  """

  # ---------------------------------------------------------------------------
  # Client API
  # ---------------------------------------------------------------------------

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc """
  Creates or updates a participant record.

  Only the provided keys are touched, so `register(%{id: "x", status: "working"})`
  is a valid partial update. `created_at` is preserved across updates.
  """
  @spec register(map(), keyword()) :: {:ok, map()} | {:error, term()}
  def register(attrs, opts \\ []) when is_map(attrs) do
    call(opts, {:register, attrs})
  end

  @spec get(String.t(), keyword()) :: {:ok, map()} | {:error, :not_found}
  def get(id, opts \\ []) when is_binary(id) do
    case :ets.lookup(table(opts), id) do
      [{^id, record}] -> {:ok, record}
      [] -> {:error, :not_found}
    end
  rescue
    ArgumentError -> {:error, :not_found}
  end

  @spec list(keyword()) :: [map()]
  def list(opts \\ []) do
    table(opts)
    |> :ets.tab2list()
    |> Enum.map(fn {_id, record} -> record end)
    |> Enum.sort_by(& &1.id)
  rescue
    ArgumentError -> []
  end

  @spec delete(String.t(), keyword()) :: :ok | {:error, :not_found}
  def delete(id, opts \\ []) when is_binary(id) do
    call(opts, {:delete, id})
  end

  @doc "Sets `status` (validated) and an optional human-readable `detail`."
  @spec set_status(String.t(), String.t(), String.t() | nil, keyword()) ::
          {:ok, map()} | {:error, term()}
  def set_status(id, status, detail \\ nil, opts \\ []) do
    register(%{id: id, status: status, detail: detail}, opts)
  end

  @doc "Path of an agent's private home directory."
  @spec home(String.t(), keyword()) :: String.t()
  def home(id, opts \\ []) do
    Path.join(server_home(opts), id)
  end

  @spec memory_path(String.t(), keyword()) :: String.t()
  def memory_path(id, opts \\ []) do
    Path.join(home(id, opts), "MEMORY.md")
  end

  @spec statuses() :: [String.t()]
  def statuses, do: @statuses

  @doc "True when `status` is part of the closed vocabulary."
  def valid_status?(status), do: status in @statuses

  def valid_name?(name) when is_binary(name), do: Regex.match?(@name_regex, name)
  def valid_name?(_), do: false

  # ---------------------------------------------------------------------------
  # GenServer callbacks
  # ---------------------------------------------------------------------------

  @impl true
  def init(opts) do
    table = Keyword.get(opts, :table, @table)
    home = home_root(opts)

    :ets.new(table, [
      :named_table,
      :set,
      :public,
      read_concurrency: true,
      write_concurrency: true
    ])

    File.mkdir_p!(home)

    records =
      home
      |> Path.join("*")
      |> Path.join("agent.json")
      |> Path.wildcard()
      |> Enum.flat_map(&load_record/1)

    Enum.each(records, fn record -> :ets.insert(table, {record.id, record}) end)

    {:ok, %{table: table, home: home}}
  end

  @impl true
  def handle_call({:register, attrs}, _from, state) do
    case build_record(attrs, state) do
      {:ok, record} ->
        :ets.insert(state.table, {record.id, record})
        persist(record, state)
        {:reply, {:ok, record}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:delete, id}, _from, state) do
    case :ets.lookup(state.table, id) do
      [{^id, _}] ->
        :ets.delete(state.table, id)
        {:reply, :ok, state}

      [] ->
        {:reply, {:error, :not_found}, state}
    end
  end

  def handle_call(:home, _from, state) do
    {:reply, state.home, state}
  end

  # ---------------------------------------------------------------------------
  # Internals
  # ---------------------------------------------------------------------------

  defp call(opts, message) do
    GenServer.call(server(opts), message, 15_000)
  end

  defp server(opts), do: Keyword.get(opts, :server, __MODULE__)
  defp table(opts), do: Keyword.get(opts, :table, @table)

  defp home_root(opts) do
    opts
    |> Keyword.get(:home)
    |> case do
      nil -> Application.get_env(:exhub, :agent_home, @default_home)
      home -> home
    end
    |> Path.expand()
  end

  # The configured root is only the *start* option; the running server is the
  # source of truth once it exists (tests and per-instance homes rely on this).
  defp server_home(opts) do
    case Keyword.get(opts, :home) do
      nil -> GenServer.call(server(opts), :home)
      home -> Path.expand(home)
    end
  rescue
    _ -> home_root(opts)
  catch
    :exit, _ -> home_root(opts)
  end

  defp build_record(attrs, state) do
    attrs = normalize_keys(attrs)
    id = attrs["id"]
    existing = :ets.lookup(state.table, id) |> Enum.map(&elem(&1, 1)) |> List.first()
    now = DateTime.utc_now() |> DateTime.to_iso8601()

    cond do
      not is_binary(id) or id == "" ->
        {:error, :missing_id}

      not valid_name?(id) ->
        {:error, :invalid_id}

      true ->
        with {:ok, status} <- resolve_status(attrs, existing),
             {:ok, detail} <- resolve_detail(attrs, existing),
             {:ok, name} <- resolve_name(attrs, id, existing),
             {:ok, capabilities} <- resolve_capabilities(attrs, existing) do
          {:ok,
           %{
             id: id,
             name: name,
             description: attrs["description"] || existing_field(existing, :description, ""),
             capabilities: capabilities,
             kind: attrs["kind"] || existing_field(existing, :kind, "acp"),
             status: status,
             detail: detail,
             home: Path.join(state.home, id),
             created_at: existing_field(existing, :created_at, now),
             updated_at: now,
             last_seen_at: now
           }}
        end
    end
  end

  defp resolve_status(attrs, existing) do
    case attrs["status"] do
      nil -> {:ok, existing_field(existing, :status, "idle")}
      status when status in @statuses -> {:ok, status}
      other -> {:error, {:invalid_status, other}}
    end
  end

  defp resolve_detail(attrs, existing) do
    case attrs["detail"] do
      nil ->
        {:ok, existing_field(existing, :detail, nil)}

      detail when is_binary(detail) ->
        detail = String.slice(detail, 0, @detail_limit)

        if valid_utf8?(detail) do
          {:ok, detail}
        else
          {:error, :invalid_detail}
        end

      _ ->
        {:error, :invalid_detail}
    end
  end

  defp resolve_name(attrs, id, existing) do
    case attrs["name"] || existing_field(existing, :name, nil) do
      nil -> {:ok, id}
      name when is_binary(name) -> {:ok, String.slice(name, 0, 100)}
      _ -> {:error, :invalid_name}
    end
  end

  defp resolve_capabilities(attrs, existing) do
    case attrs["capabilities"] || existing_field(existing, :capabilities, []) do
      list when is_list(list) ->
        if Enum.all?(list, &is_binary/1) do
          {:ok, list}
        else
          {:error, :invalid_capabilities}
        end

      _ ->
        {:error, :invalid_capabilities}
    end
  end

  defp existing_field(nil, _key, default), do: default
  defp existing_field(existing, key, default), do: Map.get(existing, key, default)

  defp normalize_keys(attrs) do
    Map.new(attrs, fn {k, v} -> {to_string(k), v} end)
  end

  defp persist(record, state) do
    dir = Path.join(state.home, record.id)
    File.mkdir_p!(dir)

    File.write!(Path.join(dir, "agent.json"), Jason.encode!(record, pretty: true))

    memory = Path.join(dir, "MEMORY.md")

    unless File.exists?(memory) do
      File.write!(memory, @memory_seed)
    end

    :ok
  rescue
    error ->
      Logger.warning("[Agent.Registry] could not persist #{record.id}: #{inspect(error)}")
      :ok
  end

  defp load_record(path) do
    with {:ok, content} <- File.read(path),
         {:ok, raw} <- Jason.decode(content) do
      [
        %{
          id: raw["id"],
          name: raw["name"] || raw["id"],
          description: raw["description"] || "",
          capabilities: raw["capabilities"] || [],
          kind: raw["kind"] || "acp",
          status: raw["status"] || "idle",
          detail: raw["detail"],
          home: Path.dirname(path),
          created_at: raw["created_at"],
          updated_at: raw["updated_at"],
          last_seen_at: raw["last_seen_at"]
        }
      ]
    else
      _ -> []
    end
  end

  defp valid_utf8?(string), do: String.valid?(string)
end
