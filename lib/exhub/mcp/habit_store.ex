defmodule Exhub.MCP.HabitStore do
  @moduledoc """
  Store for user habit configuration and environment information.

  This module manages a key-value store for user habits with support for:
  - Arbitrary key-value pairs for habit configuration
  - Metadata tracking which keys are modifiable by the model
  - Persistent storage via ETS
  """

  use GenServer

  @table :habit_store
  @data_dir Path.join([System.user_home(), ".config", "exhub"])
  @data_file "habits.json"

  # Default configuration schema - keys that are protected by default
  @default_protected_keys [
    "user_id",
    "email",
    "api_key",
    "password",
    "secret",
    "token"
  ]

  # Client API

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc """
  Get all habit configurations.
  """
  def get_all(server \\ __MODULE__) do
    GenServer.call(server, :get_all)
  end

  @doc """
  Get a specific habit value by key.
  """
  def get(server \\ __MODULE__, key) do
    GenServer.call(server, {:get, key})
  end

  @doc """
  Set a habit value. Returns error if the key is not modifiable.
  """
  def set(server \\ __MODULE__, key, value) do
    GenServer.call(server, {:set, key, value})
  end

  @doc """
  Set a habit value with metadata. Only for initialization or admin use.
  """
  def set_with_metadata(server \\ __MODULE__, key, value, metadata) do
    GenServer.call(server, {:set_with_metadata, key, value, metadata})
  end

  @doc """
  Check if a key is modifiable by the model.
  """
  def modifiable?(server \\ __MODULE__, key) do
    GenServer.call(server, {:modifiable?, key})
  end

  @doc """
  Delete a key. Returns error if the key is not modifiable.
  """
  def delete(server \\ __MODULE__, key) do
    GenServer.call(server, {:delete, key})
  end

  @doc """
  Get metadata for a key.
  """
  def get_metadata(server \\ __MODULE__, key) do
    GenServer.call(server, {:get_metadata, key})
  end

  @doc """
  Initialize default habits.
  """
  def init_defaults(server \\ __MODULE__, defaults) do
    GenServer.call(server, {:init_defaults, defaults})
  end

  @doc """
  Wait for the habit store to finish loading from disk.
  Returns :ok when loaded, {:error, :timeout} if it takes too long.
  """
  def await_loaded(server \\ __MODULE__, timeout_ms \\ 5000) do
    GenServer.call(server, :await_loaded, timeout_ms)
  end

  # Server Callbacks

  @impl true
  def init(_opts) do
    table = :ets.new(@table, [:set, :protected, :named_table, read_concurrency: true])

    data_path = data_file_path()
    
    state = %{
      table: table,
      data_path: data_path,
      dirty: false,
      timer: nil,
      loaded: false,
      last_persist_at: nil,
      waiting_calls: []
    }

    state =
      case load_from_file(state.data_path) do
        {:ok, habits} ->
          Enum.each(habits, fn %{"key" => key, "value" => value} = habit ->
            metadata = %{
              modifiable: Map.get(habit, "modifiable", true),
              created_at: parse_datetime(Map.get(habit, "created_at")),
              updated_at: parse_datetime(Map.get(habit, "updated_at")),
              description: Map.get(habit, "description", ""),
              category: Map.get(habit, "category", "general")
            }

            :ets.insert(table, {key, value, metadata})
          end)

          require Logger
          Logger.debug("HabitStore: Loaded #{length(habits)} habits from #{data_path}")
          %{state | loaded: true}

        {:error, :file_not_found} ->
          require Logger
          Logger.debug("HabitStore: No existing habits file found at #{data_path}")
          %{state | loaded: true}

        {:error, reason} ->
          require Logger
          Logger.warning("HabitStore: Failed to load habits: #{inspect(reason)}")
          %{state | loaded: true}
      end

    timer = schedule_persist(5000)

    {:ok, %{state | timer: timer}}
  end

  @impl true
  def handle_call(:get_all, _from, state) do
    entries =
      :ets.tab2list(state.table)
      |> Enum.map(fn {key, value, metadata} ->
        %{
          "key" => key,
          "value" => value,
          "modifiable" => Map.get(metadata, :modifiable, false),
          "description" => Map.get(metadata, :description, ""),
          "category" => Map.get(metadata, :category, "general")
        }
      end)

    {:reply, {:ok, entries}, state}
  end

  @impl true
  def handle_call({:get, key}, _from, state) do
    case :ets.lookup(state.table, key) do
      [{^key, value, metadata}] ->
        result = %{
          "key" => key,
          "value" => value,
          "modifiable" => Map.get(metadata, :modifiable, false),
          "description" => Map.get(metadata, :description, ""),
          "category" => Map.get(metadata, :category, "general")
        }

        {:reply, {:ok, result}, state}

      [] ->
        {:reply, {:error, :not_found}, state}
    end
  end

  @impl true
  def handle_call({:set, key, value}, _from, state) do
    result =
      case :ets.lookup(state.table, key) do
        [{^key, _old_value, metadata}] ->
          if Map.get(metadata, :modifiable, false) do
            new_metadata = Map.put(metadata, :updated_at, DateTime.utc_now())
            :ets.insert(state.table, {key, value, new_metadata})
            :ok
          else
            {:error, :not_modifiable}
          end

        [] ->
          metadata = %{
            modifiable: true,
            created_at: DateTime.utc_now(),
            updated_at: DateTime.utc_now(),
            description: "",
            category: "general"
          }

          :ets.insert(state.table, {key, value, metadata})
          :ok
      end

    if result == :ok do
      persist_to_file_async(state.table, state.data_path)
      {:reply, result, %{state | dirty: true}}
    else
      {:reply, result, state}
    end
  end

  @impl true
  def handle_call({:set_with_metadata, key, value, metadata}, _from, state) do
    default_metadata = %{
      modifiable: true,
      created_at: DateTime.utc_now(),
      updated_at: DateTime.utc_now(),
      description: "",
      category: "general"
    }

    final_metadata = Map.merge(default_metadata, metadata)
    :ets.insert(state.table, {key, value, final_metadata})
    persist_to_file_async(state.table, state.data_path)
    {:reply, :ok, %{state | dirty: true}}
  end

  @impl true
  def handle_call({:modifiable?, key}, _from, state) do
    case :ets.lookup(state.table, key) do
      [{^key, _value, metadata}] ->
        {:reply, Map.get(metadata, :modifiable, false), state}

      [] ->
        {:reply, true, state}
    end
  end

  @impl true
  def handle_call({:delete, key}, _from, state) do
    result =
      case :ets.lookup(state.table, key) do
        [{^key, _value, metadata}] ->
          if Map.get(metadata, :modifiable, false) do
            :ets.delete(state.table, key)
            :ok
          else
            {:error, :not_modifiable}
          end

        [] ->
          {:error, :not_found}
      end

    if result == :ok do
      persist_to_file_async(state.table, state.data_path)
      {:reply, result, %{state | dirty: true}}
    else
      {:reply, result, state}
    end
  end

  @impl true
  def handle_call({:get_metadata, key}, _from, state) do
    case :ets.lookup(state.table, key) do
      [{^key, _value, metadata}] ->
        {:reply, {:ok, metadata}, state}

      [] ->
        {:reply, {:error, :not_found}, state}
    end
  end

  @impl true
  def handle_call({:init_defaults, defaults}, _from, state) do
    Enum.each(defaults, fn {key, config} ->
      value = Map.get(config, "value", "")
      description = Map.get(config, "description", "")
      category = Map.get(config, "category", "general")
      modifiable = Map.get(config, "modifiable", !protected_key?(key))

      metadata = %{
        modifiable: modifiable,
        created_at: DateTime.utc_now(),
        updated_at: DateTime.utc_now(),
        description: description,
        category: category
      }

      :ets.insert(state.table, {key, value, metadata})
    end)

    {:reply, :ok, state}
  end

  @impl true
  def handle_call(:await_loaded, from, state) do
    if state.loaded do
      {:reply, :ok, state}
    else
      {:noreply, %{state | waiting_calls: [from | state.waiting_calls]}}
    end
  end

  @impl true
  def handle_info(:load_from_file, state) do
    new_state =
      case load_from_file(state.data_path) do
        {:ok, habits} ->
          Enum.each(habits, fn %{"key" => key, "value" => value} = habit ->
            metadata = %{
              modifiable: Map.get(habit, "modifiable", true),
              created_at: parse_datetime(Map.get(habit, "created_at")),
              updated_at: parse_datetime(Map.get(habit, "updated_at")),
              description: Map.get(habit, "description", ""),
              category: Map.get(habit, "category", "general")
            }

            :ets.insert(state.table, {key, value, metadata})
          end)

          require Logger
          Logger.debug("HabitStore: Loaded #{length(habits)} habits from #{state.data_path}")
          state

        {:error, :file_not_found} ->
          require Logger
          Logger.debug("HabitStore: No existing habits file found at #{state.data_path}")
          state

        {:error, reason} ->
          require Logger
          Logger.warning("HabitStore: Failed to load habits: #{inspect(reason)}")
          state
      end

    loaded_state = %{new_state | loaded: true}

    Enum.each(loaded_state.waiting_calls, fn from ->
      GenServer.reply(from, :ok)
    end)

    {:noreply, %{loaded_state | waiting_calls: []}}
  end

  @impl true
  def handle_info(:persist, %{dirty: false} = state) do
    timer = schedule_persist()
    {:noreply, %{state | timer: timer}}
  end

  @impl true
  def handle_info(:persist, state) do
    persist_to_file_async(state.table, state.data_path)
    timer = schedule_persist()
    {:noreply, %{state | dirty: false, timer: timer, last_persist_at: DateTime.utc_now()}}
  end

  @impl true
  def terminate(_reason, state) do
    if state.dirty do
      persist_to_file(state.table, state.data_path)
    end

    if state.timer, do: Process.cancel_timer(state.timer)
    :ok
  end

  defp schedule_persist(delay \\ 60000) do
    Process.send_after(self(), :persist, delay)
  end

  defp data_file_path do
    Path.join(@data_dir, @data_file)
  end

  defp load_from_file(path) do
    case File.read(path) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, habits} when is_list(habits) -> {:ok, habits}
          _ -> {:error, :invalid_format}
        end

      {:error, :enoent} ->
        {:error, :file_not_found}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp persist_to_file(table, path) do
    entries =
      :ets.tab2list(table)
      |> Enum.map(fn {key, value, metadata} ->
        %{
          "key" => key,
          "value" => value,
          "modifiable" => Map.get(metadata, :modifiable, true),
          "created_at" => format_datetime(Map.get(metadata, :created_at)),
          "updated_at" => format_datetime(Map.get(metadata, :updated_at)),
          "description" => Map.get(metadata, :description, ""),
          "category" => Map.get(metadata, :category, "general")
        }
      end)

    File.mkdir_p!(Path.dirname(path))

    tmp_path = path <> ".tmp"
    File.write!(tmp_path, Jason.encode!(entries))
    File.rename!(tmp_path, path)

    :ok
  rescue
    e ->
      require Logger
      Logger.error("Failed to persist habits: #{inspect(e)}")
      {:error, :persist_failed}
  end

  defp persist_to_file_async(table, path) do
    Task.start(fn ->
      persist_to_file(table, path)
    end)
  end

  defp format_datetime(%DateTime{} = dt), do: DateTime.to_iso8601(dt)
  defp format_datetime(_), do: nil

  defp parse_datetime(nil), do: DateTime.utc_now()

  defp parse_datetime(iso_string) when is_binary(iso_string) do
    case DateTime.from_iso8601(iso_string) do
      {:ok, dt, _} -> dt
      _ -> DateTime.utc_now()
    end
  end

  defp parse_datetime(_), do: DateTime.utc_now()

  defp protected_key?(key) do
    key_lower = String.downcase(key)

    Enum.any?(@default_protected_keys, fn protected ->
      String.contains?(key_lower, protected)
    end)
  end
end
