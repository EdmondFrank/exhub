defmodule Exhub.Agent.Bus do
  @moduledoc """
  Durable agent-to-agent message streams.

  Two kinds of stream:

    * `agent:<id>` — direct messages addressed to one participant
    * `room:<name>` — a flat shared log (no ACLs; every member reads and posts)

  Every message is appended to `<home>/<stream>.jsonl` before the caller is
  acknowledged, so a restart does not lose traffic. `seq` is per-stream and
  monotonic; read positions are per `(agent, stream)`, server-side, and
  persisted. Delivery is at-least-once: a crash between the append and the
  cursor write re-delivers rather than drops.

  ## Message shape

      %{
        id: "m_1a2b3c4d5e",
        stream: "agent:reviewer",
        seq: 7,
        from: "coder",
        to: "agent:reviewer",
        kind: "ask" | "tell" | "reply" | "handoff",
        body: "...",
        reply_to: nil | "m_...",
        idempotency_key: nil | "...",
        inserted_at: "2026-11-02T10:00:00Z"
      }
  """

  use GenServer

  require Logger

  @table :exhub_agent_bus
  @default_home "~/.exhub/bus"

  @kinds ~w(ask tell reply handoff)
  @name_regex ~r/^[a-z0-9][a-z0-9._-]{0,63}$/
  @max_body 32_768
  @max_messages 500
  @max_wait_ms 120_000
  @max_limit 200

  # ---------------------------------------------------------------------------
  # Client API
  # ---------------------------------------------------------------------------

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc """
  Sends a direct message to `to` (an agent id, with or without a leading `@`).
  """
  @spec send_message(map(), keyword()) ::
          {:ok, map(), :created | :replayed} | {:error, term()}
  def send_message(attrs, opts \\ []) do
    GenServer.call(server(opts), {:send, attrs}, 30_000)
  end

  @doc """
  Posts to a room. Creates the room and joins the sender when it does not exist.
  """
  @spec post(map(), keyword()) :: {:ok, map(), :created | :replayed} | {:error, term()}
  def post(attrs, opts \\ []) do
    GenServer.call(server(opts), {:post, attrs}, 30_000)
  end

  @spec open_room(String.t(), keyword()) :: {:ok, map()} | {:error, term()}
  def open_room(name, opts \\ []) do
    GenServer.call(server(opts), {:open_room, name, opts}, 15_000)
  end

  @spec join_room(String.t(), String.t(), keyword()) :: {:ok, map()} | {:error, term()}
  def join_room(agent_id, room, opts \\ []) do
    GenServer.call(server(opts), {:join_room, agent_id, room}, 15_000)
  end

  @spec leave_room(String.t(), String.t(), keyword()) :: {:ok, map()} | {:error, term()}
  def leave_room(agent_id, room, opts \\ []) do
    GenServer.call(server(opts), {:leave_room, agent_id, room}, 15_000)
  end

  @doc """
  Drains unread messages for `agent_id` across its direct stream and joined rooms.

  Options: `:wait_ms` (long-poll when empty), `:limit`, `:peek` (do not advance
  the read position), `:server`.
  """
  @spec inbox(String.t(), keyword()) :: {:ok, map()} | {:error, term()}
  def inbox(agent_id, opts \\ []) do
    wait_ms = normalize_wait(Keyword.get(opts, :wait_ms, 0))
    GenServer.call(server(opts), {:inbox, agent_id, opts}, wait_ms + 15_000)
  end

  @spec history(String.t(), keyword()) :: {:ok, map()} | {:error, term()}
  def history(stream, opts \\ []) do
    GenServer.call(server(opts), {:history, stream, opts}, 15_000)
  end

  @spec unread(String.t(), keyword()) :: %{String.t() => non_neg_integer()}
  def unread(agent_id, opts \\ []) do
    GenServer.call(server(opts), {:unread, agent_id}, 15_000)
  end

  @spec rooms(keyword()) :: [map()]
  def rooms(opts \\ []) do
    GenServer.call(server(opts), :rooms, 15_000)
  end

  @spec stats(keyword()) :: map()
  def stats(opts \\ []) do
    GenServer.call(server(opts), :stats, 15_000)
  end

  def kinds, do: @kinds

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

    {streams, index, idempotency} = load_streams(home)

    state = %{
      table: table,
      home: home,
      streams: streams,
      index: index,
      idempotency: idempotency,
      cursors: load_json(Path.join(home, "cursors.json"), %{}),
      rooms: load_json(Path.join(home, "rooms.json"), %{}) |> normalize_rooms(),
      waiters: %{}
    }

    {:ok, state}
  end

  @impl true
  def handle_call({:send, attrs}, _from, state) do
    case do_send(attrs, state) do
      {:ok, message, status, state} ->
        state = state |> notify_recipients(message)
        {:reply, {:ok, message, status}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:post, attrs}, _from, state) do
    case do_post(attrs, state) do
      {:ok, message, status, state} ->
        state = notify_recipients(state, message)
        {:reply, {:ok, message, status}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:open_room, name, opts}, _from, state) do
    by = Keyword.get(opts, :by)

    cond do
      not valid_name?(name) ->
        {:reply, {:error, :invalid_room_name}, state}

      true ->
        {room, state} = ensure_room(state, name, by)
        {:reply, {:ok, room}, state}
    end
  end

  def handle_call({:join_room, agent_id, room}, _from, state) do
    cond do
      not valid_name?(agent_id) ->
        {:reply, {:error, :invalid_agent_id}, state}

      not Map.has_key?(state.rooms, room) ->
        {:reply, {:error, :room_not_found}, state}

      true ->
        room_state = add_member(state.rooms[room], agent_id)
        state = state |> put_in([:rooms, room], room_state) |> persist_rooms()
        {:reply, {:ok, room_state}, state}
    end
  end

  def handle_call({:leave_room, agent_id, room}, _from, state) do
    case Map.fetch(state.rooms, room) do
      {:ok, room_state} ->
        updated = %{room_state | members: List.delete(room_state.members, agent_id)}
        state = put_in(state.rooms[room], updated)
        {:reply, {:ok, updated}, persist_rooms(state)}

      :error ->
        {:reply, {:error, :room_not_found}, state}
    end
  end

  def handle_call({:inbox, agent_id, opts}, from, state) do
    cond do
      not valid_name?(agent_id) ->
        {:reply, {:error, :invalid_agent_id}, state}

      true ->
        {payload, state} = drain(agent_id, opts, state)
        wait_ms = normalize_wait(Keyword.get(opts, :wait_ms, 0))

        if payload.messages == [] and wait_ms > 0 do
          ref = make_ref()
          timer = Process.send_after(self(), {:waiter_timeout, ref}, wait_ms)
          waiter = {from, ref, timer}
          waiters = Map.update(state.waiters, agent_id, [waiter], &[waiter | &1])
          {:noreply, %{state | waiters: waiters}}
        else
          {:reply, {:ok, payload}, state}
        end
    end
  end

  def handle_call({:history, stream, opts}, _from, state) do
    stream = normalize_stream(stream)
    since = Keyword.get(opts, :since_seq, 0) || 0
    limit = opts |> Keyword.get(:limit, 50) |> min(@max_limit) |> max(1)

    messages =
      state.streams
      |> Map.get(stream, %{messages: []})
      |> Map.get(:messages, [])
      |> Enum.filter(&(&1.seq > since))
      |> Enum.take(limit)

    {:reply, {:ok, %{stream: stream, messages: messages, count: length(messages)}}, state}
  end

  def handle_call({:unread, agent_id}, _from, state) do
    {:reply, unread_by_stream(agent_id, state), state}
  end

  def handle_call(:rooms, _from, state) do
    {:reply, state.rooms |> Map.values() |> Enum.sort_by(& &1.name), state}
  end

  def handle_call(:stats, _from, state) do
    total =
      state.streams |> Map.values() |> Enum.map(& &1.seq) |> Enum.sum()

    stats = %{
      home: state.home,
      streams: map_size(state.streams),
      messages: total,
      rooms: map_size(state.rooms),
      waiting_agents: map_size(state.waiters)
    }

    {:reply, stats, state}
  end

  @impl true
  def handle_info({:waiter_timeout, ref}, state) do
    case take_waiter(state.waiters, ref) do
      {nil, waiters} ->
        {:noreply, %{state | waiters: waiters}}

      {{agent_id, {from, _ref, _timer}}, waiters} ->
        {:ok, payload} = empty_payload(agent_id, state)
        GenServer.reply(from, {:ok, Map.put(payload, :waited_ms, 0)})
        {:noreply, %{state | waiters: waiters}}
    end
  end

  def handle_info(_message, state), do: {:noreply, state}

  # ---------------------------------------------------------------------------
  # Send / post
  # ---------------------------------------------------------------------------

  defp do_send(attrs, state) do
    attrs = normalize_keys(attrs)
    from = normalize_agent(attrs["from"])
    to = normalize_agent(attrs["to"])

    with :ok <- validate_agent(from),
         :ok <- validate_agent(to),
         {:ok, body} <- validate_body(attrs["body"]),
         {:ok, kind} <- validate_kind(attrs["kind"]),
         {:ok, key} <- validate_key(attrs["idempotency_key"]) do
      stream = "agent:#{to}"

      case replay(from, key, stream, body, state) do
        {:replayed, message} ->
          {:ok, message, :replayed, state}

        {:error, reason} ->
          {:error, reason}

        :new ->
          {message, state} =
            append(state, stream, %{
              from: from,
              to: stream,
              kind: kind,
              body: body,
              reply_to: attrs["reply_to"],
              idempotency_key: key
            })

          {:ok, message, :created, state}
      end
    end
  end

  defp do_post(attrs, state) do
    attrs = normalize_keys(attrs)
    from = normalize_agent(attrs["from"])
    room = normalize_room(attrs["room"] || attrs["to"])

    with :ok <- validate_agent(from),
         :ok <- validate_room(room),
         {:ok, body} <- validate_body(attrs["body"]),
         {:ok, kind} <- validate_kind(attrs["kind"]),
         {:ok, key} <- validate_key(attrs["idempotency_key"]) do
      stream = "room:#{room}"

      case replay(from, key, stream, body, state) do
        {:replayed, message} ->
          {:ok, message, :replayed, state}

        {:error, reason} ->
          {:error, reason}

        :new ->
          {_room, state} = ensure_room(state, room, from)

          state =
            state
            |> put_in([:rooms, room], add_member(state.rooms[room], from))
            |> persist_rooms()

          {message, state} =
            append(state, stream, %{
              from: from,
              to: stream,
              kind: kind,
              body: body,
              reply_to: attrs["reply_to"],
              idempotency_key: key
            })

          {:ok, message, :created, state}
      end
    end
  end

  defp replay(_from, nil, _stream, _body, _state), do: :new

  defp replay(from, key, stream, body, state) do
    case Map.get(state.idempotency, {from, key}) do
      nil ->
        :new

      message_id ->
        case Map.get(state.index, message_id) do
          %{stream: ^stream, body: ^body} = message -> {:replayed, message}
          %{} -> {:error, :idempotency_key_reused}
          nil -> :new
        end
    end
  end

  defp append(state, stream, fields) do
    seq = Map.get(state.streams, stream, %{seq: 0}).seq + 1
    now = DateTime.utc_now() |> DateTime.to_iso8601()

    message =
      Map.merge(fields, %{
        id: "m_" <> Base.encode16(:crypto.strong_rand_bytes(5), case: :lower),
        stream: stream,
        seq: seq,
        inserted_at: now
      })

    entry = Map.get(state.streams, stream, %{seq: 0, messages: []})
    messages = Enum.take(entry.messages ++ [message], -@max_messages)

    state = %{
      state
      | streams: Map.put(state.streams, stream, %{seq: seq, messages: messages}),
        index: Map.put(state.index, message.id, message)
    }

    state =
      case message.idempotency_key do
        nil -> state
        key -> put_in(state.idempotency[{message.from, key}], message.id)
      end

    state = if map_size(state.index) > 5_000, do: prune_index(state), else: state

    persist_message(state, message)

    {message, state}
  end

  # Keep the replay index bounded: retain only ids that are still in a buffered
  # stream (history reads) or referenced by an idempotency key.
  defp prune_index(state) do
    buffered =
      state.streams
      |> Map.values()
      |> Enum.flat_map(fn stream -> Enum.map(stream.messages, & &1.id) end)
      |> MapSet.new()

    keep = MapSet.union(buffered, MapSet.new(Map.values(state.idempotency)))

    index =
      state.index
      |> Enum.filter(fn {id, _message} -> MapSet.member?(keep, id) end)
      |> Map.new()

    %{state | index: index}
  end

  # ---------------------------------------------------------------------------
  # Inbox
  # ---------------------------------------------------------------------------

  defp drain(agent_id, opts, state) do
    limit = opts |> Keyword.get(:limit, 50) |> min(@max_limit) |> max(1)
    peek = Keyword.get(opts, :peek, false)
    cursors = Map.get(state.cursors, agent_id, %{})
    streams = inbox_streams(agent_id, state)

    candidates =
      streams
      |> Enum.flat_map(fn stream ->
        cursor = Map.get(cursors, stream, 0)

        state.streams
        |> Map.get(stream, %{messages: []})
        |> Map.get(:messages, [])
        |> Enum.filter(&(&1.seq > cursor))
      end)
      |> Enum.sort_by(&{&1.inserted_at, &1.stream, &1.seq})
      |> Enum.take(limit)

    new_cursors =
      Enum.reduce(candidates, cursors, fn message, acc ->
        Map.update(acc, message.stream, message.seq, &max(&1, message.seq))
      end)

    state =
      if peek or new_cursors == cursors do
        state
      else
        state
        |> put_in([:cursors, agent_id], new_cursors)
        |> persist_cursors()
      end

    payload = %{
      as: agent_id,
      messages: candidates,
      count: length(candidates),
      unread: remaining_unread(new_cursors, streams, state),
      cursors: new_cursors,
      waited_ms: 0
    }

    {payload, state}
  end

  defp empty_payload(agent_id, state) do
    cursors = Map.get(state.cursors, agent_id, %{})
    streams = inbox_streams(agent_id, state)

    {:ok,
     %{
       as: agent_id,
       messages: [],
       count: 0,
       unread: remaining_unread(cursors, streams, state),
       cursors: cursors
     }}
  end

  defp remaining_unread(cursors, streams, state) do
    Enum.reduce(streams, 0, fn stream, acc ->
      cursor = Map.get(cursors, stream, 0)
      seq = Map.get(state.streams, stream, %{seq: 0}).seq
      acc + max(seq - cursor, 0)
    end)
  end

  defp unread_by_stream(agent_id, state) do
    cursors = Map.get(state.cursors, agent_id, %{})

    inbox_streams(agent_id, state)
    |> Enum.reduce(%{}, fn stream, acc ->
      cursor = Map.get(cursors, stream, 0)
      seq = Map.get(state.streams, stream, %{seq: 0}).seq
      count = max(seq - cursor, 0)
      if count > 0, do: Map.put(acc, stream, count), else: acc
    end)
  end

  defp inbox_streams(agent_id, state) do
    rooms =
      state.rooms
      |> Enum.filter(fn {_name, room} -> agent_id in room.members end)
      |> Enum.map(fn {name, _room} -> "room:#{name}" end)
      |> Enum.sort()

    ["agent:#{agent_id}" | rooms]
  end

  # ---------------------------------------------------------------------------
  # Waiters
  # ---------------------------------------------------------------------------

  defp notify_recipients(state, message) do
    message
    |> recipient_agents(state)
    |> Enum.reduce(state, &wake/2)
  end

  defp recipient_agents(%{to: "agent:" <> agent_id}, _state), do: [agent_id]

  defp recipient_agents(%{to: "room:" <> room}, state) do
    state.rooms
    |> Map.get(room, %{members: []})
    |> Map.get(:members, [])
  end

  defp recipient_agents(_message, _state), do: []

  defp wake(agent_id, state) do
    case Map.get(state.waiters, agent_id) do
      [waiter | rest] ->
        {from, _ref, timer} = waiter
        Process.cancel_timer(timer)
        {payload, state} = drain(agent_id, [limit: 50], state)

        if payload.messages == [] do
          state
        else
          GenServer.reply(from, {:ok, payload})
          %{state | waiters: Map.put(state.waiters, agent_id, rest)}
        end

      _ ->
        state
    end
  end

  defp take_waiter(waiters, ref) do
    Enum.find_value(waiters, {nil, waiters}, fn {agent_id, list} ->
      case Enum.split_with(list, fn {_from, waiter_ref, _timer} -> waiter_ref == ref end) do
        {[], _} ->
          nil

        {[waiter | _], remaining} ->
          updated =
            if remaining == [] do
              Map.delete(waiters, agent_id)
            else
              Map.put(waiters, agent_id, remaining)
            end

          {{agent_id, waiter}, updated}
      end
    end)
  end

  # ---------------------------------------------------------------------------
  # Rooms
  # ---------------------------------------------------------------------------

  defp ensure_room(state, name, by) do
    case Map.get(state.rooms, name) do
      nil ->
        room = %{
          name: name,
          created_by: by,
          created_at: DateTime.utc_now() |> DateTime.to_iso8601(),
          members: if(by, do: [by], else: [])
        }

        state = %{state | rooms: Map.put(state.rooms, name, room)}
        {room, persist_rooms(state)}

      room ->
        updated = if by, do: add_member(room, by), else: room

        if updated == room do
          {room, state}
        else
          state = put_in(state.rooms[name], updated)
          {updated, persist_rooms(state)}
        end
    end
  end

  defp add_member(room, agent_id) do
    if agent_id in room.members do
      room
    else
      %{room | members: Enum.sort([agent_id | room.members])}
    end
  end

  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------

  defp validate_agent(agent_id) do
    cond do
      not is_binary(agent_id) or agent_id == "" -> {:error, :missing_agent}
      not valid_name?(agent_id) -> {:error, :invalid_agent_id}
      true -> :ok
    end
  end

  defp validate_room(room) do
    cond do
      not is_binary(room) or room == "" -> {:error, :missing_room}
      not valid_name?(room) -> {:error, :invalid_room_name}
      true -> :ok
    end
  end

  defp validate_body(body) when is_binary(body) do
    cond do
      String.trim(body) == "" -> {:error, :empty_body}
      byte_size(body) > @max_body -> {:error, {:body_too_large, @max_body}}
      true -> {:ok, body}
    end
  end

  defp validate_body(_), do: {:error, :invalid_body}

  defp validate_kind(nil), do: {:ok, "tell"}
  defp validate_kind(kind) when kind in @kinds, do: {:ok, kind}
  defp validate_kind(other), do: {:error, {:invalid_kind, other}}

  defp validate_key(nil), do: {:ok, nil}

  defp validate_key(key) when is_binary(key) do
    if byte_size(key) > 128, do: {:error, :idempotency_key_too_long}, else: {:ok, key}
  end

  defp validate_key(_), do: {:error, :invalid_idempotency_key}

  # ---------------------------------------------------------------------------
  # Helpers
  # ---------------------------------------------------------------------------

  defp normalize_keys(attrs), do: Map.new(attrs, fn {k, v} -> {to_string(k), v} end)

  defp normalize_agent(nil), do: nil
  defp normalize_agent("@" <> rest), do: rest
  defp normalize_agent(id) when is_binary(id), do: id
  defp normalize_agent(_), do: nil

  defp normalize_room(nil), do: nil
  defp normalize_room("#" <> rest), do: rest
  defp normalize_room("room:" <> rest), do: rest
  defp normalize_room(room) when is_binary(room), do: room
  defp normalize_room(_), do: nil

  defp normalize_stream(stream) when is_binary(stream) do
    cond do
      String.starts_with?(stream, "agent:") -> stream
      String.starts_with?(stream, "room:") -> stream
      true -> "room:" <> stream
    end
  end

  defp normalize_wait(nil), do: 0
  defp normalize_wait(ms) when is_integer(ms) and ms > 0, do: min(ms, @max_wait_ms)
  defp normalize_wait(_), do: 0

  defp server(opts), do: Keyword.get(opts, :server, __MODULE__)

  defp home_root(opts) do
    opts
    |> Keyword.get(:home)
    |> case do
      nil -> Application.get_env(:exhub, :agent_bus_home, @default_home)
      home -> home
    end
    |> Path.expand()
  end

  defp stream_file(home, stream) do
    Path.join(home, String.replace(stream, ":", "~") <> ".jsonl")
  end

  defp persist_message(state, message) do
    line = Jason.encode!(message) <> "\n"
    File.write!(stream_file(state.home, message.stream), line, [:append])
  rescue
    error ->
      Logger.warning("[Agent.Bus] append failed for #{message.stream}: #{inspect(error)}")
      :ok
  end

  defp persist_cursors(state) do
    File.write!(Path.join(state.home, "cursors.json"), Jason.encode!(state.cursors))
    state
  rescue
    error ->
      Logger.warning("[Agent.Bus] cursor persist failed: #{inspect(error)}")
      state
  end

  defp persist_rooms(state) do
    File.write!(Path.join(state.home, "rooms.json"), Jason.encode!(state.rooms, pretty: true))
    state
  rescue
    error ->
      Logger.warning("[Agent.Bus] room persist failed: #{inspect(error)}")
      state
  end

  defp load_streams(home) do
    home
    |> Path.join("*.jsonl")
    |> Path.wildcard()
    |> Enum.reduce({%{}, %{}, %{}}, fn path, {streams, index, idem} ->
      stream =
        path
        |> Path.basename(".jsonl")
        |> String.replace("~", ":")

      messages =
        path
        |> File.read!()
        |> String.split("\n", trim: true)
        |> Enum.flat_map(&decode_message/1)
        |> Enum.take(-@max_messages)

      case messages do
        [] ->
          {streams, index, idem}

        _ ->
          last = List.last(messages)

          streams =
            Map.put(streams, stream, %{seq: last.seq, messages: messages})

          index =
            Enum.reduce(messages, index, fn message, acc ->
              Map.put(acc, message.id, message)
            end)

          idem =
            Enum.reduce(messages, idem, fn message, acc ->
              case message.idempotency_key do
                nil -> acc
                key -> Map.put(acc, {message.from, key}, message.id)
              end
            end)

          {streams, index, idem}
      end
    end)
  end

  defp decode_message(line) do
    case Jason.decode(line) do
      {:ok, raw} when is_map(raw) ->
        [
          %{
            id: raw["id"],
            stream: raw["stream"],
            seq: raw["seq"],
            from: raw["from"],
            to: raw["to"],
            kind: raw["kind"],
            body: raw["body"],
            reply_to: raw["reply_to"],
            idempotency_key: raw["idempotency_key"],
            inserted_at: raw["inserted_at"]
          }
        ]

      _ ->
        []
    end
  end

  defp load_json(path, default) do
    with {:ok, content} <- File.read(path),
         {:ok, decoded} <- Jason.decode(content),
         true <- is_map(decoded) do
      decoded
    else
      _ -> default
    end
  end

  # Rooms are persisted with string keys (JSON); the in-memory representation
  # uses atoms, so normalise on load.
  defp normalize_rooms(rooms) when is_map(rooms) do
    Map.new(rooms, fn {name, room} ->
      {name,
       %{
         name: room["name"] || name,
         created_by: room["created_by"],
         created_at: room["created_at"],
         members: room["members"] || []
       }}
    end)
  end

  defp normalize_rooms(_), do: %{}
end
