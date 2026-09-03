defmodule Exhub.Fim.Server do
  @moduledoc """
  GenServer coordinator for asynchronous FIM (fill-in-the-middle) completion.

  Replaces the Emacs-side synchronous `plz` HTTP requests in `exhub-fim.el`:
  Emacs sends one WebSocket command per completion batch, this server runs N
  concurrent HTTP requests (one per candidate, mirroring the old multi-request
  behavior), and streams results back to Emacs as elisp payloads.

  ## Message flow

  1. Emacs sends `["func", ["exhub-fim", "complete", request_id, provider, context, opts]]`
  2. `Exhub.Fim.Server` spawns one `Task` per completion (`opts["n"]`, default 3)
  3. Each task calls `Exhub.Fim.Client.complete/3` and reports back
  4. Every successful text is forwarded immediately as
     `(exhub-fim-async-items REQUEST_ID '("item"))` via `Exhub.send_message/1`
  5. When all tasks finish, `(exhub-fim-async-done REQUEST_ID)` is sent so
     Emacs can release the request registry entry

  `cancel/1` kills the in-flight tasks for a request id (invoked by Emacs when
  the cursor moves and the suggestion is dismissed).

  The server is *lazily* started (`ensure_started/0`) so the feature works in a
  running release after a hot reload without a VM restart; it is also listed in
  the application supervision tree so future boots supervise it normally.
  """

  use GenServer
  require Logger

  alias Exhub.BlinkSearch.Backend
  alias Exhub.Fim.Client

  @default_n 3
  @max_n 10

  # ===========================================================================
  # Public API
  # ===========================================================================

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Starts the server if it is not already running.

  Uses an unlinked `GenServer.start/3` so callers (WebSocket handler tasks)
  never get linked to it. Returns `{:ok, pid}`.
  """
  def ensure_started do
    case Process.whereis(__MODULE__) do
      nil ->
        case GenServer.start(__MODULE__, %{}, name: __MODULE__) do
          {:ok, pid} -> {:ok, pid}
          {:error, {:already_started, pid}} -> {:ok, pid}
          {:error, reason} -> {:error, reason}
        end

      pid ->
        {:ok, pid}
    end
  end

  @doc "Run one batch of FIM completions; results are pushed to Emacs."
  def complete(request_id, provider, context, opts \\ %{}) when is_integer(request_id) do
    ensure_started()
    GenServer.cast(__MODULE__, {:complete, request_id, provider, context, opts || %{}})
  end

  @doc "Cancel in-flight completions for REQUEST_ID (kills their tasks)."
  def cancel(request_id) when is_integer(request_id) do
    if Process.whereis(__MODULE__) do
      GenServer.cast(__MODULE__, {:cancel, request_id})
    end

    :ok
  end

  # ===========================================================================
  # GenServer callbacks
  # ===========================================================================

  @impl true
  def init(_opts), do: {:ok, %{}}

  @impl true
  def handle_cast({:complete, request_id, provider, context, opts}, state) do
    n = n_completions(opts)
    parent = self()

    tasks =
      for _ <- 1..n do
        {ref, pid} =
          Task.start(fn ->
            send(parent, {:fim_result, request_id, run_completion(provider, context, opts)})
          end)

        {ref, pid}
      end

    state =
      Map.put(state, request_id, %{
        total: n,
        done: 0,
        tasks: tasks
      })

    {:noreply, state}
  end

  @impl true
  def handle_cast({:cancel, request_id}, state) do
    case Map.pop(state, request_id) do
      {nil, state} ->
        {:noreply, state}

      {%{tasks: tasks}, state} ->
        Enum.each(tasks, fn {_ref, pid} -> Process.exit(pid, :kill) end)
        {:noreply, state}
    end
  end

  @impl true
  def handle_info({:fim_result, request_id, result}, state) do
    case Map.get(state, request_id) do
      nil ->
        # Already cancelled or unknown; drop.
        {:noreply, state}

      entry ->
        entry = deliver_result(entry, request_id, result)
        entry = %{entry | done: entry.done + 1}

        if entry.done >= entry.total do
          send_to_emacs("(exhub-fim-async-done #{request_id})")
          {:noreply, Map.delete(state, request_id)}
        else
          {:noreply, Map.put(state, request_id, entry)}
        end
    end
  end

  @impl true
  def handle_info(_msg, state), do: {:noreply, state}

  # ===========================================================================
  # Private helpers
  # ===========================================================================

  defp run_completion(provider, context, opts) do
    client_module(opts).complete(provider, context, opts)
  rescue
    e ->
      Logger.error("Exhub.Fim completion failed: #{inspect(e)}")
      {:error, Exception.message(e)}
  end

  # Test seam: an explicit "_client" opt wins over the app env so tests can stub
  # the HTTP layer without racing other test modules on global state.
  #
  # Careful: `nil` is an atom, so a missing "_client" key must not match the
  # module clause (that caused "function nil.complete/3 is undefined" in prod).
  defp client_module(opts) do
    case Map.get(opts || %{}, "_client") do
      nil -> default_client_module()
      module when is_atom(module) and not is_nil(module) -> module
      _ -> default_client_module()
    end
  end

  defp default_client_module do
    Application.get_env(:exhub, :fim_client_module) || Client
  end

  defp deliver_result(entry, request_id, {:ok, text}) when is_binary(text) and text != "" do
    send_to_emacs("(exhub-fim-async-items #{request_id} '(#{Backend.elisp_quote(text)}))")
    entry
  end

  defp deliver_result(entry, _request_id, {:ok, _empty}), do: entry

  defp deliver_result(entry, request_id, {:error, reason}) do
    Logger.warning("Exhub.Fim request #{request_id} failed: #{inspect(reason)}")

    send_to_emacs(
      "(exhub-fim-async-error #{request_id} #{Backend.elisp_quote(to_string(reason))})"
    )

    entry
  end

  defp deliver_result(entry, _request_id, other) do
    Logger.warning("Exhub.Fim unexpected result: #{inspect(other)}")
    entry
  end

  defp n_completions(opts) do
    case Map.get(opts || %{}, "n") do
      n when is_integer(n) and n > 0 -> min(n, @max_n)
      _ -> @default_n
    end
  end

  defp send_to_emacs(elisp) do
    # Guard so the module also works (and is testable) when the app registry
    # is not running, e.g. under `mix test --no-start`.
    if Process.whereis(Exhub.Registry) do
      Exhub.send_message(elisp)
    end
  end
end
