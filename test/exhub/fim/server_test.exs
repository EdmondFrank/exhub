defmodule Exhub.Fim.ServerTest do
  use ExUnit.Case, async: false

  alias Exhub.Fim.Server

  # ---------------------------------------------------------------------------
  # Fake clients — stubbed via the `_client` request opt so no network is ever
  # touched and no global app env is mutated (safe across test files).
  # ---------------------------------------------------------------------------

  defmodule FakeClient do
    def complete(_provider, _context, opts) do
      {:ok, "completion-#{Map.get(opts || %{}, "n", 1)}"}
    end
  end

  defmodule ErrorClient do
    def complete(_provider, _context, _opts), do: {:error, "boom"}
  end

  defmodule SlowClient do
    def complete(_provider, _context, _opts) do
      Process.sleep(10_000)
      {:ok, "never"}
    end
  end

  defmodule QuoteClient do
    def complete(_provider, _context, _opts), do: {:ok, ~s(say "hi" \\ done)}
  end

  # ---------------------------------------------------------------------------
  # Setup — in-process socket handler so Exhub.send_message reaches this test.
  # ---------------------------------------------------------------------------

  setup_all do
    # The registry is linked to the ExUnit runner process (long-lived), so it
    # survives individual test processes; per-test on_exit unregistering still
    # works because each test process dies only after its own teardown ran.
    case Registry.start_link(keys: :unique, name: Exhub.Registry) do
      {:ok, _pid} -> :ok
      {:error, {:already_started, _pid}} -> :ok
    end

    :ok
  end

  setup do
    Registry.register(Exhub.Registry, "socket_handler", :socket_handler)
    on_exit(fn -> Registry.unregister(Exhub.Registry, "socket_handler") end)
    :ok
  end

  # Collects elisp frames sent to Emacs until the done frame (or a deadline).
  defp collect_until_done(acc \\ [], deadline \\ System.monotonic_time(:millisecond) + 2_000) do
    remaining = deadline - System.monotonic_time(:millisecond)

    if remaining <= 0 do
      Enum.reverse(acc)
    else
      receive do
        {:send_to_emacs, message} ->
          collect_until_done([message | acc], deadline)
      after
        remaining -> Enum.reverse(acc)
      end
    end
  end

  # ---------------------------------------------------------------------------
  # Tests
  # ---------------------------------------------------------------------------

  test "dispatches n concurrent completions and delivers items + done" do
    Server.complete(42, "codestral", %{"before-cursor" => "def f"}, %{
      "n" => 2,
      "_client" => FakeClient
    })

    frames = collect_until_done()

    assert length(frames) == 3
    assert Enum.any?(frames, &(&1 == "(exhub-fim-async-items 42 '(\"completion-2\"))"))
    assert Enum.any?(frames, &(&1 == "(exhub-fim-async-done 42)"))
  end

  test "escapes backslashes and quotes in completion text" do
    Server.complete(43, "codestral", %{}, %{"n" => 1, "_client" => QuoteClient})
    frames = collect_until_done()

    assert Enum.any?(
             frames,
             &(&1 == "(exhub-fim-async-items 43 '(\"say \\\"hi\\\" \\\\ done\"))")
           )
  end

  test "delivers an error frame and still finishes when every request fails" do
    Server.complete(44, "codestral", %{}, %{"n" => 2, "_client" => ErrorClient})
    frames = collect_until_done()

    assert Enum.any?(frames, &(&1 == "(exhub-fim-async-error 44 \"boom\")"))
    assert Enum.any?(frames, &(&1 == "(exhub-fim-async-done 44)"))
  end

  test "cancel kills in-flight tasks before they deliver" do
    Server.complete(45, "codestral", %{}, %{"n" => 1, "_client" => SlowClient})
    Server.cancel(45)

    refute_receive {:send_to_emacs, _message}, 300
  end

  test "starts lazily and reuses the running instance" do
    assert {:ok, pid} = Server.ensure_started()
    assert {:ok, ^pid} = Server.ensure_started()
    assert Process.alive?(pid)
  end

  test "falls back to app-env client when the _client opt is absent" do
    # Regression: a missing "_client" key must not resolve to nil (nil is an
    # atom) — that produced "function nil.complete/3 is undefined" in prod.
    previous = Application.get_env(:exhub, :fim_client_module)
    Application.put_env(:exhub, :fim_client_module, FakeClient)

    on_exit(fn ->
      if is_nil(previous) do
        Application.delete_env(:exhub, :fim_client_module)
      else
        Application.put_env(:exhub, :fim_client_module, previous)
      end
    end)

    Server.complete(46, "codestral", %{"before-cursor" => "def f"}, %{"n" => 1})
    frames = collect_until_done()

    assert Enum.any?(frames, &(&1 == "(exhub-fim-async-items 46 '(\"completion-1\"))"))
    assert Enum.any?(frames, &(&1 == "(exhub-fim-async-done 46)"))
  end
end
