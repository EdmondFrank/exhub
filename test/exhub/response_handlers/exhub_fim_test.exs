defmodule Exhub.ResponseHandlers.ExhubFimTest do
  use ExUnit.Case, async: false

  alias Exhub.ResponseHandlers.ExhubFim

  # Stub the completion HTTP client so handler tests never make network calls,
  # even when a real API key is configured in the local environment.
  defmodule FakeClient do
    def complete(_provider, _context, _opts), do: {:ok, "stub"}
  end

  setup do
    previous = Application.get_env(:exhub, :fim_client_module)
    Application.put_env(:exhub, :fim_client_module, FakeClient)

    on_exit(fn ->
      if is_nil(previous) do
        Application.delete_env(:exhub, :fim_client_module)
      else
        Application.put_env(:exhub, :fim_client_module, previous)
      end
    end)

    :ok
  end

  describe "call/1" do
    test "parses complete action and returns nil" do
      assert ExhubFim.call([
               "exhub-fim",
               "complete",
               1,
               "codestral",
               %{"before-cursor" => "def f"},
               %{"n" => 2}
             ]) == nil
    end

    test "accepts string request ids" do
      assert ExhubFim.call(["exhub-fim", "cancel", "99"]) == nil
    end

    test "coerces missing context/opts to maps" do
      assert ExhubFim.call(["exhub-fim", "complete", 2, "codestral", nil, nil]) == nil
    end

    test "parses cancel action" do
      assert ExhubFim.call(["exhub-fim", "cancel", 7]) == nil
    end

    test "unknown action logs and returns nil" do
      import ExUnit.CaptureLog

      log =
        capture_log(fn ->
          assert ExhubFim.call(["exhub-fim", "bogus", 1]) == nil
        end)

      assert log =~ "Unknown exhub-fim action"
    end
  end

  describe "DefaultResponseHandler routing" do
    test "routes exhub-fim messages to the exhub-fim handler" do
      import ExUnit.CaptureLog

      message = Jason.encode!(["func", ["exhub-fim", "cancel", 5]])

      log =
        capture_log(fn ->
          assert Exhub.DefaultResponseHandler.call(message) == nil
        end)

      refute log =~ "Unknown action: exhub-fim"
    end
  end
end