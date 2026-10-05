defmodule Exhub.LspBridge.E2eTest do
  @moduledoc """
  Integration smoke test against a **real** elixirLS.

  Tagged `:e2e_lsp`, so it is excluded by default. Run it explicitly (the app
  is not booted, matching the rest of the LspBridge suite):

      mix test --no-start --include e2e_lsp test/exhub/lsp_bridge/e2e_test.exs

  It creates a scratch `mix new` project, boots the LspBridge subtree (nothing
  boots it under `--no-start`), opens a file containing a deliberate compile
  error through `Session.open_file/4`, and waits for elixirLS to push
  diagnostics for it — end-to-end proof that the document-sync pipeline talks
  to a real language server. This is the P1 deliverable check.
  """
  use ExUnit.Case, async: false

  @moduletag :e2e_lsp
  @moduletag timeout: 300_000

  alias Exhub.LspBridge.{Config, Project, Session}

  setup_all do
    unless System.find_executable("language_server.sh") do
      flunk("elixirLS launcher `language_server.sh` is not on PATH")
    end

    project = Path.join(System.tmp_dir!(), "exhub_lsp_e2e_#{:erlang.unique_integer([:positive])}")
    File.mkdir_p!(project)

    {out, status} =
      System.cmd("mix", ["new", ".", "--app", "e2e_proj"], cd: project, stderr_to_stdout: true)

    assert status == 0, "mix new failed:\n#{out}"

    # The subtree `Exhub.LspBridge.Application` would own; started here because
    # `--no-start` leaves the application un-booted.
    start_supervised!({Registry, keys: :unique, name: Exhub.LspBridge.Registry})
    start_supervised!({Config, []})

    start_supervised!(
      {DynamicSupervisor, strategy: :one_for_one, name: Exhub.LspBridge.Supervisor}
    )

    start_supervised!(
      {DynamicSupervisor, strategy: :one_for_one, name: Exhub.LspBridge.SessionSupervisor}
    )

    on_exit(fn -> File.rm_rf(project) end)
    %{project: project}
  end

  test "real elixirLS pushes diagnostics for a file with a compile error", %{project: project} do
    file = Path.join(project, "lib/e2e_proj.ex")
    original = File.read!(file)

    # A syntax error elixirLS is guaranteed to flag.
    content = original <> "\ndefmodule E2eBroken do\n  def oops(, do: :bad\nend\n"

    {:ok, selection} =
      Project.resolve(file, %{"language-id" => "elixir", "project-path" => project})

    refute selection.multi
    assert Enum.any?(selection.servers, &(&1.name == "elixirLS"))

    # Skip dialyzer / dep fetching: this is a compile-diagnostics smoke test, so
    # keep the server responsive and deterministic.
    servers =
      Enum.map(selection.servers, fn config ->
        %{
          config
          | settings: %{
              "elixirLS" => %{
                "dialyzerEnabled" => false,
                "fetchDeps" => false,
                "suggestSpecs" => false
              }
            }
        }
      end)

    {:ok, pid} =
      Session.ensure(selection.root, selection.profile,
        multi: selection.multi,
        server_infos: servers,
        owner: self(),
        diag_idle: 100
      )

    assert {:ok, server_names} =
             Session.open_file(pid, file, content, %{"language-id" => "elixir"})

    assert "elixirLS" in server_names

    # elixirLS first publishes an empty list (clearing stale state) before it
    # compiles and reports the real error, so wait past empty updates.
    deadline = System.monotonic_time(:millisecond) + 240_000
    diagnostics = wait_for_diagnostics(file, deadline)

    assert diagnostics != [], "elixirLS pushed no diagnostics for a file with a syntax error"

    assert Enum.any?(diagnostics, &(&1["severity"] == 1)),
           "expected at least one error diagnostic"

    assert Enum.any?(diagnostics, &(&1["server-name"] == "elixirLS"))

    Session.shutdown(pid)
  end

  test "real elixirLS answers document symbols", %{project: project} do
    file = Path.join(project, "lib/e2e_proj.ex")
    content = File.read!(file)

    {:ok, selection} =
      Project.resolve(file, %{"language-id" => "elixir", "project-path" => project})

    servers =
      Enum.map(selection.servers, fn config ->
        %{
          config
          | settings: %{
              "elixirLS" => %{
                "dialyzerEnabled" => false,
                "fetchDeps" => false,
                "suggestSpecs" => false
              }
            }
        }
      end)

    {:ok, pid} =
      Session.ensure(selection.root, selection.profile,
        multi: selection.multi,
        server_infos: servers,
        owner: self(),
        diag_idle: 100
      )

    assert {:ok, _} = Session.open_file(pid, file, content, %{"language-id" => "elixir"})
    assert :ok = Session.perform(pid, file, "document-symbol", %{})

    assert_receive {:lsp_handler_result, {:symbols, ^file, symbols}}, 120_000
    assert is_list(symbols) and symbols != []

    Session.shutdown(pid)
  end

  defp wait_for_diagnostics(path, deadline) do
    remaining = max(deadline - System.monotonic_time(:millisecond), 0)

    receive do
      {:lsp_diagnostics_update, ^path, diagnostics, _count} when diagnostics != [] ->
        diagnostics

      {:lsp_diagnostics_update, ^path, _diagnostics, _count} ->
        wait_for_diagnostics(path, deadline)
    after
      remaining -> []
    end
  end
end
