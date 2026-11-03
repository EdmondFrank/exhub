defmodule Exhub.Agent.RegistryTest do
  use ExUnit.Case, async: false

  alias Exhub.Agent.Registry

  setup do
    home = tmp_home("registry")
    start_registry(home)
    %{home: home}
  end

  defp tmp_home(prefix) do
    unique = System.unique_integer([:positive])
    path = Path.join(System.tmp_dir!(), "exhub-#{prefix}-#{unique}")
    File.mkdir_p!(path)
    on_exit(fn -> File.rm_rf(path) end)
    path
  end

  defp start_registry(home) do
    {:ok, pid} = Registry.start_link(home: home)

    on_exit(fn ->
      if Process.alive?(pid), do: GenServer.stop(pid)
    end)

    pid
  end

  describe "register/1" do
    test "creates a record with a home directory and a MEMORY.md" do
      assert {:ok, record} =
               Registry.register(%{
                 "id" => "coder",
                 "name" => "Coder",
                 "description" => "writes Elixir",
                 "capabilities" => ["elixir", "tests"]
               })

      assert record.id == "coder"
      assert record.name == "Coder"
      assert record.status == "idle"
      assert record.capabilities == ["elixir", "tests"]
      assert File.exists?(Registry.memory_path("coder"))
      assert File.exists?(Path.join(record.home, "agent.json"))
    end

    test "partial updates preserve fields that were not mentioned" do
      {:ok, _} = Registry.register(%{"id" => "coder", "description" => "writes Elixir"})
      {:ok, created} = Registry.get("coder")

      {:ok, updated} = Registry.register(%{"id" => "coder", "status" => "working"})

      assert updated.description == "writes Elixir"
      assert updated.status == "working"
      assert updated.created_at == created.created_at
    end

    test "rejects an unknown status" do
      assert {:error, {:invalid_status, "sleepy"}} =
               Registry.register(%{"id" => "coder", "status" => "sleepy"})
    end

    test "rejects an invalid id" do
      assert {:error, :invalid_id} = Registry.register(%{"id" => "Bad Id!"})
    end

    test "truncates detail to 200 characters" do
      {:ok, record} =
        Registry.register(%{"id" => "coder", "detail" => String.duplicate("x", 500)})

      assert String.length(record.detail) == 200
    end
  end

  describe "set_status/3" do
    test "updates status and detail" do
      {:ok, _} = Registry.register(%{"id" => "coder"})
      {:ok, record} = Registry.set_status("coder", "blocked", "waiting on review")
      assert record.status == "blocked"
      assert record.detail == "waiting on review"
    end
  end

  describe "list and delete" do
    test "returns records sorted by id and deletes them" do
      {:ok, _} = Registry.register(%{"id" => "reviewer"})
      {:ok, _} = Registry.register(%{"id" => "coder"})

      assert Enum.map(Registry.list(), & &1.id) == ["coder", "reviewer"]
      assert :ok = Registry.delete("coder")
      assert {:error, :not_found} = Registry.get("coder")
    end
  end

  describe "persistence" do
    test "records survive a restart", %{home: home} do
      {:ok, _} = Registry.register(%{"id" => "coder", "description" => "kept"})

      pid = Process.whereis(Registry)
      GenServer.stop(pid)
      start_registry(home)

      assert {:ok, record} = Registry.get("coder")
      assert record.description == "kept"
    end
  end
end
