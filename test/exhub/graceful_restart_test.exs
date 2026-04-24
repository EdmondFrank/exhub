defmodule Exhub.GracefulRestartTest do
  use ExUnit.Case, async: true

  alias Exhub.GracefulRestart

  describe "socket_activation?/0" do
    test "returns false when LISTEN_FDS is not set" do
      System.delete_env("LISTEN_FDS")
      System.delete_env("LISTEN_PID")
      assert GracefulRestart.socket_activation?() == false
    end

    test "returns false when LISTEN_PID does not match current process" do
      System.put_env("LISTEN_FDS", "1")
      System.put_env("LISTEN_PID", "999999")
      assert GracefulRestart.socket_activation?() == false
    end
  end

  describe "extract_listen_fd/0" do
    test "returns :error when not in socket activation mode" do
      System.delete_env("LISTEN_FDS")
      assert GracefulRestart.extract_listen_fd() == :error
    end
  end

  describe "schedule_restart/2" do
    test "returns {:ok, pid} for soft restart" do
      assert {:ok, pid} = GracefulRestart.schedule_restart(:soft, 10_000)
      assert is_pid(pid)
      Process.exit(pid, :kill)
    end

    test "returns {:ok, pid} for hard restart" do
      assert {:ok, pid} = GracefulRestart.schedule_restart(:hard, 10_000)
      assert is_pid(pid)
      Process.exit(pid, :kill)
    end
  end
end
