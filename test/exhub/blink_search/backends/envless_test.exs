defmodule Exhub.BlinkSearch.Backends.EnvlessTest do
  use ExUnit.Case, async: true

  alias Exhub.BlinkSearch.Backends.Envless

  # Only pure helpers are exercised here: no `Exhub.send_message/1` path is
  # hit, so no bare `Exhub.Registry` is needed under `--no-start`.

  describe "parse_keys/1" do
    test "splits, trims and dedupes key names" do
      assert Envless.parse_keys("A\nB\n\n  C  \nA\n") == ["A", "B", "C"]
    end

    test "returns [] for empty output" do
      assert Envless.parse_keys("") == []
    end
  end

  describe "filter_keys/2" do
    test "filters fuzzy matches and sorts" do
      keys = ["GITEE_TOKEN", "COTP_PASS", "COMPASS_SSH_PASS", "GITHUB_PERSONAL_ACCESS_TOKEN"]

      assert Envless.filter_keys(keys, "pass") == ["COMPASS_SSH_PASS", "COTP_PASS"]
    end

    test "empty prefix returns all, sorted" do
      assert Envless.filter_keys(["b", "a"], "") == ["a", "b"]
    end

    test "caps the number of candidates" do
      keys = Enum.map(1..80, &"KEY_#{&1}")
      assert length(Envless.filter_keys(keys, "KEY")) == 50
    end
  end

  describe "validate_key/1" do
    test "accepts env var style names" do
      assert Envless.validate_key("COTP_PASS") == :ok
      assert Envless.validate_key("A1_b") == :ok
    end

    test "rejects shell metacharacters and empty names" do
      assert Envless.validate_key("FOO; rm -rf /") == {:error, :invalid_key}
      assert Envless.validate_key("$(whoami)") == {:error, :invalid_key}
      assert Envless.validate_key("") == {:error, :invalid_key}
      assert Envless.validate_key("1BAD") == {:error, :invalid_key}
      assert Envless.validate_key(nil) == {:error, :invalid_key}
    end
  end

  describe "update/2" do
    test "stores root and env" do
      state = Envless.update(["/vault", "prod"], %{})
      assert state.root == "/vault"
      assert state.env == "prod"
    end

    test "defaults the env when missing" do
      state = Envless.update(["/vault"], %{})
      assert state.root == "/vault"
      assert state.env == "dev"
    end

    test "ignores malformed config" do
      assert Envless.update([nil, "prod"], %{existing: 1}) == %{existing: 1}
      assert Envless.update(:nope, %{}) == %{}
    end
  end

  describe "select/2 and parent/2" do
    test "never touch the clipboard" do
      assert Envless.select("KEY", %{}) == :ok
      assert Envless.parent("KEY", %{}) == :ok
    end
  end
end
