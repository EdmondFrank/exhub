defmodule Exhub.Memory.SecretScanTest do
  use ExUnit.Case, async: true

  alias Exhub.Memory.SecretScan

  test "clean text passes" do
    assert :ok = SecretScan.scan("restart the kuri daemon after changing config")
  end

  test "flags an api key" do
    assert {:error, findings} = SecretScan.scan("token sk-abcdefghijklmnop1234")
    assert "api key" in findings
  end

  test "flags an inline credential" do
    assert {:error, findings} = SecretScan.scan("password = hunter2secret")
    assert "inline credential" in findings
  end

  test "scan_many merges findings across fields" do
    assert {:error, findings} = SecretScan.scan_many(["ok", "ghp_aaaaaaaaaaaaaaaaaaaa"])
    assert "github token" in findings
  end

  test "redact replaces matches" do
    redacted = SecretScan.redact("use sk-abcdefghijklmnop1234 here")
    refute redacted =~ "sk-abcdefghijklmnop1234"
    assert redacted =~ "[REDACTED]"
  end
end
