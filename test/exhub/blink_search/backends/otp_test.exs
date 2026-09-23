defmodule Exhub.BlinkSearch.Backends.OTPTest do
  use ExUnit.Case, async: true

  alias Exhub.BlinkSearch.Backends.OTP

  # Only pure helpers are exercised here: no `Exhub.send_message/1` path is
  # hit, so no bare `Exhub.Registry` is needed under `--no-start`.

  @json ~s([{"issuer":"JumpServer","label":"JumpServer:qinhuatian","otp_code":"123456"},) <>
          ~s({"issuer":"GitHub","label":"GitHub:EdmondFrank","otp_code":"654321"}])

  describe "parse_entries/1" do
    test "keeps label and search text, drops otp_code" do
      entries = OTP.parse_entries(@json)

      assert entries == [
               %{text: "JumpServer:qinhuatian", search: "JumpServer JumpServer:qinhuatian"},
               %{text: "GitHub:EdmondFrank", search: "GitHub GitHub:EdmondFrank"}
             ]

      refute inspect(entries) =~ "123456"
      refute inspect(entries) =~ "654321"
    end

    test "handles invalid JSON and non-list payloads" do
      assert OTP.parse_entries("not json") == []
      assert OTP.parse_entries(~s({"issuer":"x"})) == []
    end

    test "skips entries without a usable label" do
      assert OTP.parse_entries(~s([{"issuer":"x"},{"label":""},{"label":"ok"}])) == [
               %{text: "ok", search: " ok"}
             ]
    end
  end

  describe "filter_entries/2" do
    test "matches on issuer or label and maps to display text" do
      entries = OTP.parse_entries(@json)

      assert OTP.filter_entries(entries, "jump") == ["JumpServer:qinhuatian"]
      assert OTP.filter_entries(entries, "edmond") == ["GitHub:EdmondFrank"]
    end

    test "empty prefix keeps all, sorted" do
      entries = OTP.parse_entries(@json)

      assert OTP.filter_entries(entries, "") == [
               "GitHub:EdmondFrank",
               "JumpServer:qinhuatian"
             ]
    end
  end

  describe "escape_glob/1" do
    test "escapes cotp glob metacharacters" do
      assert OTP.escape_glob("a*b?c[d]") == "a\\*b\\?c\\[d]"
      assert OTP.escape_glob("back\\slash") == "back\\\\slash"
      assert OTP.escape_glob("plain:label") == "plain:label"
    end
  end

  describe "update/2" do
    test "stores pass key and optional db path" do
      state = OTP.update(["cotp_pass", "/tmp/db.cotp"], %{})
      assert state.pass_key == "cotp_pass"
      assert state.db_path == "/tmp/db.cotp"
    end

    test "blank db path becomes nil" do
      assert OTP.update(["cotp_pass", ""], %{}).db_path == nil
      assert OTP.update(["cotp_pass"], %{}).db_path == nil
    end

    test "ignores malformed config" do
      assert OTP.update([nil], %{existing: 1}) == %{existing: 1}
      assert OTP.update(:nope, %{}) == %{}
    end
  end

  describe "search_match/2 password source" do
    test "returns [] when the SecretVault secret is absent" do
      assert OTP.search_match("jump", %{pass_key: "exhub_otp_test_missing_secret"}) == []
    end
  end

  describe "select/2 and parent/2" do
    test "never touch the clipboard" do
      assert OTP.select("label", %{}) == :ok
      assert OTP.parent("label", %{}) == :ok
    end
  end
end
