defmodule Exhub.ResponseHandlers.ExhubChatTest do
  use ExUnit.Case, async: true

  alias Exhub.ResponseHandlers.ExhubChat

  @diff "diff --git a/lib/a.ex b/lib/a.ex\n" <>
          "index 111..222 100644\n" <>
          "--- a/lib/a.ex\n" <>
          "+++ b/lib/a.ex\n" <>
          "@@ -1,3 +1,4 @@\n" <>
          "-old line\n" <>
          "+new line\n" <>
          "+another line\n" <>
          " context\n" <>
          "diff --git a/assets/logo.png b/assets/logo.png\n" <>
          "index 333..444 100644\n" <>
          "Binary files a/assets/logo.png and b/assets/logo.png differ\n"

  describe "max_diff_chars/0" do
    test "defaults to 40k characters" do
      assert ExhubChat.max_diff_chars() == 40_000
    end
  end

  describe "truncate_diff/2" do
    test "keeps small diffs untouched" do
      assert {"+a\n-b", nil} == ExhubChat.truncate_diff("+a\n-b", 100)
    end

    test "caps large diffs and explains the truncation" do
      big = String.duplicate("+line\n", 100)
      {head, note} = ExhubChat.truncate_diff(big, 100)

      assert byte_size(head) <= 100
      assert note =~ "diff truncated"
      assert note =~ "of #{byte_size(big)} bytes"
    end

    test "cuts on a hunk boundary so no partial file is sent" do
      diff = "diff --git a/a.ex b/a.ex\n+a\ndiff --git a/b.ex b/b.ex\n+bbbbbbbbbb\n"
      {head, _note} = ExhubChat.truncate_diff(diff, 45)

      assert String.ends_with?(head, "+a")
      refute head =~ "b.ex"
    end
  end

  describe "diff_stat/1" do
    test "counts added and removed lines per file" do
      assert ExhubChat.diff_stat(@diff) ==
               "lib/a.ex | +2 -1\nassets/logo.png | binary"
    end

    test "returns an empty string for an empty diff" do
      assert ExhubChat.diff_stat("") == ""
    end
  end

  describe "build_commit_user_message/3" do
    test "includes history, the git summary and the diff" do
      msg = ExhubChat.build_commit_user_message(@diff, "feat: prior", stat: "lib/a.ex | +2 -1")

      assert msg =~ "Recent commit messages:\nfeat: prior"
      assert msg =~ "Staged changes (summary):\nlib/a.ex | +2 -1"
      assert msg =~ "Current changes (diff):"
      assert msg =~ "+new line"
      refute msg =~ "diff truncated"
    end

    test "derives the summary from the diff when git --stat is unavailable" do
      msg = ExhubChat.build_commit_user_message(@diff, "")

      assert msg =~ "lib/a.ex | +2 -1"
      assert msg =~ "assets/logo.png | binary"
    end

    test "marks a truncated payload inside the prompt" do
      big = "diff --git a/a.ex b/a.ex\n" <> String.duplicate("+line\n", 10_000)
      msg = ExhubChat.build_commit_user_message(big, "", stat: "a.ex | +10000 -0")

      assert msg =~ "diff truncated"
      assert msg =~ "a.ex | +10000 -0"
    end
  end

  describe "clean_reply/1" do
    test "strips markdown code fences" do
      assert ExhubChat.clean_reply("```text\nfeat: add thing\n```") == "feat: add thing"
      assert ExhubChat.clean_reply("```\nfix: bug\n```") == "fix: bug"
    end

    test "strips wrapping quotes" do
      assert ExhubChat.clean_reply(~s("feat: add thing")) == "feat: add thing"
      assert ExhubChat.clean_reply("'fix: bug'") == "fix: bug"
    end

    test "keeps inner quotes" do
      assert ExhubChat.clean_reply(~s("feat: say "hi" now")) == ~s(feat: say "hi" now)
    end

    test "trims surrounding whitespace" do
      assert ExhubChat.clean_reply("  feat: add thing  \n") == "feat: add thing"
    end

    test "leaves an empty reply empty" do
      assert ExhubChat.clean_reply("   ") == ""
    end
  end
end
