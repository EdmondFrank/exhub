defmodule Exhub.Toonflow.ScriptTest do
  use ExUnit.Case, async: true

  alias Exhub.Toonflow.Script

  describe "parse_script/1" do
    test "strips a leading fence and language tag" do
      assert Script.parse_script("```markdown\n# 第一场\n\n对白。\n```") == "# 第一场\n\n对白。"
    end

    test "returns plain content unchanged" do
      assert Script.parse_script("  # 场景  \n") == "# 场景"
    end

    test "handles non-binary input" do
      assert Script.parse_script(nil) == ""
    end
  end
end
