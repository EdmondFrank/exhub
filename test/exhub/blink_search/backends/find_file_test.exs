defmodule Exhub.BlinkSearch.Backends.FindFileTest do
  use ExUnit.Case, async: false

  alias Exhub.BlinkSearch.Backends.FindFile

  setup do
    # fd runs through Exile, whose watcher supervisor lives in the :exile
    # application — not booted under --no-start.
    {:ok, _} = Application.ensure_all_started(:exile)

    # No on_exit clear: it runs async and would race with the next test's
    # in-flight cache entries. Tests use unique tmp dirs, so leftovers are
    # harmless.
    FindFile.clear_cache()
    :ok
  end

  defp tmp_project do
    dir = Path.join(System.tmp_dir!(), "blink_search_ff_#{System.unique_integer([:positive])}")
    File.mkdir_p!(Path.join(dir, "src"))
    File.write!(Path.join([dir, "src", "alpha_module.ex"]), "")
    File.write!(Path.join(dir, "README.md"), "")
    dir
  end

  # ── search_match/2 ───────────────────────────────────────────────────

  describe "search_match/2" do
    test "finds files matching a fuzzy pattern" do
      dir = tmp_project()

      results = FindFile.search_match("alpha mod", %{search_path: dir, search_dir: dir})

      assert "src/alpha_module.ex" in results
    end

    test "empty prefix lists directory contents" do
      dir = tmp_project()

      results = FindFile.search_match("", %{search_path: dir, search_dir: dir})

      assert Enum.sort(results) == ["README.md", "src"]
    end

    test "caches results for an identical pattern and path" do
      if System.find_executable("fd") || System.find_executable("fdfind") do
        dir = tmp_project()
        state = %{search_path: dir, search_dir: dir}

        assert FindFile.search_match("alpha", state) == ["src/alpha_module.ex"]

        # A new matching file does not appear until the cache is cleared
        File.write!(Path.join(dir, "alpha_new.ex"), "")
        assert FindFile.search_match("alpha", state) == ["src/alpha_module.ex"]

        FindFile.clear_cache()
        results = FindFile.search_match("alpha", state)
        assert Enum.sort(results) == ["alpha_new.ex", "src/alpha_module.ex"]
      end
    end
  end
end
