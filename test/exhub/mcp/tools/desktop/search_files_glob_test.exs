defmodule Exhub.MCP.Tools.Desktop.SearchFilesGlobTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Desktop.SearchFiles

  # Exile (used by the ripgrep path) needs its supervisor running.
  setup_all do
    Application.ensure_all_started(:exile)
    :ok
  end

  setup do
    tmp_dir =
      System.tmp_dir!()
      |> Path.join("search_files_glob_#{System.unique_integer([:positive])}")

    File.mkdir_p!(Path.join([tmp_dir, "sub", "deep"]))
    File.mkdir_p!(Path.join(tmp_dir, "node_modules"))

    File.write!(Path.join(tmp_dir, "a.ex"), "defmodule A do\nend\n")
    File.write!(Path.join(tmp_dir, "b.exs"), "IO.puts(:b)\n")
    File.write!(Path.join(tmp_dir, "README.md"), "# readme\n")
    File.write!(Path.join([tmp_dir, "sub", "c.ex"]), "defmodule C do\nend\n")
    File.write!(Path.join([tmp_dir, "sub", "deep", "d.ex"]), "defmodule D do\nend\n")
    File.write!(Path.join([tmp_dir, "node_modules", "junk.ex"]), "defmodule Junk do\nend\n")
    File.write!(Path.join(tmp_dir, ".hidden.ex"), "defmodule Hidden do\nend\n")

    # ripgrep honors `.ignore` without requiring a git repository.
    File.write!(Path.join(tmp_dir, ".ignore"), "node_modules/\n")

    on_exit(fn -> File.rm_rf!(tmp_dir) end)
    {:ok, tmp_dir: tmp_dir}
  end

  defp search(params) do
    {:reply, resp, _frame} = SearchFiles.execute(params, %{})
    assert resp.isError == false
    Enum.find(resp.content, &(&1["type"] == "text"))["text"]
  end

  # Extract the TOON `results[N]: a,b,c` list as a list of entries.
  defp results(text) do
    [_, list] = Regex.run(~r/^results\[\d+\]: (.*)$/m, text)
    if list == "", do: [], else: String.split(list, ",")
  end

  test "matches nested files with ** and returns paths relative to path", %{tmp_dir: tmp_dir} do
    text = search(%{path: tmp_dir, pattern: "**/*.ex", search_type: "glob"})

    assert text =~ "a.ex"
    assert text =~ "sub/c.ex"
    assert text =~ "sub/deep/d.ex"
    refute text =~ "/sub/c.ex"
  end

  test "excludes non-matching extensions, ignored and hidden files by default", %{
    tmp_dir: tmp_dir
  } do
    text = search(%{path: tmp_dir, pattern: "**/*.ex", search_type: "glob"})

    refute text =~ "b.exs"
    refute text =~ "README.md"
    refute text =~ "junk.ex"
    refute text =~ ".hidden.ex"
  end

  test "a pattern without a slash is anchored to path", %{tmp_dir: tmp_dir} do
    text = search(%{path: tmp_dir, pattern: "*.ex", search_type: "glob"})

    assert text =~ "a.ex"
    refute text =~ "sub/c.ex"
    refute text =~ "sub/deep/d.ex"
  end

  test "supports brace alternation", %{tmp_dir: tmp_dir} do
    text = search(%{path: tmp_dir, pattern: "*.{ex,exs}", search_type: "glob"})

    assert text =~ "a.ex"
    assert text =~ "b.exs"
    refute text =~ "README.md"
  end

  test "the ignore option excludes additional globs", %{tmp_dir: tmp_dir} do
    text =
      search(%{
        path: tmp_dir,
        pattern: "**/*.ex",
        search_type: "glob",
        ignore: ["**/deep/**"]
      })

    assert text =~ "sub/c.ex"
    refute text =~ "sub/deep/d.ex"
  end

  test "include_ignored surfaces ignored and hidden files", %{tmp_dir: tmp_dir} do
    text =
      search(%{
        path: tmp_dir,
        pattern: "**/*.ex",
        search_type: "glob",
        include_ignored: true
      })

    assert text =~ "junk.ex"
    assert text =~ ".hidden.ex"
  end

  test "reports truncation when max_results is exceeded", %{tmp_dir: tmp_dir} do
    text = search(%{path: tmp_dir, pattern: "**/*.ex", search_type: "glob", max_results: 1})

    assert text =~ "limit_reached"
    assert text =~ "truncated"
  end

  test "unknown search types are rejected", %{tmp_dir: tmp_dir} do
    {:reply, resp, _frame} =
      SearchFiles.execute(%{path: tmp_dir, pattern: "x", search_type: "files"}, %{})

    assert resp.isError == true
    text = Enum.find(resp.content, &(&1["type"] == "text"))["text"]
    assert text =~ "glob"
  end

  test "directories are included by default and suffixed with a slash", %{tmp_dir: tmp_dir} do
    entries = results(search(%{path: tmp_dir, pattern: "*", search_type: "glob"}))

    assert "sub/" in entries
    assert "a.ex" in entries
    assert "README.md" in entries
    refute "sub/deep/" in entries
    refute "node_modules/" in entries
    refute ".hidden.ex" in entries
  end

  test "** also matches nested directories", %{tmp_dir: tmp_dir} do
    entries = results(search(%{path: tmp_dir, pattern: "**", search_type: "glob"}))

    assert "sub/" in entries
    assert "sub/deep/" in entries
    assert "sub/deep/d.ex" in entries
    refute "node_modules/" in entries
  end

  test "a pattern ending in / returns directories only", %{tmp_dir: tmp_dir} do
    entries = results(search(%{path: tmp_dir, pattern: "sub/*/", search_type: "glob"}))

    assert entries == ["sub/deep/"]
  end

  test "include_dirs: false returns files only", %{tmp_dir: tmp_dir} do
    entries =
      results(search(%{path: tmp_dir, pattern: "*", search_type: "glob", include_dirs: false}))

    assert "a.ex" in entries
    refute "sub/" in entries
  end

  test "ignored directories appear only with include_ignored", %{tmp_dir: tmp_dir} do
    refute "node_modules/" in results(
             search(%{path: tmp_dir, pattern: "**", search_type: "glob"})
           )

    entries =
      results(search(%{path: tmp_dir, pattern: "**", search_type: "glob", include_ignored: true}))

    assert "node_modules/" in entries
  end
end
