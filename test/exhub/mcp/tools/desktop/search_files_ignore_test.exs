defmodule Exhub.MCP.Tools.Desktop.SearchFilesIgnoreTest do
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
      |> Path.join("search_files_ignore_#{System.unique_integer([:positive])}")

    File.mkdir_p!(tmp_dir)
    File.write!(Path.join(tmp_dir, "keep.txt"), "MATCH keep\n")

    # `.ignore` is honored by ripgrep without requiring a git repository.
    File.write!(Path.join(tmp_dir, "ignored.txt"), "MATCH ignored\n")
    File.write!(Path.join(tmp_dir, ".ignore"), "ignored.txt\n")

    on_exit(fn -> File.rm_rf!(tmp_dir) end)
    {:ok, tmp_dir: tmp_dir}
  end

  defp search(params) do
    {:reply, resp, _frame} = SearchFiles.execute(params, %{})
    assert resp.isError == false
    Enum.find(resp.content, &(&1["type"] == "text"))["text"]
  end

  test "content + file_pattern does not resurface ignored files", %{tmp_dir: tmp_dir} do
    text =
      search(%{
        path: tmp_dir,
        pattern: "MATCH",
        search_type: "content",
        file_pattern: "*.txt"
      })

    assert text =~ "keep.txt"
    refute text =~ "ignored.txt"
  end

  test "content + include_ignored: true does surface ignored files", %{tmp_dir: tmp_dir} do
    text =
      search(%{
        path: tmp_dir,
        pattern: "MATCH",
        search_type: "content",
        file_pattern: "*.txt",
        include_ignored: true
      })

    assert text =~ "keep.txt"
    assert text =~ "ignored.txt"
  end

  test "glob mode respects ignore rules", %{tmp_dir: tmp_dir} do
    text = search(%{path: tmp_dir, pattern: "*.txt", search_type: "glob"})

    assert text =~ "keep.txt"
    refute text =~ "ignored.txt"
  end

  test "a pattern starting with a dash is matched literally", %{tmp_dir: tmp_dir} do
    File.write!(Path.join(tmp_dir, "dash.txt"), "-i literal\n")

    text = search(%{path: tmp_dir, pattern: "-i", search_type: "content", file_pattern: "*.txt"})

    assert text =~ "literal"
  end
end
