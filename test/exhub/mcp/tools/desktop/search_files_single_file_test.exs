defmodule Exhub.MCP.Tools.Desktop.SearchFilesSingleFileTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Desktop.SearchFiles

  # Exile (used by the ripgrep/grep and probe paths) needs its supervisor running.
  setup_all do
    Application.ensure_all_started(:exile)
    :ok
  end

  setup do
    tmp_dir =
      System.tmp_dir!()
      |> Path.join("search_files_single_file_#{System.unique_integer([:positive])}")

    File.mkdir_p!(tmp_dir)

    fixture = Path.join(tmp_dir, "fixture.txt")
    File.write!(fixture, "line 1\nTARGET one\nline 3\n")

    other = Path.join(tmp_dir, "other.txt")
    File.write!(other, "TARGET two\n")

    on_exit(fn -> File.rm_rf!(tmp_dir) end)
    {:ok, tmp_dir: tmp_dir, fixture: fixture, other: other}
  end

  defp text(resp) do
    assert resp.isError == false
    Enum.find(resp.content, &(&1["type"] == "text"))["text"]
  end

  test "content search accepts a single file path and returns only its matches", %{
    fixture: fixture,
    other: other
  } do
    {:reply, resp, _frame} =
      SearchFiles.execute(%{path: fixture, pattern: "TARGET", search_type: "content"}, %{})

    body = text(resp)
    assert body =~ "TARGET one"
    assert body =~ "fixture.txt"
    refute body =~ other
  end

  test "glob search accepts a single file path and matches by basename", %{fixture: fixture} do
    {:reply, resp, _frame} =
      SearchFiles.execute(%{path: fixture, pattern: "*.txt", search_type: "glob"}, %{})

    body = text(resp)
    assert body =~ "fixture.txt"
    assert body =~ "count: 1"
  end

  test "glob search on a file returns nothing when the pattern does not match", %{
    fixture: fixture
  } do
    {:reply, resp, _frame} =
      SearchFiles.execute(%{path: fixture, pattern: "*.go", search_type: "glob"}, %{})

    assert text(resp) =~ "count: 0"
  end

  test "semantic search accepts a single file path", %{fixture: fixture} do
    {:reply, resp, _frame} = SearchFiles.execute(%{path: fixture, query: "TARGET"}, %{})
    assert resp.isError == false
  end

  test "missing paths still report not found" do
    {:reply, resp, _frame} =
      SearchFiles.execute(
        %{path: "/nonexistent/definitely/missing.txt", pattern: "x", search_type: "content"},
        %{}
      )

    assert resp.isError == true
    body = Enum.find(resp.content, &(&1["type"] == "text"))["text"]
    assert body =~ "not found"
  end
end
