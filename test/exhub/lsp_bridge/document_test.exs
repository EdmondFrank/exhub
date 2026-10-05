defmodule Exhub.LspBridge.DocumentTest do
  use ExUnit.Case, async: true

  alias Exhub.LspBridge.Document

  test "didOpen params use version 0 and the buffer content" do
    doc = Document.new("/tmp/a.ex", "abc", "elixir")
    params = Document.did_open_params(doc)

    assert params["textDocument"]["version"] == 0
    assert params["textDocument"]["text"] == "abc"
    assert params["textDocument"]["languageId"] == "elixir"
    assert params["textDocument"]["uri"] == "file:///tmp/a.ex"
  end

  test "didChange params carry range, rangeLength and text with the current version" do
    doc = Document.new("/tmp/a.ex", "abc", "elixir")

    change = %{
      "range" => %{
        "start" => %{"line" => 0, "character" => 1},
        "end" => %{"line" => 0, "character" => 1}
      },
      "rangeLength" => 0,
      "text" => "X"
    }

    params = Document.did_change_params(doc, change, 2)
    assert params["textDocument"]["version"] == 1
    assert [%{"range" => _, "rangeLength" => 0, "text" => "X"}] = params["contentChanges"]
  end

  test "full-sync servers receive the whole content" do
    doc = Document.new("/tmp/a.ex", "abc", "elixir")
    params = Document.did_change_params(doc, %{"text" => "X"}, 1)
    assert params["contentChanges"] == [%{"text" => "abc"}]
  end

  test "didSave includes text only when asked" do
    doc = Document.new("/tmp/a.ex", "abc", "elixir")
    refute Map.has_key?(Document.did_save_params(doc, false)["textDocument"], "text")
    assert Document.did_save_params(doc, true)["textDocument"]["text"] == "abc"
  end

  test "apply_change mirrors an incremental edit onto the cached content" do
    doc = Document.new("/tmp/a.ex", "hello\nworld", "elixir")

    change = %{
      "range" => %{
        "start" => %{"line" => 0, "character" => 0},
        "end" => %{"line" => 0, "character" => 5}
      },
      "text" => "HELLO"
    }

    assert Document.apply_change(doc, change).content == "HELLO\nworld"
  end

  test "apply_change counts UTF-16 code units (astral characters)" do
    doc = Document.new("/tmp/a.ex", "a😀b", "elixir")

    # The emoji is two UTF-16 code units wide.
    change = %{
      "range" => %{
        "start" => %{"line" => 0, "character" => 1},
        "end" => %{"line" => 0, "character" => 3}
      },
      "text" => "_"
    }

    assert Document.apply_change(doc, change).content == "a_b"
  end

  test "a change without a range replaces the whole content" do
    assert Document.apply_change_text("abc", %{"text" => "xyz"}) == "xyz"
  end
end
