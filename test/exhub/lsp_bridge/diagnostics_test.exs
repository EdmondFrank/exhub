defmodule Exhub.LspBridge.DiagnosticsTest do
  use ExUnit.Case, async: true

  alias Exhub.LspBridge.{Diagnostics, Document}

  defp doc, do: Document.new("/tmp/a.ex", "", "elixir")

  defp diag(line, message, severity \\ 1) do
    %{
      "severity" => severity,
      "message" => message,
      "range" => %{
        "start" => %{"line" => line, "character" => 0},
        "end" => %{"line" => line, "character" => 1}
      }
    }
  end

  test "records per server, sorts by range, and merges with a server-name tag" do
    doc =
      doc()
      |> Diagnostics.record("s1", [diag(5, "b", 2), diag(1, "a", 1)])
      |> Diagnostics.record("s2", [diag(3, "c", 1)])

    assert Diagnostics.count(doc) == 3

    merged = Diagnostics.merge(doc)
    assert Enum.map(merged, & &1["message"]) == ["a", "c", "b"]
    assert Enum.map(merged, & &1["server-name"]) == ["s1", "s2", "s1"]
  end

  test "hide_severities and max" do
    doc =
      doc()
      |> Diagnostics.record("s1", [diag(0, "e", 1), diag(1, "i", 3)])

    assert Enum.map(Diagnostics.merge(doc, hide_severities: [3]), & &1["message"]) == ["e"]
    assert length(Diagnostics.merge(doc, max: 1)) == 1
  end

  test "nil hide_severities is treated as empty rather than raising" do
    doc = doc() |> Diagnostics.record("s1", [diag(0, "e", 1)])

    assert Enum.map(Diagnostics.merge(doc, hide_severities: nil), & &1["message"]) == ["e"]
  end

  test "pull params and result extraction" do
    assert Diagnostics.pull_params("id1", nil) == %{"identifier" => "id1"}
    assert Diagnostics.pull_params(nil, "v1") == %{"previousResultId" => "v1"}

    assert Diagnostics.from_pull_result(%{"items" => [%{"message" => "x"}]}) ==
             [%{"message" => "x"}]

    assert Diagnostics.from_pull_result(%{"kind" => "unchanged"}) == nil
  end
end
