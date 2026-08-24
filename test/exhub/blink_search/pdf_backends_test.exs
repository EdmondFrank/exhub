defmodule Exhub.BlinkSearch.Backends.PDFBackendsTest do
  use ExUnit.Case, async: true

  # Exhub.send_message/1 dispatches over Exhub.Registry; under --no-start we
  # start a bare registry so dispatch is a no-op instead of raising.
  # start_supervised! scopes it to each test, avoiding async races.
  setup do
    start_supervised!({Registry, keys: :unique, name: Exhub.Registry})
    :ok
  end

  describe "GrepPDF.select/2 with plain-string candidates" do
    test "does not crash when candidate is a string (regression)" do
      state = %{
        search_paths: ["/tmp/docs"],
        match_text: "needle"
      }

      assert :ok =
               Exhub.BlinkSearch.Backends.GrepPDF.select("report.pdf:12:5: some text", state)
    end

    test "prefers match_text from map candidates, falls back to state" do
      assert :ok =
               Exhub.BlinkSearch.Backends.GrepPDF.do_action(
                 %{text: "report.pdf:12:5: body", match_text: "from-candidate"},
                 %{search_paths: ["/tmp/docs"], match_text: "from-state"}
               )
    end
  end

  describe "PDF.select/2 with plain-string candidates" do
    test "does not crash when candidate is a string (regression)" do
      state = %{search_paths: ["/tmp/report.pdf"], match_text: "needle"}

      assert :ok = Exhub.BlinkSearch.Backends.PDF.select("12:5: some text", state)
    end
  end

  describe "Backend.parse_rg_line/2" do
    test "handles match lines without a lines field" do
      line =
        Jason.encode!(%{
          "type" => "match",
          "data" => %{
            "path" => %{"text" => "/tmp/a.ex"},
            "line_number" => 3,
            "submatches" => [%{"match" => %{"text" => "foo"}, "start" => 0, "end" => 3}]
          }
        })

      result = Exhub.BlinkSearch.Backend.parse_rg_line(line, "/tmp")

      assert %{text: text, matches: matches} = result
      assert text == "a.ex:3:0: "
      # prefix "a.ex:3:0: " is 10 bytes; submatch spans bytes 0..3
      assert matches == [[10, 13]]
    end

    test "returns nil for non-match lines" do
      line = Jason.encode!(%{"type" => "begin", "data" => %{}})
      assert Exhub.BlinkSearch.Backend.parse_rg_line(line, nil) == nil
    end
  end
end
