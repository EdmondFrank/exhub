defmodule Exhub.BrowserAgent.SnapshotTest do
  use ExUnit.Case, async: true

  alias Exhub.BrowserAgent.Snapshot

  @compact """
  RootWebArea "Flights"
    combobox "Where from?" @e2 = San Francisco
    combobox "Where to?" @e3
    textbox "Departure" @e4
    link "Search" @e5
    heading "Results"
  """

  describe "parse/1" do
    test "parses the compact text tree into nodes with role, name, ref, and value" do
      [root, from, to, departure, search, heading] = Snapshot.parse(@compact)

      assert root.role == "RootWebArea"
      assert root.depth == 0

      assert from.role == "combobox"
      assert from.name == "Where from?"
      assert from.ref == "e2"
      assert from.value == "San Francisco"
      assert from.depth == 2

      assert to.name == "Where to?"
      assert to.value == nil
      assert departure.role == "textbox"
      assert search.role == "link"
      assert heading.name == "Results"
      assert heading.ref == nil
    end

    test "captures state and description when present" do
      [checkbox] =
        Snapshot.parse(~s(checkbox "Email me" @e7 [checked=false required] desc="Weekly updates"))

      assert checkbox.state == "checked=false required"
      assert checkbox.description == "Weekly updates"
    end

    test "decodes unicode escapes and tolerates a truncated trailing escape" do
      [a, b] = Snapshot.parse(~s(textbox "\\u5546\\u54c1" @e1\nbutton "\\u63d0\\u4ea4\\u5" @e2))

      assert a.name == "商品"
      assert b.name == "提交"
    end

    test "handles a ref without a name" do
      [node] = Snapshot.parse(~s(textbox @e9))
      assert node.role == "textbox"
      assert node.name == nil
      assert node.ref == "e9"
    end

    test "unwraps the browser-use helper JSON envelope" do
      envelope =
        Jason.encode!(%{"stdout" => ~s(link "Home" @e1), "stderr" => "", "exit_status" => 0})

      [node] = Snapshot.parse(envelope)
      assert node.role == "link"
      assert node.ref == "e1"
    end

    test "accepts a JSON array of nodes" do
      json = Jason.encode!([%{"ref" => "e1", "role" => "button", "name" => "Submit"}])
      [node] = Snapshot.parse(json)
      assert node.role == "button"
      assert node.name == "Submit"
    end
  end

  describe "action_space/1" do
    test "numbers reffed elements and offers per-operation target heads" do
      elements = Snapshot.parse(@compact)
      {indexed, targets} = Snapshot.action_space(elements)

      # heading has no ref, so it is excluded from the action space
      assert Enum.map(indexed, & &1.ref) == ["e2", "e3", "e4", "e5"]
      assert Enum.map(indexed, & &1.index) == [1, 2, 3, 4]

      assert Map.keys(targets["CLICK"]) == ["4"]
      assert Map.keys(targets["TYPE_TEXT"]) == ["1", "2", "3"]
      assert targets["CLICK"]["4"].label == "Search"
    end

    test "caps each target head at max_targets" do
      elements = for i <- 1..40, do: %{role: "link", name: "L#{i}", ref: "e#{i}"}
      {_indexed, targets} = Snapshot.action_space(elements, max_targets: 26)

      assert map_size(targets["CLICK"]) == 26

      # System One accepts at most 16 candidates per choice question.
      {_indexed, default} = Snapshot.action_space(elements)

      assert map_size(default["CLICK"]) == 16
    end

    test "offsets the target head so a deep candidate can be offered" do
      elements = for i <- 1..40, do: %{role: "link", name: "L#{i}", ref: "e#{i}"}

      {_indexed, targets} = Snapshot.action_space(elements, offset: 16)

      assert Map.keys(targets["CLICK"]) == Enum.map(17..32, &to_string/1)
      assert targets["CLICK"]["17"].ref == "e17"

      # Past the ceiling there is nothing left to offer.
      {_indexed, empty} = Snapshot.action_space(elements, offset: 40)

      refute Map.has_key?(empty, "CLICK")
    end

    test "reports the candidate ceiling of the widest target head" do
      assert Snapshot.candidate_ceiling([
               %{role: "link"},
               %{role: "link"},
               %{role: "textbox"},
               %{role: "heading"}
             ]) == 2

      assert Snapshot.candidate_ceiling([]) == 0
    end

    test "offers goal-relevant candidates first" do
      elements = for i <- 1..20, do: %{role: "link", name: "Link #{i}", ref: "e#{i}"}
      deep = %{role: "link", name: "ClassMethods#before_action", ref: "deep"}
      prefer = Snapshot.goal_terms("open the documentation for before_action")

      {_indexed, targets} = Snapshot.action_space(elements ++ [deep], prefer: prefer)

      # Ranked first, so it displaces the document-order sixteenth candidate.
      assert Map.has_key?(targets["CLICK"], "21")
      assert targets["CLICK"]["21"].ref == "deep"
      refute Map.has_key?(targets["CLICK"], "16")
    end

    test "goal_terms keeps significant tokens and drops boilerplate" do
      terms =
        Snapshot.goal_terms(
          "Open the documentation for ActionCable::Channel::Streams#stream_from " <>
            "in the sidebar and report its signature"
        )

      assert "stream_from" in terms
      assert "actioncable" in terms
      assert "channel" in terms
      assert "streams" in terms

      refute "open" in terms
      refute "documentation" in terms
      refute "the" in terms
      refute "sidebar" in terms

      assert Snapshot.goal_terms("open the documentation") == []
    end
  end

  describe "render/1" do
    test "renders a numbered table" do
      {indexed, _targets} = Snapshot.parse(@compact) |> Snapshot.action_space()

      assert Snapshot.render(indexed) =~ ~s([1] combobox "Where from?" = San Francisco)
      assert Snapshot.render(indexed) =~ ~s([4] link "Search")
    end
  end
end
