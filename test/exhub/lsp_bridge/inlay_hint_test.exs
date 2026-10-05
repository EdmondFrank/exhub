defmodule Exhub.LspBridge.InlayHintTest do
  use ExUnit.Case, async: true

  alias Exhub.LspBridge.Handlers.InlayHint

  @ctx %{path: "/proj/lib/a.ex", language_id: "elixir", args: %{}}

  describe "InlayHint" do
    test "metadata" do
      assert InlayHint.name() == "inlay-hint"
      assert InlayHint.method() == "textDocument/inlayHint"
      assert InlayHint.provider() == "inlay_hint"
      assert InlayHint.cancel_on_change?()
    end

    test "request params carry the range, defaulting to an empty map" do
      range = %{
        "start" => %{"line" => 0, "character" => 0},
        "end" => %{"line" => 9, "character" => 0}
      }

      assert InlayHint.request_params(%{"range" => range}, @ctx) == %{"range" => range}
      assert InlayHint.request_params(%{}, @ctx) == %{}
    end

    test "hints pass through as an inlay_hints payload" do
      hints = [%{"position" => %{"line" => 1, "character" => 3}, "label" => ":ok"}]

      assert InlayHint.process_response(hints, @ctx) == {:inlay_hints, "/proj/lib/a.ex", hints}
    end

    test "an absent result clears overlays with an empty list" do
      assert InlayHint.process_response(nil, @ctx) == {:inlay_hints, "/proj/lib/a.ex", []}
    end
  end
end
