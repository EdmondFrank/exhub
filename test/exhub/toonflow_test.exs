defmodule Exhub.ToonflowTest do
  use ExUnit.Case, async: true

  alias Exhub.Toonflow

  describe "json_safe/1" do
    test "passes JSON-native values through untouched" do
      assert Toonflow.json_safe("ok") == "ok"
      assert Toonflow.json_safe(:ok) == :ok
      assert Toonflow.json_safe(3) == 3
      assert Toonflow.json_safe(1.5) == 1.5
      assert Toonflow.json_safe(true) == true
      assert Toonflow.json_safe(nil) == nil
    end

    test "inspects tuples — JSON.Encoder implements no Tuple" do
      assert Toonflow.json_safe({:image_failed, :boom}) == "{:image_failed, :boom}"
      assert_raise Protocol.UndefinedError, fn -> JSON.encode!({:image_failed, :boom}) end
    end

    test "inspects structs — JSON.Encoder implements nothing for them" do
      err = struct(LangChain.LangChainError, type: "unknown_error", message: "timeout")

      assert Toonflow.json_safe(err) == inspect(err)
      assert_raise Protocol.UndefinedError, fn -> JSON.encode!(err) end
    end

    test "recurses into lists and maps" do
      assert Toonflow.json_safe([{:a, 1}, "b", 2]) == ["{:a, 1}", "b", 2]
      assert Toonflow.json_safe(%{"reason" => {:a, 1}}) == %{"reason" => "{:a, 1}"}
    end

    test "inspects non-atom/binary map keys" do
      assert Toonflow.json_safe(%{{:a, :b} => %{c: 1}}) == %{"{:a, :b}" => %{c: 1}}
    end

    test "the result always encodes" do
      payload = %{
        "stages" => [
          %{
            "stage" => "images",
            "detail" => %{"errors" => [%{"reason" => {:image_failed, :boom}}]}
          }
        ],
        "reason" => {:llm_failed, struct(LangChain.LangChainError, type: "x", message: "y")}
      }

      assert is_binary(JSON.encode!(Toonflow.json_safe(payload)))
    end
  end

  describe "file_key/1" do
    test "leaves an already-safe key untouched" do
      assert Toonflow.file_key("sht_9dd1b4483ff6_start") == "sht_9dd1b4483ff6_start"
      assert Toonflow.file_key("ref_character_naruto") == "ref_character_naruto"
    end

    test "disambiguates names that sanitize to the same string" do
      lao = Toonflow.file_key("ref_character_老盐")
      ling = Toonflow.file_key("ref_character_小铃")

      # Both used to collapse to `ref_character___`, so the second reference
      # sheet overwrote the first.
      refute lao == ling
      assert lao =~ ~r/^ref_character_+_[0-9a-f]{6}$/
      assert ling =~ ~r/^ref_character_+_[0-9a-f]{6}$/
    end

    test "is stable for the same input" do
      assert Toonflow.file_key("小铃") == Toonflow.file_key("小铃")
      assert Toonflow.file_key(:atom_key) == "atom_key"
    end
  end
end
