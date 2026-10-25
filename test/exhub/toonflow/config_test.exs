defmodule Exhub.Toonflow.ConfigTest do
  use ExUnit.Case, async: false

  alias Exhub.Toonflow.Config

  setup do
    previous = Application.get_env(:exhub, :toonflow)

    on_exit(fn ->
      if previous,
        do: Application.put_env(:exhub, :toonflow, previous),
        else: Application.delete_env(:exhub, :toonflow)
    end)

    :ok
  end

  describe "voice_map/0" do
    test "stringifies keys and nested spec keys, defaulting to empty" do
      Application.put_env(:exhub, :toonflow, %{})
      assert Config.voice_map() == %{}

      Application.put_env(:exhub, :toonflow, %{
        "media" => %{"voice_map" => %{"老盐" => "shanghainv", 鸣人: %{voice: "lucas"}}}
      })

      assert Config.voice_map() == %{
               "鸣人" => %{"voice" => "lucas"},
               "老盐" => "shanghainv"
             }
    end
  end

  describe "voice_spec/2" do
    test "accepts a voice name or a spec map, and misses return %{}" do
      voice_map = %{
        "a" => "lucas",
        "b" => %{"voice" => "shanghainv", "speech_rate" => 0.8}
      }

      assert Config.voice_spec(voice_map, "a") == %{"voice" => "lucas"}
      assert Config.voice_spec(voice_map, "b")["speech_rate"] == 0.8
      assert Config.voice_spec(voice_map, "missing") == %{}
      assert Config.voice_spec(voice_map, nil) == %{}
      assert Config.voice_spec(nil, "a") == %{}
    end
  end

  describe "tts_narration?/0" do
    test "is false by default and true only when explicitly enabled" do
      Application.put_env(:exhub, :toonflow, %{})
      refute Config.tts_narration?()

      Application.put_env(:exhub, :toonflow, %{"media" => %{"tts_narration" => true}})
      assert Config.tts_narration?()

      Application.put_env(:exhub, :toonflow, %{"media" => %{"tts_narration" => false}})
      refute Config.tts_narration?()
    end
  end
end
