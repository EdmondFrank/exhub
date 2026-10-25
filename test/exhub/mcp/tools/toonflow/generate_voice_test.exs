defmodule Exhub.MCP.Tools.Toonflow.GenerateVoiceTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Toonflow.GenerateVoice

  describe "input_schema/0" do
    test "requires a project and declares the realtime tuning fields" do
      schema = GenerateVoice.input_schema()

      assert schema["required"] == ["project"]
      # `:number` is not a Peri type — an unrecognised type both drops the
      # JSON Schema `type` and fails validation, making the field unusable.
      assert schema["properties"]["speech_rate"]["type"] == "number"
      assert schema["properties"]["volume"]["type"] == "integer"
      assert schema["properties"]["sample_rate"]["type"] == "integer"
    end
  end
end
