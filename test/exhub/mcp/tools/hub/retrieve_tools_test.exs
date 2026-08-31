defmodule Exhub.MCP.Tools.Hub.RetrieveToolsTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Hub.RetrieveTools

  describe "compact_params/1" do
    test "summarizes types and required markers" do
      schema = %{
        "type" => "object",
        "required" => ["query"],
        "properties" => %{
          "query" => %{"type" => "string", "description" => "search terms"},
          "limit" => %{"type" => "integer", "default" => 5}
        }
      }

      assert RetrieveTools.compact_params(schema) ==
               "limit: integer, query: string (required)"
    end

    test "handles array-typed properties" do
      schema = %{
        "properties" => %{"paths" => %{"type" => ["string", "null"]}}
      }

      assert RetrieveTools.compact_params(schema) == "paths: string"
    end

    test "returns nil for tools without properties" do
      assert RetrieveTools.compact_params(%{"type" => "object"}) == nil
      assert RetrieveTools.compact_params(nil) == nil
    end

    test "returns nil for empty properties map" do
      assert RetrieveTools.compact_params(%{"properties" => %{}}) == nil
    end
  end
end
