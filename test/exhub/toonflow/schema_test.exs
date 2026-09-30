defmodule Exhub.Toonflow.SchemaTest do
  use ExUnit.Case, async: true

  alias Exhub.Toonflow.Schema

  test "all_ddl/0 covers every core table" do
    sql = Enum.join(Schema.all_ddl(), "\n")

    for table <-
          ~w(projects novels chapters events scripts characters shots assets jobs memory_notes) do
      assert sql =~ "CREATE TABLE IF NOT EXISTS #{table}"
    end
  end

  test "registry_ddl/0 only defines the projects table" do
    sql = Enum.join(Schema.registry_ddl(), "\n")
    assert sql =~ "CREATE TABLE IF NOT EXISTS projects"
    refute sql =~ "CREATE TABLE IF NOT EXISTS novels"
  end

  test "project_ddl/0 excludes the registry-only projects table" do
    sql = Enum.join(Schema.project_ddl(), "\n")
    assert sql =~ "CREATE TABLE IF NOT EXISTS novels"
    refute sql =~ "CREATE TABLE IF NOT EXISTS projects"
  end

  test "decode_project/1 maps the column order" do
    row = [
      "prj_1",
      "demo",
      "/tmp/demo",
      ~s({"description":"x"}),
      "2026-01-01T00:00:00Z",
      "2026-01-01T00:00:00Z"
    ]

    decoded = Schema.decode_project(row)
    assert decoded["id"] == "prj_1"
    assert decoded["name"] == "demo"
    assert decoded["meta"] == %{"description" => "x"}
  end

  test "decode_json/1 tolerates nil, blank and invalid JSON" do
    assert Schema.decode_json(nil) == nil
    assert Schema.decode_json("") == nil
    assert Schema.decode_json("not json") == nil
    assert Schema.decode_json(~s({"a":1})) == %{"a" => 1}
  end

  test "encode_json/1 round-trips through decode_json/1" do
    value = %{"a" => [1, 2], "b" => "x"}
    assert Schema.decode_json(Schema.encode_json(value)) == value
    assert Schema.encode_json(nil) == nil
  end
end
