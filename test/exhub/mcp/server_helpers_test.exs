defmodule Exhub.MCP.ServerHelpersTest do
  use ExUnit.Case, async: true

  alias Anubis.Server.Context
  alias Anubis.Server.Frame
  alias Exhub.MCP.ServerHelpers

  defp frame(headers) when is_map(headers) do
    %Frame{context: %Context{headers: headers}}
  end

  describe "request_cwd/1" do
    test "returns the project directory from x-project-dir" do
      dir = Path.expand(System.tmp_dir!())

      assert {:ok, cwd} = ServerHelpers.request_cwd(frame(%{"x-project-dir" => dir}))
      assert cwd == dir
    end

    test "matches the header name case-insensitively and trims it" do
      dir = Path.expand(System.tmp_dir!())

      assert {:ok, ^dir} = ServerHelpers.request_cwd(frame(%{"X-Project-Dir" => " #{dir} "}))
    end

    test "returns :error without the header" do
      assert :error = ServerHelpers.request_cwd(frame(%{}))
      assert :error = ServerHelpers.request_cwd(nil)
      assert :error = ServerHelpers.request_cwd(%Frame{})
    end

    test "returns :error for a blank header" do
      assert :error = ServerHelpers.request_cwd(frame(%{"x-project-dir" => "   "}))
    end

    test "returns :error for a relative path" do
      assert :error = ServerHelpers.request_cwd(frame(%{"x-project-dir" => "lib/exhub"}))
    end

    test "returns :error when the directory does not exist" do
      missing =
        Path.join(System.tmp_dir!(), "exhub-does-not-exist-#{System.unique_integer([:positive])}")

      assert :error = ServerHelpers.request_cwd(frame(%{"x-project-dir" => missing}))
    end

    test "returns :error when the path is a file, not a directory" do
      file = Path.join(System.tmp_dir!(), "exhub-file-#{System.unique_integer([:positive])}")
      File.write!(file, "x")
      on_exit(fn -> File.rm(file) end)

      assert :error = ServerHelpers.request_cwd(frame(%{"x-project-dir" => file}))
    end
  end

  describe "request_headers/1" do
    test "returns the frame headers" do
      assert %{"x-project-dir" => "/tmp"} =
               ServerHelpers.request_headers(frame(%{"x-project-dir" => "/tmp"}))
    end

    test "returns an empty map when there are none" do
      assert %{} = ServerHelpers.request_headers(nil)
      assert %{} = ServerHelpers.request_headers(%Frame{})
    end
  end
end
