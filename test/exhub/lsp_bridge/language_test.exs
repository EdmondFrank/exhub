defmodule Exhub.LspBridge.LanguageTest do
  use ExUnit.Case, async: true

  alias Exhub.LspBridge.Language

  describe "language_for/1" do
    test "maps common extensions" do
      assert Language.language_for("src/main.zig") == "zig"
      assert Language.language_for("src/store.zig") == "zig"
      assert Language.language_for("lib/app.ex") == "elixir"
      assert Language.language_for("main.go") == "go"
      assert Language.language_for("lib.rs") == "rust"
      assert Language.language_for("app.py") == "python"
    end

    test "is case-insensitive on the extension" do
      assert Language.language_for("Main.ZIG") == "zig"
      assert Language.language_for("App.TSX") == "typescriptreact"
    end

    test "maps C sources to the c++ server, which is what clangd declares" do
      assert Language.language_for("foo.c") == "c++"
      assert Language.language_for("foo.h") == "c++"
    end

    test "maps well-known file names" do
      assert Language.language_for("Dockerfile") == "Dockerfiles"
      assert Language.language_for("CMakeLists.txt") == "cmake"
    end

    test "returns nil for files no configured server handles" do
      assert Language.language_for("notes.txt") == nil
      assert Language.language_for("data.bin") == nil
    end

    test "supported?/1 mirrors language_for/1" do
      assert Language.supported?("a.zig")
      refute Language.supported?("a.txt")
    end
  end

  describe "preferred/1 and candidates/1" do
    test "preferred server is the one a headless caller should try first" do
      assert Language.preferred("zig") == "zls"
      assert Language.preferred("elixir") == "elixirLS"
      assert Language.preferred("typescript") == "typescript"
      assert Language.preferred("unknown") == nil
    end

    test "candidates put the preferred server first" do
      candidates = Language.candidates("zig")

      assert [%Exhub.LspBridge.Config{name: "zls"} | _] = candidates
      assert Enum.all?(candidates, &(&1.language_id == "zig"))
    end

    test "candidates skip servers that cannot run on this platform" do
      candidates = Language.candidates("java") |> Enum.map(& &1.name)

      case :os.type() do
        {:win32, _} -> refute Enum.any?(candidates, &String.ends_with?(&1, "_darwin"))
        _ -> refute Enum.any?(candidates, &String.ends_with?(&1, "_windows"))
      end
    end

    test "candidates is empty for an unconfigured language" do
      assert Language.candidates("no-such-language") == []
    end
  end

  describe "project_files/1" do
    test "build markers for languages used by headless root discovery" do
      assert "build.zig" in Language.project_files("zig")
      assert "mix.exs" in Language.project_files("elixir")
      assert "Cargo.toml" in Language.project_files("rust")
      assert "go.mod" in Language.project_files("go")
      assert Language.project_files("unknown") == []
    end
  end
end
