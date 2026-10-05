defmodule Exhub.LspBridge.ConfigTest do
  # `async: false` — the loader uses one global named ETS table, so instances
  # must not overlap across tests (a test's instance is torn down before the
  # next starts, freeing the table).
  use ExUnit.Case, async: false

  alias Exhub.LspBridge.{Config, MultiServer}

  @priv to_string(:code.priv_dir(:exhub))

  describe "parse/1" do
    test "normalizes a single-command argv into {command, args}" do
      info =
        Config.parse(%{
          "name" => "pyright",
          "languageId" => "python",
          "command" => ["pyright-langserver", "--stdio"],
          "projectFiles" => ["pyproject.toml"],
          "settings" => %{"python.analysis" => %{"typeCheckingMode" => "basic"}}
        })

      assert info.name == "pyright"
      assert info.language_id == "python"
      assert info.command == "pyright-langserver"
      assert info.args == ["--stdio"]
      assert info.project_files == ["pyproject.toml"]
      assert info.settings["python.analysis"]["typeCheckingMode"] == "basic"
      assert info.support_single_file == true
    end

    test "honors support-single-file false" do
      info = Config.parse(%{"name" => "elixirLS", "support-single-file" => false})
      assert info.support_single_file == false
    end

    test "handles an empty command" do
      info = Config.parse(%{"name" => "x"})
      assert info.command == nil
      assert info.args == []
      assert Config.command_args(info) == nil
    end
  end

  describe "command_args/1" do
    test "joins command and args" do
      info = Config.parse(%{"command" => ["language_server.sh", "-v"]})
      assert Config.command_args(info) == ["language_server.sh", "-v"]
    end
  end

  describe "vendored configs" do
    setup do
      start_supervised!({Config, [name: :cfg_test]})
      :ok
    end

    test "for_name finds elixirLS" do
      assert {:ok, info} = Config.for_name("elixirLS")
      assert info.language_id == "elixir"
      assert "mix.exs" in info.project_files
      assert info.command == "language_server.sh"
    end

    test "for_language maps a language id to a server" do
      assert {:ok, info} = Config.for_language("elixir")
      assert info.language_id == "elixir"
    end

    test "all/0 exposes every langserver name" do
      assert Enum.any?(Config.all(), &(&1.name == "elixirLS"))
    end

    test "default dirs point into priv/lsp_bridge" do
      assert Config.default_langserver_dir() == Path.join([@priv, "lsp_bridge", "langserver"])
      assert Config.default_multiserver_dir() == Path.join([@priv, "lsp_bridge", "multiserver"])
      assert File.dir?(Config.default_langserver_dir())
      assert File.dir?(Config.default_multiserver_dir())
    end
  end

  describe "multiserver profiles" do
    setup do
      start_supervised!({Config, [name: :cfg_multi_test]})
      :ok
    end

    test "multi/1 loads a profile by filename" do
      assert {:ok, profile} = Config.multi("pyright_ruff")
      assert profile["default"] == "pyright"
      assert profile["formatting"] == "ruff"
    end

    test "multi_all/0 indexes profiles by name" do
      all = Config.multi_all()
      assert Map.has_key?(all, "pyright_ruff")
      assert Map.has_key?(all, "typescript_eslint")
    end

    test "multi_servers returns the ordered list for a feature" do
      assert ["pyright", "ruff"] = Config.multi_servers("pyright_ruff", "diagnostics")
      # single-string values normalize to a one-element list
      assert ["ruff"] = Config.multi_servers("pyright_ruff", "formatting")
    end

    test "multi_servers accepts a raw LSP method and normalizes it" do
      assert ["pyright", "ruff"] = Config.multi_servers("pyright_ruff", "textDocument/codeAction")
    end

    test "multi_servers falls back to default for an unmapped feature" do
      assert ["pyright"] = Config.multi_servers("pyright_ruff", "hover")
    end

    test "multi_servers returns [] for an unknown profile" do
      assert [] = Config.multi_servers("no_such_profile", "diagnostics")
    end
  end

  describe "MultiServer.all_servers/1" do
    test "de-duplicates the server names referenced by a profile" do
      profile = %{
        "default" => "pyright",
        "diagnostics" => ["pyright", "ruff"],
        "formatting" => "ruff"
      }

      assert Enum.sort(MultiServer.all_servers(profile)) == ["pyright", "ruff"]
    end
  end
end
