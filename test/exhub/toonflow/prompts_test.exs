defmodule Exhub.Toonflow.PromptsTest do
  use ExUnit.Case, async: false

  alias Exhub.Toonflow.{Prompts, Store}

  describe "render_string/2" do
    test "substitutes placeholders" do
      assert Prompts.render_string("你好 {{name}}，{{missing}}", %{"name" => "世界"}) ==
               "你好 世界，{{missing}}"
    end

    test "stringifies non-binary values" do
      assert Prompts.render_string("n={{n}}", %{"n" => 3}) == "n=3"
    end
  end

  describe "render/2" do
    test "loads and renders a shipped template" do
      assert {:ok, text} = Prompts.render("events.user", %{"title" => "第一章", "text" => "正文"})
      assert text =~ "第一章"
      assert text =~ "正文"
    end

    test "errors on a missing template" do
      assert {:error, :enoent} = Prompts.render("does_not_exist", %{})
    end
  end

  describe "overrides" do
    setup do
      root =
        Path.join(System.tmp_dir!(), "toonflow_prompts_#{System.unique_integer([:positive])}")

      server = :"toonflow_prompts_store_#{System.unique_integer([:positive])}"
      {:ok, pid} = Store.start_link(root_dir: root, name: server)
      assert {:ok, _} = Store.create_project("demo", [], server)

      on_exit(fn ->
        if Process.alive?(pid), do: GenServer.stop(pid)
        File.rm_rf(root)
      end)

      %{server: server}
    end

    test "current/3 reports the shipped template when there is no override", %{server: server} do
      assert {:ok, current} = Prompts.current("events.user", "demo", server)
      assert current["name"] == "events.user"
      assert current["source"] == "shipped"
      assert current["version"] == nil
      assert current["content"] =~ "{{title}}"
    end

    test "resolve/4 falls back to the shipped template", %{server: server} do
      assert {:ok, text} =
               Prompts.resolve(
                 "events.user",
                 %{"title" => "第一章", "text" => "正文"},
                 "demo",
                 server
               )

      assert text =~ "第一章"
      assert text =~ "正文"
    end

    test "resolve/4 renders the latest override", %{server: server} do
      assert {:ok, %{"version" => 1}} =
               Prompts.set("demo", [name: "events.user", content: "覆盖：{{title}}"], server)

      assert {:ok, current} = Prompts.current("events.user", "demo", server)
      assert current["source"] == "override"
      assert current["version"] == 1

      assert {:ok, "覆盖：第一章"} =
               Prompts.resolve("events.user", %{"title" => "第一章"}, "demo", server)
    end

    test "set/3 versions overrides and history/3 lists them newest first", %{server: server} do
      assert {:ok, %{"version" => 1}} =
               Prompts.set("demo", [name: "events.user", content: "v1"], server)

      assert {:ok, %{"version" => 2}} =
               Prompts.set("demo", [name: "events.user", content: "v2", note: "second"], server)

      assert {:ok, [latest, first]} = Prompts.history("events.user", "demo", server)
      assert latest["version"] == 2
      assert latest["content"] == "v2"
      assert latest["note"] == "second"
      assert first["version"] == 1
      assert first["content"] == "v1"

      assert {:ok, "v2"} = Prompts.resolve("events.user", %{}, "demo", server)
    end

    test "revert/3 restores an earlier version as a new one", %{server: server} do
      assert {:ok, %{"version" => 1}} =
               Prompts.set("demo", [name: "events.user", content: "原始"], server)

      assert {:ok, %{"version" => 2}} =
               Prompts.set("demo", [name: "events.user", content: "改动"], server)

      assert {:ok, reverted} = Prompts.revert("demo", [name: "events.user", version: 1], server)
      assert reverted["version"] == 3
      assert reverted["content"] == "原始"
      assert reverted["note"] =~ "revert"

      assert {:ok, "原始"} = Prompts.resolve("events.user", %{}, "demo", server)

      assert {:error, :version_not_found} =
               Prompts.revert("demo", [name: "events.user", version: 99], server)
    end

    test "list/2 summarizes the overridden templates", %{server: server} do
      assert {:ok, %{"version" => 1}} =
               Prompts.set("demo", [name: "storyboard.user", content: "a"], server)

      assert {:ok, %{"version" => 2}} =
               Prompts.set("demo", [name: "storyboard.user", content: "b"], server)

      assert {:ok, %{"version" => 1}} =
               Prompts.set("demo", [name: "custom.only", content: "c"], server)

      assert {:ok, [custom, storyboard]} = Prompts.list("demo", server)
      assert custom["name"] == "custom.only"
      assert custom["versions"] == 1
      assert custom["shipped"] == false

      assert storyboard["name"] == "storyboard.user"
      assert storyboard["version"] == 2
      assert storyboard["versions"] == 2
      assert storyboard["shipped"] == true
    end

    test "set/3 validates its input", %{server: server} do
      assert {:error, :missing_name} = Prompts.set("demo", [content: "x"], server)
      assert {:error, :missing_content} = Prompts.set("demo", [name: "events.user"], server)
      assert {:error, :missing_name} = Prompts.history("", "demo", server)
    end

    test "resolve/4 ignores overrides when they are disabled in config", %{server: server} do
      previous = Application.get_env(:exhub, :toonflow)

      on_exit(fn ->
        if previous,
          do: Application.put_env(:exhub, :toonflow, previous),
          else: Application.delete_env(:exhub, :toonflow)
      end)

      assert {:ok, _} = Prompts.set("demo", [name: "events.user", content: "override"], server)
      assert {:ok, "override"} = Prompts.resolve("events.user", %{}, "demo", server)

      Application.put_env(:exhub, :toonflow, %{"prompts" => %{"overrides" => false}})
      refute Prompts.overrides_enabled?()

      assert {:ok, text} =
               Prompts.resolve(
                 "events.user",
                 %{"title" => "第一章", "text" => "正文"},
                 "demo",
                 server
               )

      assert text =~ "第一章"
      refute text =~ "override"
      # `current/3` still reports the stored override — only `resolve/4` gates.
      assert {:ok, %{"source" => "override"}} = Prompts.current("events.user", "demo", server)
    end
  end
end
