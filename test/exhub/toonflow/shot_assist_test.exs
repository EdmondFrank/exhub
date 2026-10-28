defmodule Exhub.Toonflow.ShotAssistTest do
  use ExUnit.Case, async: false

  alias Exhub.Toonflow.{Config, DB, Schema, ShotAssist, Store}

  setup do
    root = Path.join(System.tmp_dir!(), "toonflow_assist_#{System.unique_integer([:positive])}")
    server = :"toonflow_assist_store_#{System.unique_integer([:positive])}"
    {:ok, pid} = Store.start_link(root_dir: root, name: server)
    assert {:ok, _} = Store.create_project("demo", [], server)

    previous_llm = Application.get_env(:exhub, :toonflow_llm)
    previous_reply = Application.get_env(:exhub, :shot_assist_reply)
    Application.put_env(:exhub, :toonflow_llm, __MODULE__.LLMStub)
    Application.put_env(:exhub, :shot_assist_reply, "润色后的画面")

    on_exit(fn ->
      if Process.alive?(pid), do: GenServer.stop(pid)
      File.rm_rf(root)
      restore(:toonflow_llm, previous_llm)
      restore(:shot_assist_reply, previous_reply)
    end)

    %{server: server}
  end

  describe "fields and actions" do
    test "only prose fields are offered to the assistant" do
      assert "description" in ShotAssist.fields()
      assert "dialogue" in ShotAssist.fields()
      assert "prompt" in ShotAssist.fields()
      assert "keyframe_end_prompt" in ShotAssist.fields()

      refute "size" in ShotAssist.fields()
      refute "motion" in ShotAssist.fields()
      refute "idx" in ShotAssist.fields()
    end

    test "actions carry the labels the prompt uses" do
      assert ShotAssist.actions() == ~w(polish continue expand shorten)
      assert ShotAssist.action_label("polish") == "润色"
      assert ShotAssist.action_label("continue") == "续写"
    end
  end

  describe "assist/4" do
    test "runs on the configured director model with the shot's context", %{server: server} do
      insert_shot(server, 1, %{"description" => "林昭推门"})
      target = insert_shot(server, 2, %{"description" => "林昭立在门前"})
      insert_shot(server, 3, %{"description" => "族长发怒"})

      assert {:ok, result} =
               ShotAssist.assist(
                 "demo",
                 target,
                 [field: "description", action: "polish", text: "林昭站在门口"],
                 server
               )

      model = Config.agent_model("director")
      assert result["model"] == model
      assert result["field"] == "description"
      assert result["action"] == "polish"
      assert result["text"] == "润色后的画面"

      assert_received {:llm_called, system, user, opts}
      assert opts[:model] == model
      assert system =~ "导演"
      assert user =~ "林昭站在门口"
      assert user =~ "画面描述"
      assert user =~ "润色"
      assert user =~ "前一个镜头"
      assert user =~ "后一个镜头"
      assert user =~ "林昭推门"
      assert user =~ "族长发怒"
    end

    test "continue strips a code fence and an echoed original", %{server: server} do
      Application.put_env(:exhub, :shot_assist_reply, "```text\n林昭推门而入。\n门外传来脚步声。\n```")
      target = insert_shot(server, 1, %{"description" => "林昭推门"})

      assert {:ok, result} =
               ShotAssist.assist(
                 "demo",
                 target,
                 [field: "description", action: "continue", text: "林昭推门而入。"],
                 server
               )

      assert result["text"] == "门外传来脚步声。"
      assert_received {:llm_called, _system, user, _opts}
      assert user =~ "续写"
      assert user =~ "不要重复原文"
    end

    test "defaults the action to polish and passes optional instructions", %{server: server} do
      target = insert_shot(server, 1)

      assert {:ok, result} =
               ShotAssist.assist(
                 "demo",
                 target,
                 [field: "prompt", text: "少年立在门前", instruction: "更冷峻一些"],
                 server
               )

      assert result["action"] == "polish"
      assert_received {:llm_called, _system, user, _opts}
      assert user =~ "更冷峻一些"
    end

    test "validates its target and input", %{server: server} do
      target = insert_shot(server, 1)

      assert {:error, :missing_field} = ShotAssist.assist("demo", target, [text: "x"], server)

      assert {:error, {:invalid_field, "idx"}} =
               ShotAssist.assist("demo", target, [field: "idx", text: "x"], server)

      assert {:error, {:invalid_action, "rewrite"}} =
               ShotAssist.assist(
                 "demo",
                 target,
                 [field: "description", action: "rewrite", text: "x"],
                 server
               )

      assert {:error, :missing_text} =
               ShotAssist.assist("demo", target, [field: "description", text: "   "], server)

      assert {:error, :shot_not_found} =
               ShotAssist.assist("demo", "missing", [field: "description", text: "x"], server)
    end
  end

  defp insert_shot(server, idx, attrs \\ %{}) do
    id = "sht_#{idx}"

    meta =
      attrs
      |> Map.get("meta", %{})
      |> Map.put("characters", ["林昭"])
      |> Map.put("dialogue", attrs["dialogue"] || "我不走")
      |> Map.put("speaker", "林昭")
      |> Map.put("keyframe_start_prompt", "开门首帧")

    {:ok, _} =
      Store.run_project(
        "demo",
        fn conn ->
          DB.execute(
            conn,
            """
            INSERT INTO shots (id, script_id, idx, scene, shot_desc, size, camera, lighting, motion, prompt, meta_json)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            [
              id,
              "scr_1",
              idx,
              attrs["scene"] || "祖宅",
              attrs["description"] || "林昭推门而入",
              "中景",
              nil,
              "冷光",
              "推",
              "青衫少年立于门前",
              Schema.encode_json(meta)
            ]
          )

          {:ok, id}
        end,
        server
      )

    id
  end

  defp restore(key, nil), do: Application.delete_env(:exhub, key)
  defp restore(key, value), do: Application.put_env(:exhub, key, value)
end

defmodule Exhub.Toonflow.ShotAssistTest.LLMStub do
  @moduledoc false
  @behaviour Exhub.Toonflow.LLM

  @impl true
  def call_llm(system, user, opts) do
    send(self(), {:llm_called, system, user, opts})
    {:ok, Application.get_env(:exhub, :shot_assist_reply, "ok")}
  end
end
