defmodule Exhub.MCP.TodoTest do
  use ExUnit.Case, async: false

  alias Exhub.MCP.TodoStore
  alias Exhub.MCP.Tools.TodoClearItems
  alias Exhub.MCP.Tools.TodoGetItems
  alias Exhub.MCP.Tools.TodoSetItems
  alias Exhub.MCP.Tools.TodoUpdateItem

  # Isolated ETS table so these tests stay pure (--no-start) and never touch the
  # app-supervised store.
  @table :todo_test_store

  setup_all do
    {:ok, pid} = TodoStore.start_link(name: TodoStore, table: @table)

    on_exit(fn ->
      if Process.alive?(pid), do: GenServer.stop(pid)
    end)

    :ok
  end

  defp tenant, do: "test-#{System.unique_integer([:positive])}"

  defp run(tool, params), do: apply(tool, :execute, [params, %{}])

  defp text(%Anubis.Server.Response{content: content}) do
    content
    |> Enum.find(&(Map.get(&1, "type") == "text"))
    |> Map.get("text")
  end

  defp json(resp), do: resp |> text() |> JSON.decode!()

  describe "TodoStore.update_items/3" do
    test "updates a single item by name" do
      id = tenant()
      :ok = TodoStore.set_todos(id, [%{name: "a", completed: false}], "prompt")

      assert {:ok, entry, []} = TodoStore.update_items(id, [{"a", true}])
      assert entry.items == [%{name: "a", completed: true}]
    end

    test "applies a batch in one atomic call" do
      id = tenant()
      :ok = TodoStore.set_todos(id, [%{name: "a"}, %{name: "b"}, %{name: "c"}], "")

      assert {:ok, entry, []} = TodoStore.update_items(id, [{"a", true}, {"c", true}])
      assert Enum.map(entry.items, & &1.completed) == [true, false, true]
    end

    test "reports names that are not in the list" do
      id = tenant()
      :ok = TodoStore.set_todos(id, [%{name: "a"}], "")

      assert {:ok, _entry, ["missing"]} =
               TodoStore.update_items(id, [{"a", true}, {"missing", true}])
    end

    test "returns not_found for an unknown tenant" do
      assert {:error, :not_found} = TodoStore.update_items(tenant(), [{"a", true}])
    end

    test "update_item/4 keeps working as a single-item wrapper" do
      id = tenant()
      :ok = TodoStore.set_todos(id, [%{name: "a"}], "")

      assert {:ok, entry} = TodoStore.update_item(id, "a", true)
      assert entry.items == [%{name: "a", completed: true}]
    end
  end

  describe "set_items / get_items" do
    test "round-trips the plan and the original prompt" do
      id = tenant()

      {:reply, set_resp, _} =
        run(TodoSetItems, %{
          tenant_id: id,
          items: [%{name: "step"}],
          initial_user_prompt: "do it"
        })

      assert json(set_resp)["count"] == 1

      {:reply, get_resp, _} = run(TodoGetItems, %{tenant_id: id})
      data = json(get_resp)
      assert data["initial_user_prompt"] == "do it"
      assert data["items"] == [%{"name" => "step", "completed" => false}]
    end

    test "returns an empty list (not an error) for an unknown tenant" do
      {:reply, resp, _} = run(TodoGetItems, %{tenant_id: tenant()})
      assert resp.isError == false
      assert json(resp)["items"] == []
    end
  end

  describe "update_item_completion" do
    test "single-item form updates the list" do
      id = tenant()
      run(TodoSetItems, %{tenant_id: id, items: [%{name: "a"}, %{name: "b"}]})

      {:reply, resp, _} = run(TodoUpdateItem, %{tenant_id: id, name: "a", completed: true})

      data = json(resp)
      assert data["updated"] == ["a"]

      assert data["items"] == [
               %{"name" => "a", "completed" => true},
               %{"name" => "b", "completed" => false}
             ]
    end

    test "batch form updates several items at once" do
      id = tenant()
      run(TodoSetItems, %{tenant_id: id, items: [%{name: "a"}, %{name: "b"}, %{name: "c"}]})

      {:reply, resp, _} =
        run(TodoUpdateItem, %{
          tenant_id: id,
          items: [%{name: "a", completed: true}, %{name: "c", completed: true}]
        })

      data = json(resp)
      assert data["updated"] == ["a", "c"]
      assert Enum.map(data["items"], & &1["completed"]) == [true, false, true]
    end

    test "accepts string-keyed batch items" do
      id = tenant()
      run(TodoSetItems, %{tenant_id: id, items: [%{name: "a"}]})

      {:reply, resp, _} =
        run(TodoUpdateItem, %{tenant_id: id, items: [%{"name" => "a", "completed" => true}]})

      assert json(resp)["updated"] == ["a"]
    end

    test "reports unknown item names" do
      id = tenant()
      run(TodoSetItems, %{tenant_id: id, items: [%{name: "a"}]})

      {:reply, resp, _} = run(TodoUpdateItem, %{tenant_id: id, name: "nope", completed: true})
      assert json(resp)["not_found"] == ["nope"]
    end

    test "errors when no update target is given" do
      {:reply, resp, _} = run(TodoUpdateItem, %{tenant_id: tenant()})
      assert resp.isError == true
      assert text(resp) =~ "Provide either"
    end

    test "errors when name is given without completed" do
      {:reply, resp, _} = run(TodoUpdateItem, %{tenant_id: tenant(), name: "a"})
      assert resp.isError == true
      assert text(resp) =~ "completed"
    end

    test "errors when a batch entry omits completed" do
      {:reply, resp, _} = run(TodoUpdateItem, %{tenant_id: tenant(), items: [%{name: "a"}]})
      assert resp.isError == true
      assert text(resp) =~ "boolean `completed`"
    end

    test "errors when a batch entry has no usable name" do
      {:reply, resp, _} =
        run(TodoUpdateItem, %{tenant_id: tenant(), items: [%{name: "", completed: true}]})

      assert resp.isError == true
      assert text(resp) =~ "non-empty string `name`"
    end

    test "errors when both call forms are supplied" do
      {:reply, resp, _} =
        run(TodoUpdateItem, %{
          tenant_id: tenant(),
          name: "a",
          completed: true,
          items: [%{name: "a", completed: true}]
        })

      assert resp.isError == true
      assert text(resp) =~ "not both"
    end

    test "errors when the tenant has no list" do
      {:reply, resp, _} = run(TodoUpdateItem, %{tenant_id: tenant(), name: "a", completed: true})
      assert resp.isError == true
      assert text(resp) =~ "No todo list found"
    end
  end

  describe "clear_items" do
    test "empties the list but keeps the tenant entry" do
      id = tenant()
      run(TodoSetItems, %{tenant_id: id, items: [%{name: "a"}], initial_user_prompt: "keep"})

      {:reply, clear_resp, _} = run(TodoClearItems, %{tenant_id: id})
      assert json(clear_resp)["success"] == true

      {:reply, get_resp, _} = run(TodoGetItems, %{tenant_id: id})
      data = json(get_resp)
      assert data["items"] == []
      assert data["initial_user_prompt"] == "keep"
    end
  end
end
