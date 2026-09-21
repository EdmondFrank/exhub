defmodule Exhub.MCP.Tools.Brain.SearchVaultFilterTest do
  # Mutates global Application config (:obsidian_vault_path and the Relevance
  # module env), so run serialized with the other Brain search tests.
  use ExUnit.Case, async: false

  alias Exhub.MCP.Brain.Search.Relevance
  alias Exhub.MCP.Tools.Brain.SearchVault

  setup do
    vault =
      System.tmp_dir!()
      |> Path.join("brain_search_filter_#{System.unique_integer([:positive])}")

    File.mkdir_p!(vault)
    Application.put_env(:exhub, :obsidian_vault_path, vault)

    previous = Application.get_env(:exhub, Relevance)

    on_exit(fn ->
      Application.delete_env(:exhub, :obsidian_vault_path)
      restore(previous)
      File.rm_rf!(vault)
    end)

    {:ok, vault: vault}
  end

  defp restore(nil), do: Application.delete_env(:exhub, Relevance)
  defp restore(previous), do: Application.put_env(:exhub, Relevance, previous)

  defp noul(probability) do
    {:ok, %{"answers" => %{"relevant" => %{"type" => "noul", "noul" => probability}}}}
  end

  defp search(params) do
    {:reply, resp, _frame} = SearchVault.execute(params, %{})
    Enum.find(resp.content, &(&1["type"] == "text"))["text"]
  end

  test "filters out candidates the model judges irrelevant", %{vault: vault} do
    File.write!(Path.join(vault, "budget.md"), "quarterly budget planning notes")
    File.write!(Path.join(vault, "grocery.md"), "quarterly grocery shopping list")

    decider = fn state, _questions, _opts ->
      if String.contains?(state, "budget.md"), do: noul(0.95), else: noul(0.05)
    end

    Application.put_env(:exhub, Relevance, enabled: true, decider: decider, fallback: false)

    output = search(%{query: "quarterly", filter: true})

    assert output =~ "budget.md"
    refute output =~ "grocery.md"
    assert output =~ "judged 1/2 note(s) relevant"
  end

  test "filter: false returns the raw ranked results", %{vault: vault} do
    File.write!(Path.join(vault, "budget.md"), "quarterly budget planning notes")
    File.write!(Path.join(vault, "grocery.md"), "quarterly grocery shopping list")

    decider = fn _state, _questions, _opts -> flunk("decider must not be called") end
    Application.put_env(:exhub, Relevance, enabled: true, decider: decider)

    output = search(%{query: "quarterly", filter: false})

    assert output =~ "budget.md"
    assert output =~ "grocery.md"
    refute output =~ "relevance filter"
  end

  test "falls back to the ranked pool when nothing is relevant", %{vault: vault} do
    File.write!(Path.join(vault, "notes.md"), "quarterly planning notes")

    decider = fn _state, _questions, _opts -> noul(0.01) end
    Application.put_env(:exhub, Relevance, enabled: true, decider: decider, fallback: true)

    output = search(%{query: "quarterly", filter: true})

    assert output =~ "notes.md"
    assert output =~ "found nothing relevant"
  end
end
