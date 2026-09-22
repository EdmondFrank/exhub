defmodule Exhub.MCP.Tools.Desktop.SearchFilesFilterTest do
  # Mutates the global Application config (the Relevance module env) to inject a
  # decider, so run serialized with the other Desktop search tests.
  use ExUnit.Case, async: false

  alias Exhub.MCP.Desktop.Search.Relevance
  alias Exhub.MCP.Tools.Desktop.SearchFiles

  # Exile (used by the probe path) needs its supervisor running.
  setup_all do
    Application.ensure_all_started(:exile)
    :ok
  end

  setup do
    tmp_dir =
      System.tmp_dir!()
      |> Path.join("search_files_filter_#{System.unique_integer([:positive])}")

    File.mkdir_p!(tmp_dir)

    File.write!(Path.join(tmp_dir, "keep.ex"), """
    defmodule Keep do
      def target_handler(payload) do
        {:ok, payload}
      end
    end
    """)

    File.write!(Path.join(tmp_dir, "drop.ex"), """
    defmodule Drop do
      def target_handler(other) do
        {:error, other}
      end
    end
    """)

    previous = Application.get_env(:exhub, Relevance)

    on_exit(fn ->
      restore(previous)
      File.rm_rf!(tmp_dir)
    end)

    {:ok, tmp_dir: tmp_dir}
  end

  defp restore(nil), do: Application.delete_env(:exhub, Relevance)
  defp restore(previous), do: Application.put_env(:exhub, Relevance, previous)

  defp noul(probability) do
    {:ok, %{"answers" => %{"relevant" => %{"type" => "noul", "noul" => probability}}}}
  end

  defp search(params) do
    {:reply, resp, _frame} = SearchFiles.execute(params, %{})
    assert resp.isError == false
    Enum.find(resp.content, &(&1["type"] == "text"))["text"]
  end

  test "filters out candidates the model judges irrelevant", %{tmp_dir: tmp_dir} do
    decider = fn state, _questions, _opts ->
      if String.contains?(state, "keep.ex"), do: noul(0.95), else: noul(0.05)
    end

    Application.put_env(:exhub, Relevance,
      enabled: true,
      decider: decider,
      fallback: false
    )

    output = search(%{path: tmp_dir, query: "target_handler", filter: true})

    assert output =~ "keep.ex"
    refute output =~ "drop.ex"
    assert output =~ "judged 1/2 result(s) relevant"
  end

  test "the purpose drives the Smart Decide question, not the raw query", %{tmp_dir: tmp_dir} do
    {:ok, agent} = Agent.start_link(fn -> nil end)

    decider = fn _state, questions, _opts ->
      Agent.update(agent, fn _ -> questions end)
      noul(0.95)
    end

    Application.put_env(:exhub, Relevance, enabled: true, decider: decider)

    _ =
      search(%{
        path: tmp_dir,
        query: "target_handler",
        purpose: "find how the keep module handles payloads",
        filter: true
      })

    questions = Agent.get(agent, & &1)
    instructions = questions["relevant"]["instructions"]
    assert instructions =~ "find how the keep module handles payloads"
    refute instructions =~ "target_handler"
  end

  test "a blank purpose falls back to the query", %{tmp_dir: tmp_dir} do
    {:ok, agent} = Agent.start_link(fn -> nil end)

    decider = fn _state, questions, _opts ->
      Agent.update(agent, fn _ -> questions end)
      noul(0.95)
    end

    Application.put_env(:exhub, Relevance, enabled: true, decider: decider)

    _ =
      search(%{
        path: tmp_dir,
        query: "target_handler",
        purpose: "   ",
        filter: true
      })

    questions = Agent.get(agent, & &1)
    instructions = questions["relevant"]["instructions"]
    assert instructions =~ "target_handler"
  end

  test "keeps oversized results unjudged and reports them", %{tmp_dir: tmp_dir} do
    decider = fn _state, _questions, _opts -> flunk("decider must not be called") end

    Application.put_env(:exhub, Relevance,
      enabled: true,
      decider: decider,
      max_judgeable_chars: 5
    )

    output = search(%{path: tmp_dir, query: "target_handler", filter: true})

    assert output =~ "keep.ex"
    assert output =~ "drop.ex"
    assert output =~ "oversized"
  end

  test "filter: false returns the raw probe results", %{tmp_dir: tmp_dir} do
    decider = fn _state, _questions, _opts -> flunk("decider must not be called") end
    Application.put_env(:exhub, Relevance, enabled: true, decider: decider)

    output = search(%{path: tmp_dir, query: "target_handler", filter: false})

    assert output =~ "keep.ex"
    assert output =~ "drop.ex"
    refute output =~ "relevance filter"
  end

  test "falls back to the ranked pool when nothing is relevant", %{tmp_dir: tmp_dir} do
    decider = fn _state, _questions, _opts -> noul(0.01) end
    Application.put_env(:exhub, Relevance, enabled: true, decider: decider, fallback: true)

    output = search(%{path: tmp_dir, query: "target_handler", filter: true})

    assert output =~ "keep.ex"
    assert output =~ "drop.ex"
    assert output =~ "found nothing relevant"
  end
end
