defmodule Exhub.Hercules.Middleware.TestTracker do
  @moduledoc """
  Sagents middleware: Hercules test tracker.

  Parses the Planner's structured JSON responses after each LLM call and
  tracks assertion results (is_assert, is_passed, assert_summary) in
  `state.metadata`. Also injects test data into the system prompt.
  """

  @behaviour Sagents.Middleware

  require Logger

  @impl true
  def init(opts) do
    test_data = Keyword.get(opts, :test_data, "")
    {:ok, %{test_data: test_data}}
  end

  @impl true
  def system_prompt(config) do
    if config.test_data != "" do
      "\n\n## Available Test Data\n\n#{config.test_data}\n"
    else
      ""
    end
  end

  @impl true
  def before_model(state, _config) do
    {:ok, state}
  end

  @impl true
  def after_model(state, _config) do
    case List.last(state.messages) do
      %LangChain.Message{role: :assistant, content: content} when is_binary(content) ->
        case parse_planner_json(content) do
          {:ok, parsed} ->
            metadata =
              Map.merge(state.metadata || %{}, %{
                "hercules_plan" => parsed["plan"],
                "hercules_next_step" => parsed["next_step"],
                "hercules_terminate" => parsed["terminate"],
                "hercules_is_assert" => parsed["is_assert"],
                "hercules_is_passed" => parsed["is_passed"],
                "hercules_assert_summary" => parsed["assert_summary"],
                "hercules_final_response" => parsed["final_response"],
                "hercules_target_helper" => parsed["target_helper"]
              })

            {:ok, %{state | metadata: metadata}}

          :no_json ->
            {:ok, state}
        end

      %LangChain.Message{role: :assistant, content: content} when is_list(content) ->
        # Handle content parts (multimodal messages)
        text =
          Enum.map_join(content, "", fn
            s when is_binary(s) -> s
            %LangChain.Message.ContentPart{type: :text, content: c} -> c || ""
            %LangChain.Message.ContentPart{content: c} when is_binary(c) -> c
            _ -> ""
          end)

        case parse_planner_json(text) do
          {:ok, parsed} ->
            metadata =
              Map.merge(state.metadata || %{}, %{
                "hercules_plan" => parsed["plan"],
                "hercules_next_step" => parsed["next_step"],
                "hercules_terminate" => parsed["terminate"],
                "hercules_is_assert" => parsed["is_assert"],
                "hercules_is_passed" => parsed["is_passed"],
                "hercules_assert_summary" => parsed["assert_summary"],
                "hercules_final_response" => parsed["final_response"],
                "hercules_target_helper" => parsed["target_helper"]
              })

            {:ok, %{state | metadata: metadata}}

          :no_json ->
            {:ok, state}
        end

      _ ->
        {:ok, state}
    end
  end

  # ─── Private Functions ───────────────────────────────────────────────────

  defp parse_planner_json(content) do
    cleaned =
      content
      |> String.replace("```json", "")
      |> String.replace("```", "")
      |> String.trim()

    case Jason.decode(cleaned) do
      {:ok, %{"terminate" => _} = map} -> {:ok, map}
      _ -> try_extract_json(cleaned)
    end
  end

  defp try_extract_json(text) do
    case Regex.run(~r/\{[\s\S]*\}/, text) do
      [match] ->
        case Jason.decode(match) do
          {:ok, %{"terminate" => _} = map} -> {:ok, map}
          _ -> :no_json
        end

      _ ->
        :no_json
    end
  end
end
