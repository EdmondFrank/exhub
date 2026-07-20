defmodule Exhub.Genclaw.Middleware.CompletionGuard do
  @moduledoc """
  Sagents middleware: GenClaw completion guard.

  Ensures that an image-generation request actually produces a final image
  before the agent ends. After the agent loop completes (`after_model` hook),
  scans tool results for `final_path` indicators. If no image was produced,
  appends a `<system-reminder>` user message pushing the agent to either
  produce an image or explain why it cannot.

  Only fires when perception has been initialized (i.e. this is a genuine
  image request), mirroring the Python `completion_guard.py` guard.

  This is a port of GenClaw's `runtime/completion_guard.py`.
  """

  @behaviour Sagents.Middleware

  require Logger

  @reminder_body """
  [completion-check] The user's original request is an image task, but this run has NOT produced any final image yet (no image tool returned a successful final_path). Do NOT stop here and do NOT hand the task back to the user as a follow-up step. Either:
    (a) call the appropriate image tool now (t2i / i2i / code_text_draft, or code_scene_draft -> i2i) to actually produce the image and complete the request; or
    (b) if the request genuinely cannot be fulfilled with an image, state clearly to the user WHY.
  Reasoning output alone is not the deliverable — the image is.
  """

  @impl true
  def init(opts) do
    {:ok, %{max_fires: Keyword.get(opts, :max_fires, 1)}}
  end

  @impl true
  def system_prompt(_config), do: ""

  @impl true
  def before_model(state, _config) do
    {:ok, state}
  end

  @impl true
  def after_model(state, config) do
    guard_state = get_in(state.runtime, [:genclaw_completion_guard]) || %{fired: 0}
    image_produced_before = Map.get(guard_state, :image_produced, false)

    # If an image was already produced in a prior turn, we're done
    if image_produced_before do
      {:ok, state}
    else
      # Check if an image was produced in this turn
      image_produced = scan_for_final_image(state.messages)

      # Update tracking
      guard_state = Map.put(guard_state, :image_produced, image_produced)
      state = %{state | runtime: Map.put(state.runtime, :genclaw_completion_guard, guard_state)}

      if image_produced do
        {:ok, state}
      else
        # Only fire if perception was initialized (genuine image request)
        perception_state = get_in(state.runtime, [:genclaw_perception])
        perception_initialized = is_map(perception_state) and Map.get(perception_state, "initialized", false)

        if not perception_initialized do
          {:ok, state}
        else
          fired = Map.get(guard_state, :fired, 0)

          if fired >= config.max_fires do
            Logger.warning("[Genclaw.CompletionGuard] max fires (#{fired}) reached, no image produced")
            {:ok, state}
          else
            guard_state = Map.put(guard_state, :fired, fired + 1)
            state = %{state | runtime: Map.put(state.runtime, :genclaw_completion_guard, guard_state)}

            Logger.info("[Genclaw.CompletionGuard] no image produced, firing reminder (fire ##{fired + 1})")

            reminder_msg =
              LangChain.Message.new_user!(
                "<system-reminder>\n#{@reminder_body}\n</system-reminder>"
              )

            {:ok, Sagents.State.add_message(state, reminder_msg)}
          end
        end
      end
    end
  end

  # ─── Private Functions ───────────────────────────────────────────────────

  defp scan_for_final_image(messages) do
    messages
    |> Enum.filter(&(&1.role == :tool))
    |> Enum.any?(fn msg ->
      check_tool_results_for_final_path(msg) or check_msg_content_for_final_path(msg)
    end)
  end

  defp check_msg_content_for_final_path(%LangChain.Message{content: content}) do
    cond do
      is_binary(content) ->
        check_json_for_final_path(content)

      is_list(content) ->
        text = content |> Enum.map_join("", fn
          s when is_binary(s) -> s
          %LangChain.Message.ContentPart{type: :text, content: c} -> c || ""
          %LangChain.Message.ContentPart{content: c} when is_binary(c) -> c
          _ -> ""
        end)
        check_json_for_final_path(text)

      true ->
        false
    end
  end

  defp check_json_for_final_path(text) when is_binary(text) do
    case Jason.decode(text) do
      {:ok, %{"status" => "success", "final_path" => path}}
      when is_binary(path) and path != "" ->
        true

      _ ->
        false
    end
  end

  defp check_json_for_final_path(_), do: false

  defp check_tool_results_for_final_path(%LangChain.Message{} = msg) do
    case msg.tool_results do
      nil -> false
      results when is_list(results) -> Enum.any?(results, &check_single_result/1)
      _ -> false
    end
  end

  defp check_single_result(%LangChain.Message.ToolResult{} = result) do
    text = extract_text_from_tool_content(result.content)
    check_json_for_final_path(text)
  end

  defp check_single_result(_), do: false

  defp extract_text_from_tool_content(content) when is_binary(content), do: content

  defp extract_text_from_tool_content(content) when is_list(content) do
    content |> Enum.map_join("", fn
      text when is_binary(text) -> text
      %LangChain.Message.ContentPart{type: :text, content: c} -> c || ""
      %LangChain.Message.ContentPart{content: c} when is_binary(c) -> c
      %{content: c} when is_binary(c) -> c
      _ -> ""
    end)
  end

  defp extract_text_from_tool_content(%LangChain.Message.ContentPart{type: :text, content: c}), do: c || ""
  defp extract_text_from_tool_content(%LangChain.Message.ContentPart{content: c}) when is_binary(c), do: c
  defp extract_text_from_tool_content(_), do: ""
end
