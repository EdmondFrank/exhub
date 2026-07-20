defmodule Exhub.Genclaw.Middleware.Perception do
  @moduledoc """
  Sagents middleware: GenClaw perception layer (painter notes).

  Runs once on the first `before_model` call: extracts the user's prompt,
  calls a perception sub-LLM, and stores the resulting "painter notes" in
  state.runtime. On every `before_model` call, injects the painter notes
  as a `<system-reminder>` user message so the main LLM sees them every turn.

  This is a direct port of GenClaw's `perception/` + `runtime/perception_init.py`
  + `runtime/perception_state.py` + `runtime/attachment.py` (perception hook).
  """

  @behaviour Sagents.Middleware

  require Logger

  alias Exhub.Genclaw.LLMHelper

  @perception_prompt_path "genclaw/perception_prompt.yaml"

  @default_perception %{
    "painter_thoughts" => "(perception unavailable)",
    "image_intent" => "(perception unavailable)",
    "planning_needs" => [],
    "notes_for_planner" => [
      "Perception failed. Use the original user prompt and tool cards directly."
    ]
  }

  @impl true
  def init(_opts) do
    {:ok, %{}}
  end

  @impl true
  def system_prompt(_config), do: ""

  @impl true
  def before_model(state, _config) do
    Logger.info("[Genclaw.Perception] before_model called, messages count: #{length(state.messages)}")

    # Check if perception has already been initialized
    perception_state = get_in(state.runtime, [:genclaw_perception])

    perception_state =
      if perception_state do
        Logger.info("[Genclaw.Perception] perception already initialized, reusing")
        perception_state
      else
        # Run perception for the first time
        user_prompt = extract_user_prompt(state.messages)
        Logger.info("[Genclaw.Perception] running perception for prompt: #{String.slice(user_prompt, 0, 100)}")
        result = run_perception(user_prompt)
        Logger.info("[Genclaw.Perception] perception result: painter_thoughts=#{String.slice(Map.get(result, "painter_thoughts", ""), 0, 100)}")
        result
      end

    # Store perception state in runtime
    state = %{state | runtime: Map.put(state.runtime, :genclaw_perception, perception_state)}

    # Only inject painter notes if not already present in messages (dedup)
    if already_has_reminder?(state.messages) do
      {:ok, state}
    else
      reminder = render_reminder(perception_state)

      if reminder != "" do
        reminder_msg =
          LangChain.Message.new_user!(
            "<system-reminder>\n#{reminder}\n</system-reminder>"
          )

        {:ok, Sagents.State.add_message(state, reminder_msg)}
      else
        {:ok, state}
      end
    end
  end

  @impl true
  def after_model(state, _config) do
    {:ok, state}
  end

  # ─── Private Functions ───────────────────────────────────────────────────

  defp already_has_reminder?(messages) do
    Enum.any?(messages, fn msg ->
      msg.role == :user and
        is_binary(msg.content) and
        String.contains?(msg.content, "[PAINTER NOTES — perception output]")
    end)
  end

  defp extract_user_prompt(messages) do
    msg = Enum.find(messages, fn msg -> msg.role == :user end)

    if msg do
      text = extract_message_text(msg)
      Logger.info("[Genclaw.Perception] found user message, content type: #{inspect(msg.content) |> String.slice(0, 100)}")
      text
    else
      Logger.warning("[Genclaw.Perception] no user message found in #{length(messages)} messages")
      ""
    end
  end

  defp extract_message_text(%LangChain.Message{content: content}) when is_binary(content), do: content

  defp extract_message_text(%LangChain.Message{content: content}) when is_list(content) do
    Enum.map_join(content, "", fn
      text when is_binary(text) -> text
      %LangChain.Message.ContentPart{type: :text, content: c} -> c || ""
      %LangChain.Message.ContentPart{content: c} when is_binary(c) -> c
      %{content: c} when is_binary(c) -> c
      _ -> ""
    end)
  end

  defp extract_message_text(%LangChain.Message{content: %LangChain.Message.ContentPart{type: :text, content: c}}), do: c || ""
  defp extract_message_text(%LangChain.Message{content: %{content: c}}) when is_binary(c), do: c
  defp extract_message_text(_), do: ""

  defp run_perception(user_prompt) do
    system_prompt = load_perception_prompt()

    if system_prompt == "" or user_prompt == "" do
      Logger.warning("[Genclaw.Perception] skipping: system_prompt empty=#{system_prompt == ""}, user_prompt empty=#{user_prompt == ""}")
      @default_perception
    else
      case call_perception_llm(system_prompt, user_prompt) do
        {:ok, result} ->
          Logger.info("[Genclaw.Perception] LLM returned raw: #{String.slice(result, 0, 200)}")
          parsed = parse_perception_result(result)
          Logger.info("[Genclaw.Perception] parsed: thoughts=#{String.slice(Map.get(parsed, "painter_thoughts", ""), 0, 80)}")
          parsed

        {:error, e} ->
          Logger.warning("[Genclaw.Perception] perception LLM call failed: #{inspect(e)}")
          @default_perception
      end
    end
  end

  defp load_perception_prompt do
    path =
      :exhub
      |> :code.priv_dir()
      |> Path.join(@perception_prompt_path)

    case File.read(path) do
      {:ok, content} ->
        # Extract the system_prompt field from the YAML
        case Regex.run(~r/system_prompt:\s*\|\s*\n(.*)/s, content) do
          [_, prompt] -> String.trim(prompt)
          _ -> ""
        end

      _ ->
        ""
    end
  end

  defp call_perception_llm(system_prompt, user_prompt) do
    case LLMHelper.call_llm(system_prompt, user_prompt) do
      {:ok, text} -> {:ok, text}
      {:error, e, _model} -> {:error, e}
    end
  end

  defp parse_perception_result(text) do
    stripped = String.trim(text)

    stripped =
      if String.starts_with?(stripped, "```") do
        stripped
        |> String.replace(~r/^```(?:json)?\s*/, "")
        |> String.replace(~r/\s*```$/, "")
      else
        stripped
      end

    case Jason.decode(stripped) do
      {:ok, obj} when is_map(obj) ->
        %{
          "painter_thoughts" => Map.get(obj, "painter_thoughts", ""),
          "image_intent" => Map.get(obj, "image_intent", ""),
          "planning_needs" => Map.get(obj, "planning_needs", []),
          "notes_for_planner" => Map.get(obj, "notes_for_planner", []),
          "initialized" => true
        }

      _ ->
        # Try to extract JSON from text
        case Regex.run(~r/\{[\s\S]*\}/, stripped) do
          [match] ->
            case Jason.decode(match) do
              {:ok, obj} when is_map(obj) ->
                %{
                  "painter_thoughts" => Map.get(obj, "painter_thoughts", ""),
                  "image_intent" => Map.get(obj, "image_intent", ""),
                  "planning_needs" => Map.get(obj, "planning_needs", []),
                  "notes_for_planner" => Map.get(obj, "notes_for_planner", []),
                  "initialized" => true
                }

              _ ->
                @default_perception
            end

          _ ->
            @default_perception
        end
    end
  end

  defp render_reminder(%{"initialized" => true} = state) do
    lines = ["[PAINTER NOTES — perception output]"]

    thoughts = Map.get(state, "painter_thoughts", "")

    lines =
      if thoughts != "" and thoughts != "(perception unavailable)" do
        lines ++ ["painter_thoughts:", "  #{thoughts}"]
      else
        lines
      end

    intent = Map.get(state, "image_intent", "")
    intent_display = if intent == "" or intent == nil, do: "(use original prompt)", else: intent
    lines = lines ++ ["image_intent: #{intent_display}"]

    planning_needs = Map.get(state, "planning_needs", [])

    needs_lines =
      case planning_needs do
        [] ->
          ["(none listed; use the original prompt and tool cards directly)"]

        needs ->
          needs
          |> Enum.with_index(1)
          |> Enum.map(fn {need, idx} ->
            kind = Map.get(need, "need", "?")
            question = Map.get(need, "question", "")
            question_display = if question == "" or question == nil, do: "(none)", else: question
            evidence = Map.get(need, "evidence", "")
            resolved = Map.get(need, "resolved", false)

            base = "#{idx}. #{kind} | resolved=#{resolved} | question: #{question_display}"

            if evidence != "" do
              base <> "\n   evidence: #{evidence}"
            else
              base
            end
          end)
      end

    lines = lines ++ ["[planning_needs — hints, not routing decisions]"] ++ needs_lines

    notes = Map.get(state, "notes_for_planner", [])

    lines =
      case notes do
        [] -> lines
        _ -> lines ++ ["notes_for_planner:"] ++ Enum.map(notes, &"  - #{&1}")
      end

    lines =
      lines ++
        ["", "[REMINDER]",
         "These notes are hints, not tool routing rules. Open todo_write by default.",
         "Build concrete tool arguments from the original user prompt and prior",
         "tool_result.content at the moment you call each tool."]

    Enum.join(lines, "\n")
  end

  defp render_reminder(_), do: ""
end
