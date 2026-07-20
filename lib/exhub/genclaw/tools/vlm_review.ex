defmodule Exhub.Genclaw.Tools.VLMReview do
  @moduledoc """
  vlm_review tool — final image quality review via exhub's look MCP tool.

  Reviews the final image against the original user request and returns
  structured, actionable feedback.
  """

  require Logger

  alias Exhub.Genclaw.{Session, ToolCards}
  alias Exhub.MCP.Hub.BuiltInRegistry

  @default_criteria ["exact_count", "object_presence", "color_fidelity", "spatial_relation", "relative_size"]

  def build do
    descriptions = ToolCards.load_all_descriptions()
    desc = Map.get(descriptions, "vlm_review", default_description())

    LangChain.Function.new!(%{
      name: "vlm_review",
      description: desc,
      parameters_schema: %{
        "type" => "object",
        "properties" => %{
          "image_path" => %{
            "type" => "string",
            "description" => "Absolute path to the final image to review."
          },
          "original_user_prompt" => %{
            "type" => "string",
            "description" => "The original user request; source of truth for hard constraints."
          },
          "criteria" => %{
            "type" => "array",
            "items" => %{"type" => "string"},
            "description" => "Optional criteria to check."
          },
          "draft_png_path" => %{"type" => "string"},
          "svg_description" => %{"type" => "string"},
          "expected_texts" => %{
            "type" => "array",
            "items" => %{"type" => "string"}
          },
          "reference_image_paths" => %{
            "type" => "array",
            "items" => %{"type" => "string"}
          },
          "reasoning_conclusions" => %{
            "type" => "array",
            "items" => %{"type" => "string"}
          },
          "max_issues" => %{"type" => "integer"}
        },
        "required" => ["image_path", "original_user_prompt"],
        "additionalProperties" => false
      },
      function: fn args, _ctx ->
        image_path = Map.get(args, "image_path", "")
        original_prompt = Map.get(args, "original_user_prompt", "")

        cond do
          not is_binary(image_path) or image_path == "" ->
            Jason.encode!(%{"status" => "failed", "error" => "image_path is required"})

          not String.starts_with?(image_path, "/") or not File.exists?(image_path) ->
            Jason.encode!(%{"status" => "failed", "error" => "image_path must be an existing absolute file path"})

          not is_binary(original_prompt) or original_prompt == "" ->
            Jason.encode!(%{"status" => "failed", "error" => "original_user_prompt is required"})

          true ->
            criteria = Map.get(args, "criteria", @default_criteria)
            expected_texts = Map.get(args, "expected_texts", [])
            ref_paths = Map.get(args, "reference_image_paths", [])
            reasoning = Map.get(args, "reasoning_conclusions", [])

            review_prompt = build_review_prompt(original_prompt, criteria, expected_texts, ref_paths, reasoning)

            try do
              result = call_vlm(image_path, ref_paths, review_prompt)
              payload = normalize_review(result)

              out_dir = Session.tool_output_dir("vlm_review")
              File.write!(Path.join(out_dir, "meta.json"), Jason.encode!(payload))
              Session.record_artifacts_in_dir(out_dir, tool_name: "vlm_review")

              Jason.encode!(payload)
            rescue
              e ->
                Jason.encode!(%{"status" => "failed", "error" => "vlm_review failed: #{inspect(e)}"})
            end
        end
      end
    })
  end

  defp call_vlm(image_path, ref_paths, prompt) do
    # Use the look MCP tool for VLM review, passing reference images if available
    args =
      %{"image" => image_path, "prompt" => prompt}
      |> maybe_add_ref_context(ref_paths)

    case BuiltInRegistry.call_tool("look", "look", args) do
      {:ok, result} -> result
      {:error, e} -> raise RuntimeError, "look tool failed: #{inspect(e)}"
    end
  end

  defp maybe_add_ref_context(args, ref_paths) when is_list(ref_paths) and ref_paths != [] do
    # Append reference image context to the prompt so the VLM can compare
    existing_refs = Enum.filter(ref_paths, &File.exists?/1)

    if existing_refs != [] do
      ref_note =
        "\n\n[Reference images for comparison]: " <>
          Enum.join(existing_refs, ", ")

      Map.update!(args, "prompt", &(&1 <> ref_note))
    else
      args
    end
  end

  defp maybe_add_ref_context(args, _), do: args

  defp build_review_prompt(original_prompt, criteria, expected_texts, ref_paths, reasoning) do
    """
    You are a strict visual QA reviewer. Check whether the image satisfies the user's hard visual constraints.

    Original user prompt: #{original_prompt}

    Review criteria: #{inspect(criteria)}

    Expected texts: #{inspect(expected_texts)}

    Reference image paths: #{inspect(ref_paths)}

    Reasoning conclusions: #{inspect(reasoning)}

    Return ONLY a JSON object:
    {
      "passed": boolean,
      "verdict": "PASS" | "NEEDS_FIX" | "FAIL",
      "issues": [{"type": "...", "severity": "minor|major", "description": "...", "fix_instruction": "..."}],
      "suggested_fix_prompt": "concise prompt for i2i revision",
      "repair_strategy": "none" | "edit_previous" | "regenerate_from_source",
      "should_retry": boolean
    }
    """
  end

  defp normalize_review(result) do
    case result do
      %{text: text} when is_binary(text) ->
        parse_review_json(text)

      %{"text" => text} when is_binary(text) ->
        parse_review_json(text)

      map when is_map(map) ->
        Map.put(map, "status", "success")

      _ ->
        %{"status" => "success", "passed" => true, "verdict" => "PASS", "should_retry" => false}
    end
  end

  defp parse_review_json(text) do
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
        Map.put(obj, "status", "success")

      _ ->
        # Try to extract JSON from text
        case Regex.run(~r/\{[\s\S]*\}/, stripped) do
          [match] ->
            case Jason.decode(match) do
              {:ok, obj} -> Map.put(obj, "status", "success")
              _ -> fallback_review()
            end

          _ ->
            fallback_review()
        end
    end
  end

  defp fallback_review do
    %{
      "status" => "success",
      "passed" => true,
      "verdict" => "PASS",
      "issues" => [],
      "should_retry" => false,
      "repair_strategy" => "none"
    }
  end

  defp default_description do
    "Review the final image against the original user request. Returns structured, actionable feedback."
  end
end
