defmodule Exhub.Genclaw.Tools.FormatPrompt do
  @moduledoc """
  format_prompt tool — compose trajectory context into a final image prompt.
  """

  alias Exhub.Genclaw.{Session, ToolCards}

  def build do
    descriptions = ToolCards.load_all_descriptions()
    desc = Map.get(descriptions, "format_prompt", default_description())

    LangChain.Function.new!(%{
      name: "format_prompt",
      description: desc,
      parameters_schema: %{
        "type" => "object",
        "properties" => %{
          "original_user_prompt" => %{"type" => "string"},
          "image_intent" => %{"type" => "string"},
          "painter_thoughts" => %{"type" => "string"},
          "planning_needs" => %{"type" => "array"},
          "notes_for_planner" => %{"type" => "array", "items" => %{"type" => "string"}},
          "search_facts_summary" => %{"type" => "string"},
          "verified_facts" => %{"type" => "string"},
          "reasoning_output" => %{"type" => "array", "items" => %{"type" => "string"}},
          "draft_png_path" => %{"type" => "string"},
          "text_template_path" => %{"type" => "string"},
          "expected_texts_used" => %{"type" => "array", "items" => %{"type" => "string"}},
          "category" => %{"type" => "string"},
          "render_type" => %{"type" => "string"},
          "source_image_path" => %{"type" => "string"},
          "target_tool" => %{"type" => "string", "enum" => ["", "t2i", "i2i"]},
          "base_prompt" => %{"type" => "string"},
          "facts" => %{"type" => "string"},
          "reasoning" => %{"type" => "array", "items" => %{"type" => "string"}},
          "reference_image_paths" => %{"type" => "array", "items" => %{"type" => "string"}},
          "user_image_path" => %{"type" => "string"},
          "svg_description" => %{"type" => "string"}
        },
        "required" => [],
        "additionalProperties" => false
      },
      function: fn args, _ctx ->
        has_anchor =
          ["original_user_prompt", "base_prompt", "image_intent"]
          |> Enum.any?(fn key ->
            value = Map.get(args, key, "")
            is_binary(value) and String.trim(value) != ""
          end)

        if not has_anchor do
          Jason.encode!(%{
            "status" => "failed",
            "error" => "format_prompt requires 'original_user_prompt', 'base_prompt', or 'image_intent'."
          })
        else
          payload = compose_prompt(args)

          # Write artifacts
          out_dir = Session.tool_output_dir("format_prompt")
          File.write!(Path.join(out_dir, "final_prompt.txt"), Map.get(payload, "final_prompt", ""))
          Session.record_artifacts_in_dir(out_dir, tool_name: "format_prompt")

          Jason.encode!(payload)
        end
      end
    })
  end

  defp compose_prompt(args) do
    base =
      Map.get(args, "original_user_prompt") ||
        Map.get(args, "base_prompt") ||
        Map.get(args, "image_intent") ||
        ""

    facts = Map.get(args, "facts") || Map.get(args, "verified_facts") || Map.get(args, "search_facts_summary") || ""
    reasoning = Map.get(args, "reasoning") || Map.get(args, "reasoning_output") || []
    svg_desc = Map.get(args, "svg_description", "")
    refs = Map.get(args, "reference_image_paths", [])
    expected_texts = Map.get(args, "expected_texts_used", [])
    target = Map.get(args, "target_tool", "")

    parts = [base]

    parts =
      if facts != "" and facts != nil do
        parts ++ ["\n\nVerified facts:\n#{facts}"]
      else
        parts
      end

    parts =
      if is_list(reasoning) and reasoning != [] do
        parts ++ ["\n\nReasoning conclusions:\n" <> Enum.join(reasoning, "\n")]
      else
        parts
      end

    parts =
      if svg_desc != "" and svg_desc != nil do
        parts ++ ["\n\nLayout description:\n#{svg_desc}"]
      else
        parts
      end

    parts =
      if is_list(expected_texts) and expected_texts != [] do
        parts ++ ["\n\nText that must remain readable:\n" <> Enum.join(expected_texts, ", ")]
      else
        parts
      end

    final_prompt = Enum.join(parts) |> String.trim()

    %{
      "final_prompt" => final_prompt,
      "reference_paths" => refs,
      "i2i_image_path_hint" => List.first(refs),
      "i2i_extra_reference_paths_hint" => if(length(refs) > 1, do: Enum.drop(refs, 1), else: []),
      "target_tool" => target,
      "status" => "success"
    }
  end

  defp default_description do
    "Compose trajectory context (user prompt, facts, reasoning, layout, references) into a final image generation prompt."
  end
end
