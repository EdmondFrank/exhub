defmodule Exhub.Genclaw.Tools.I2I do
  @moduledoc """
  i2i tool — generate a new image guided by an existing image and a prompt.
  """

  alias Exhub.Genclaw.{ImageGen, ToolCards}

  @legal_sources_hint """
  Legal sources for image_path (extract from a previous tool_result.content):
    - search.reference_paths[i]          (when refining a web reference)
    - code_scene_draft.draft_png_path    (when upgrading an SVG draft to photoreal)
    - code_text_draft.final_path         (when restyling a long-text image)
    - t2i.final_path                     (when iterating on a prior generation)
    - i2i.final_path                     (when chaining multiple i2i refinements)
    - user_image_path from painter notes (when the user uploaded an image)
  """

  def build do
    descriptions = ToolCards.load_all_descriptions()
    desc = Map.get(descriptions, "i2i", default_description())

    LangChain.Function.new!(%{
      name: "i2i",
      description: desc,
      parameters_schema: %{
        "type" => "object",
        "properties" => %{
          "image_path" => %{
            "type" => "string",
            "description" =>
              "ABSOLUTE path to an existing image file. Must be extracted from a previous tool_result.content — do NOT invent paths."
          },
          "prompt" => %{
            "type" => "string",
            "description" => "Guidance for the result image."
          },
          "reference_image_paths" => %{
            "type" => "array",
            "items" => %{"type" => "string"},
            "description" => "Additional reference images for multi-image guidance (do NOT include image_path here)."
          },
          "quality" => %{
            "type" => "string",
            "enum" => ["fast", "balanced", "best"],
            "description" => "Reserved. Currently has no effect."
          }
        },
        "required" => ["image_path", "prompt"],
        "additionalProperties" => false
      },
      function: fn args, _ctx ->
        image_path = Map.get(args, "image_path")
        prompt = Map.get(args, "prompt", "")

        cond do
          not is_binary(image_path) or image_path == "" ->
            fail("i2i requires arg 'image_path' (absolute path to an existing image).\n" <> @legal_sources_hint)

          not String.starts_with?(image_path, "/") ->
            fail("i2i: image_path '#{image_path}' is not an absolute path.\n" <> @legal_sources_hint)

          not File.exists?(image_path) ->
            fail("i2i: image_path '#{image_path}' is not a valid file (does not exist).\n" <> @legal_sources_hint)

          not is_binary(prompt) or String.trim(prompt) == "" ->
            fail("i2i requires a non-empty 'prompt'.")

          true ->
            extra_refs =
              args
              |> Map.get("reference_image_paths", [])
              |> Enum.filter(&(&1 != image_path))

            refs = [Path.expand(image_path) | Enum.map(extra_refs, &Path.expand/1)]

            try do
              final_path = ImageGen.generate_i2i(prompt, refs)
              Jason.encode!(%{"status" => "success", "final_path" => final_path})
            rescue
              e ->
                fail("#{inspect(e)}")
            end
        end
      end
    })
  end

  defp fail(error) do
    Jason.encode!(%{"status" => "failed", "error" => error})
  end

  defp default_description do
    "Generate a new image guided by an existing image and a prompt. Returns the absolute path of the generated PNG."
  end
end
