defmodule Exhub.Genclaw.Tools.T2I do
  @moduledoc """
  t2i tool — generate a new image from a text prompt only.
  """

  alias Exhub.Genclaw.{ImageGen, ToolCards}

  def build do
    descriptions = ToolCards.load_all_descriptions()
    desc = Map.get(descriptions, "t2i", default_description())

    LangChain.Function.new!(%{
      name: "t2i",
      description: desc,
      parameters_schema: %{
        "type" => "object",
        "properties" => %{
          "prompt" => %{
            "type" => "string",
            "description" => "Text description of the image to generate."
          },
          "quality" => %{
            "type" => "string",
            "enum" => ["fast", "balanced", "best"],
            "description" => "Reserved. Currently has no effect."
          }
        },
        "required" => ["prompt"],
        "additionalProperties" => false
      },
      function: fn args, _ctx ->
        prompt = Map.get(args, "prompt", "")

        if not is_binary(prompt) or String.trim(prompt) == "" do
          Jason.encode!(%{"status" => "failed", "error" => "t2i requires a non-empty 'prompt'."})
        else
          try do
            final_path = ImageGen.generate_t2i(prompt)
            Jason.encode!(%{"status" => "success", "final_path" => final_path})
          rescue
            e ->
              Jason.encode!(%{"status" => "failed", "error" => "#{inspect(e)}"})
          end
        end
      end
    })
  end

  defp default_description do
    "Generate a new image from a text prompt. Returns the absolute path of the generated PNG."
  end
end
