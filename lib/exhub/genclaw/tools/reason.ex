defmodule Exhub.Genclaw.Tools.Reason do
  @moduledoc """
  reason tool — general-purpose LLM reasoning before image tools.

  A single reasoning call: give it a prompt (optionally with images to
  interpret) and it thinks the problem through, returning conclusions
  as plain text.
  """

  require Logger

  alias Exhub.Genclaw.{Session, ToolCards}
  alias Exhub.Llm.LlmConfigServer

  def build do
    descriptions = ToolCards.load_all_descriptions()
    desc = Map.get(descriptions, "reason", default_description())

    LangChain.Function.new!(%{
      name: "reason",
      description: desc,
      parameters_schema: %{
        "type" => "object",
        "properties" => %{
          "prompt" => %{
            "type" => "string",
            "description" => "The reasoning question."
          },
          "reference_image_paths" => %{
            "type" => "array",
            "items" => %{"type" => "string"}
          },
          "user_image_path" => %{
            "type" => "string"
          }
        },
        "required" => ["prompt"],
        "additionalProperties" => false
      },
      function: fn args, _ctx ->
        prompt = Map.get(args, "prompt", "")

        if not is_binary(prompt) or String.trim(prompt) == "" do
          Jason.encode!(%{"status" => "failed", "error" => "reason requires non-empty 'prompt'"})
        else
          try do
            reasoning = run_reasoning(prompt, Map.get(args, "reference_image_paths", []))
            out_dir = Session.tool_output_dir("reason")
            File.write!(Path.join(out_dir, "meta.json"), Jason.encode!(%{prompt: prompt, reasoning: reasoning}))
            Session.record_artifacts_in_dir(out_dir, tool_name: "reason")

            status = if reasoning != [], do: "success", else: "failed"
            Jason.encode!(%{"reasoning_output" => reasoning, "status" => status})
          rescue
            e ->
              Jason.encode!(%{"status" => "failed", "error" => "reason failed: #{inspect(e)}"})
          end
        end
      end
    })
  end

  defp run_reasoning(prompt, ref_image_paths) do
    case LlmConfigServer.get_default_llm_config() do
      {:ok, llm_config} ->
        model = build_langchain_model(llm_config)

        content_parts = [
          LangChain.Message.ContentPart.text!(prompt <> "\n\nThink through this step by step. Return your conclusions as a JSON array of strings.")
        ]

        content_parts =
          content_parts ++ encode_reference_images(ref_image_paths)

        message = LangChain.Message.new_user!(content_parts)

        case LangChain.Chains.LLMChain.new!(%{llm: model})
             |> LangChain.Chains.LLMChain.add_message(message)
             |> LangChain.Chains.LLMChain.run() do
          {:ok, chain} ->
            case List.last(chain.messages) do
              %LangChain.Message{role: :assistant, content: content} ->
                parse_reasoning(content)

              _ ->
                []
            end

          {:error, e} ->
            Logger.warning("[Genclaw.Reason] LLM call failed: #{inspect(e)}")
            []
        end

      {:error, _} ->
        []
    end
  end

  defp encode_reference_images(paths) do
    Enum.flat_map(paths, fn path ->
      if is_binary(path) and File.exists?(path) do
        data = File.read!(path)
        b64 = Base.encode64(data)
        mime = infer_mime(path)
        [LangChain.Message.ContentPart.image_url!("data:#{mime};base64,#{b64}")]
      else
        []
      end
    end)
  end

  defp parse_reasoning(content) when is_binary(content) do
    case Jason.decode(content) do
      {:ok, list} when is_list(list) ->
        Enum.map(list, &to_string/1)

      _ ->
        [content]
    end
  end

  defp parse_reasoning(content) when is_list(content) do
    content
    |> Enum.filter(&is_binary/1)
    |> Enum.join("\n")
    |> List.wrap()
  end

  defp build_langchain_model(config) do
    [provider, model_name] = String.split(config[:model], "/", parts: 2)

    base = %{model: model_name, api_key: config[:api_key]}

    case provider do
      "google" ->
        Map.put(base, :endpoint, config[:api_base])
        |> LangChain.ChatModels.ChatGoogleAI.new!()

      "anthropic" ->
        Map.put(base, :endpoint, "#{config[:api_base]}/messages")
        |> LangChain.ChatModels.ChatAnthropic.new!()

      _ ->
        Map.put(base, :endpoint, "#{config[:api_base]}/chat/completions")
        |> LangChain.ChatModels.ChatOpenAI.new!()
    end
  end

  defp infer_mime(path) do
    ext = Path.extname(path) |> String.downcase()
    Map.get(%{".png" => "image/png", ".jpg" => "image/jpeg", ".jpeg" => "image/jpeg",
              ".webp" => "image/webp", ".gif" => "image/gif"}, ext, "image/png")
  end

  defp default_description do
    "General-purpose LLM reasoning. Give it a prompt and it thinks the problem through, returning conclusions as text."
  end
end
