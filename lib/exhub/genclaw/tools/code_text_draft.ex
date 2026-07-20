defmodule Exhub.Genclaw.Tools.CodeTextDraft do
  @moduledoc """
  code_text_draft tool — render exact literal text into an image via HTML/CSS.

  Uses LLM to generate HTML/CSS code containing the verbatim text strings,
  renders to PNG via headless Chrome.

  For pure_code outputs, final_path IS the final image.
  For embedded_svg outputs, final_path is a text/structure reference — the
  agent should follow the returned next_step_hint to call format_prompt → i2i.
  """

  require Logger

  alias Exhub.Genclaw.{Session, ToolCards, LLMHelper, RenderHelper}

  @code_delim "===SVG_CODE_START==="
  @max_retries 3

  @html_system_prompt """
  You are an expert HTML/CSS designer. Generate a complete, standalone HTML document that renders the requested content with exact text.

  **CRITICAL: Do NOT include ANY conversational text, explanations, or commentary. Output ONLY the HTML code.**

  The HTML must:
  1. Be a complete <!DOCTYPE html> document with inline CSS (no external resources).
  2. Include ALL expected text strings VERBATIM — character-perfect, no typos.
  3. Use appropriate typography: readable fonts, proper sizing, alignment.
  4. Have a clean, professional layout matching the prompt's description.
  5. Use a fixed pixel canvas (e.g., 1024x1024 or appropriate aspect ratio).
  6. Set body margin to 0 and use a container div with explicit dimensions.
  7. Include the exact text strings as specified in expected_long_texts.

  Output format:
  First, a brief description of the layout on one line.
  Then the delimiter: ===SVG_CODE_START===
  Then the complete HTML code (no markdown fences).
  """

  @html_revise_system_prompt """
  You are an expert HTML/CSS designer. You are given:
  1. The original prompt and expected texts
  2. Your previous HTML code that needs fixes
  3. Specific feedback on what to fix

  Output the corrected HTML code only. Start with a one-line description,
  then ===SVG_CODE_START===, then the complete corrected HTML code.
  """

  @review_prompt """
  You are evaluating whether an HTML-rendered image contains the exact expected text strings.

  **Prompt**: "{prompt}"
  **Expected texts**: {expected_texts}

  Check:
  1. Are ALL expected text strings present, character-perfect?
  2. Is the text readable and properly styled?
  3. Is the layout appropriate for the content type?

  Respond with ONE of:
  - PASS  (all texts present and readable)
  - FAIL  (output <feedback>concise description of the issue</feedback>)
  """

  def build do
    descriptions = ToolCards.load_all_descriptions()
    desc = Map.get(descriptions, "code_text_draft", default_description())

    LangChain.Function.new!(%{
      name: "code_text_draft",
      description: desc,
      parameters_schema: %{
        "type" => "object",
        "properties" => %{
          "prompt" => %{
            "type" => "string",
            "description" => "Overall image description (style, mood, background scene)."
          },
          "expected_long_texts" => %{
            "type" => "array",
            "items" => %{"type" => "string"},
            "description" => "Verbatim strings that MUST appear character-perfectly in the image. At least 1 required."
          },
          "category" => %{
            "type" => "string",
            "enum" => ["", "menu", "scoreboard", "poster", "sign"],
            "default" => ""
          },
          "gen_backend" => %{
            "type" => "string",
            "enum" => ["gemini", "qwen"],
            "default" => "gemini"
          }
        },
        "required" => ["prompt", "expected_long_texts"],
        "additionalProperties" => false
      },
      function: fn args, _ctx ->
        prompt = Map.get(args, "prompt", "")
        expected = Map.get(args, "expected_long_texts", [])

        expected = expected
        |> Enum.map(&to_string/1)
        |> Enum.map(&String.trim/1)
        |> Enum.reject(&(&1 == ""))

        cond do
          not is_binary(prompt) or String.trim(prompt) == "" ->
            Jason.encode!(%{"status" => "failed", "error" => "code_text_draft requires non-empty 'prompt'"})

          expected == [] ->
            Jason.encode!(%{"status" => "failed", "error" => "code_text_draft requires non-empty 'expected_long_texts'. If no specific text needs rendering, use 't2i' instead."})

          true ->
            try do
              result = run_text_pipeline(prompt, expected)

              out_dir = Session.tool_output_dir("code_text_draft")
              File.write!(Path.join(out_dir, "meta.json"), Jason.encode!(Map.take(result, [
                "prompt", "expected_long_texts", "render_type", "final_path",
                "output_role", "requires_i2i", "next_step_hint"
              ]), pretty: true))

              Session.record_artifacts_in_dir(out_dir, tool_name: "code_text_draft")

              case Map.get(result, "final_path") do
                path when is_binary(path) and path != "" ->
                  Session.record_artifact(path,
                    tool_name: "code_text_draft",
                    role: "final",
                    kind: "image",
                    label: "Text-rendered image"
                  )
                _ -> :ok
              end

              Jason.encode!(Map.take(result, [
                "final_path", "render_type", "expected_texts_used",
                "output_role", "requires_i2i", "next_step_hint", "status", "error"
              ]))
            rescue
              e ->
                Jason.encode!(%{"status" => "failed", "error" => "code_text_draft failed: #{inspect(e)}"})
            end
        end
      end
    })
  end

  # ─── Pipeline ─────────────────────────────────────────────────────────────

  defp run_text_pipeline(prompt, expected_texts) do
    out_dir = Session.tool_output_dir("code_text_draft")

    # Step 1: generate HTML
    Logger.info("[Genclaw.CodeTextDraft] generating HTML for: #{String.slice(prompt, 0, 80)}")
    gen = generate_html(prompt, expected_texts)
    html_code = gen["code"]

    File.write!(Path.join(out_dir, "step1.html"), html_code)

    # Step 2: render PNG
    Logger.info("[Genclaw.CodeTextDraft] rendering PNG")
    final_path = Path.join(out_dir, "final.png")
    render_html_to_png(html_code, final_path)

    # Step 3: VLM review
    {review_passed, review_feedback} = review_render(prompt, expected_texts, final_path)

    # Revise if needed (up to 2 rounds)
    {html_code, final_path, review_passed} =
      if not review_passed do
        run_revise_loop(prompt, expected_texts, html_code, final_path, review_feedback, out_dir, 1)
      else
        {html_code, final_path, true}
      end

    File.write!(Path.join(out_dir, "final.html"), html_code)

    # Determine render type: if the prompt suggests artistic styling/background,
    # the text image is a reference for i2i beautification; otherwise it's final.
    {render_type, requires_i2i, output_role, next_step_hint} =
      if needs_artistic_restyle?(prompt) do
        {"embedded_text", true, "text_reference",
         "Call i2i with image_path=this final_path and a prompt that preserves all readable text while adding artistic background/style."}
      else
        {"pure_code", false, "final_image", nil}
      end

    status = if final_path != "" and File.exists?(final_path), do: "success", else: "failed"

    %{
      "final_path" => final_path,
      "render_type" => render_type,
      "expected_texts_used" => expected_texts,
      "output_role" => output_role,
      "requires_i2i" => requires_i2i,
      "next_step_hint" => next_step_hint,
      "review_passed" => review_passed,
      "status" => status
    }
  end

  defp run_revise_loop(prompt, expected_texts, html_code, _current_path, feedback, out_dir, round) when round <= 2 do
    Logger.info("[Genclaw.CodeTextDraft] revise round #{round}: #{String.slice(feedback, 0, 100)}")

    revised = revise_html(prompt, expected_texts, html_code, feedback)
    new_html = revised["code"]

    new_path = Path.join(out_dir, "revise#{round}.png")
    render_html_to_png(new_html, new_path)

    {passed, fb} = review_render(prompt, expected_texts, new_path)

    if passed do
      {new_html, new_path, true}
    else
      run_revise_loop(prompt, expected_texts, new_html, new_path, fb, out_dir, round + 1)
    end
  end

  defp run_revise_loop(_, _, html_code, final_path, _, _, _) do
    {html_code, final_path, false}
  end

  # ─── HTML Generation ────────────────────────────────────────────────────────

  defp generate_html(prompt, expected_texts) do
    user_msg = """
    Scene description: #{prompt}

    Expected text strings (MUST appear verbatim):
    #{Enum.map(expected_texts, &"  - #{&1}") |> Enum.join("\n")}

    Generate a complete HTML document with these exact texts.
    """

    case call_llm(@html_system_prompt, user_msg) do
      {:ok, raw} ->
        parsed = parse_code_response(raw)
        if String.length(parsed["code"]) < 100 do
          Logger.warning("[Genclaw.CodeTextDraft] HTML too short, retrying")
          generate_html_retry(prompt, expected_texts, 1)
        else
          parsed
        end

      {:error, e} ->
        raise RuntimeError, "HTML generation failed: #{inspect(e)}"
    end
  end

  defp generate_html_retry(prompt, expected_texts, attempt) when attempt <= @max_retries do
    case call_llm(@html_system_prompt, "Generate HTML for:\n#{prompt}\n\nExpected texts: #{inspect(expected_texts)}\n\n(Previous attempt was too short. Output complete HTML.)") do
      {:ok, raw} ->
        parsed = parse_code_response(raw)
        if String.length(parsed["code"]) < 100 do
          generate_html_retry(prompt, expected_texts, attempt + 1)
        else
          parsed
        end

      {:error, _} ->
        generate_html_retry(prompt, expected_texts, attempt + 1)
    end
  end

  defp generate_html_retry(_, _, _), do: raise(RuntimeError, "HTML generation failed after #{@max_retries} retries")

  defp revise_html(prompt, expected_texts, old_code, feedback) do
    user_msg = """
    Original prompt: #{prompt}

    Expected texts: #{inspect(expected_texts)}

    Previous HTML code:
    ```
    #{old_code}
    ```

    Review feedback: #{feedback}

    Please generate corrected HTML that fixes all the issues.
    """

    case call_llm(@html_revise_system_prompt, user_msg) do
      {:ok, raw} -> parse_code_response(raw)
      {:error, e} -> raise RuntimeError, "HTML revision failed: #{inspect(e)}"
    end
  end

  defp parse_code_response(raw) do
    if String.contains?(raw, @code_delim) do
      [desc, code] = String.split(raw, @code_delim, parts: 2)
      description = String.trim(desc)
      code = String.trim(code)
      |> String.replace(~r/^```(?:html|xml)?\s*/, "")
      |> String.replace(~r/\s*```$/, "")
      |> String.replace(@code_delim, "")

      code = extract_html_block(code)

      %{"code" => code, "description" => description, "code_type" => "html"}
    else
      code = String.trim(raw)
      |> String.replace(~r/^```(?:html|xml)?\s*/, "")
      |> String.replace(~r/\s*```$/, "")

      code = extract_html_block(code)

      %{"code" => code, "description" => "", "code_type" => "html"}
    end
  end

  defp extract_html_block(code) do
    case Regex.run(~r/(?:<html|<!doctype)/i, code, return: :index) do
      [{s, _}] -> binary_part(code, s, byte_size(code) - s)
      _ -> code
    end
  end

  # ─── VLM Review ────────────────────────────────────────────────────────────

  defp review_render(prompt, expected_texts, png_path) do
    review_text = @review_prompt
    |> String.replace("{prompt}", prompt)
    |> String.replace("{expected_texts}", inspect(expected_texts))

    case Exhub.MCP.Hub.BuiltInRegistry.call_tool("look", "look", %{
      "image" => png_path,
      "prompt" => review_text
    }) do
      {:ok, result} ->
        reply = extract_text(result)
        if String.starts_with?(String.trim(reply), "PASS") do
          {true, ""}
        else
          feedback = case Regex.run(~r/<feedback>\s*(.*?)\s*<\/feedback>/s, reply) do
            [_, f] -> f
            _ -> String.trim(reply)
          end
          {false, feedback}
        end

      {:error, e} ->
        Logger.warning("[Genclaw.CodeTextDraft] review failed: #{inspect(e)}")
        {true, ""}
    end
  end

  # ─── Rendering ─────────────────────────────────────────────────────────────

  defp render_html_to_png(html_code, output_path) do
    RenderHelper.render_to_png(html_code, output_path, "html", window_size: {1024, 1024})
  end

  # ─── Style Detection ──────────────────────────────────────────────────────

  @artistic_keywords ~w(
    artistic art style stylized painterly watercolor oil-painting
    illustration cinematic dramatic background scene decorative
    艺术 风格 水彩 油画 插画 电影感 背景 装饰 唯美 海报 宣传
  )

  defp needs_artistic_restyle?(prompt) do
    downcased = String.downcase(prompt)
    Enum.any?(@artistic_keywords, &String.contains?(downcased, &1))
  end

  # ─── LLM ───────────────────────────────────────────────────────────────────

  defp call_llm(system_prompt, user_prompt) do
    case LLMHelper.call_llm(system_prompt, user_prompt) do
      {:ok, text} -> {:ok, text}
      {:error, e, _model} -> {:error, e}
    end
  end

  defp extract_text(content), do: LLMHelper.extract_text(content)

  defp default_description do
    "Render exact literal text into an image via HTML/CSS. For menus, posters, scoreboards, and multi-line text."
  end
end
