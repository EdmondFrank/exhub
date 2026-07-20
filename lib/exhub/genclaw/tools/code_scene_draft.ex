defmodule Exhub.Genclaw.Tools.CodeSceneDraft do
  @moduledoc """
  code_scene_draft tool — code-draw an SVG draft to lock precise multi-object
  geometry (count, colour, position).

  Uses LLM to generate SVG code, renders to PNG via headless Chrome,
  optionally self-reviews via VLM (look MCP tool).

  Output is a draft PNG; the agent should refine it to photoreal via i2i.
  """

  require Logger

  alias Exhub.Genclaw.{Session, ToolCards}
  alias Exhub.Llm.LlmConfigServer
  alias Exhub.MCP.Hub.BuiltInRegistry

  @planning_delim "===PLANNING_START==="
  @legend_delim "===LEGEND_START==="
  @svg_delim "===SVG_CODE_START==="
  @min_svg_chars 400
  @max_retries 3
  @max_revise_rounds 3
  @llm_receive_timeout 180_000

  @svg_system_prompt """
  You are a structural layout SVG generator. Your SVG is a DRAFT BLUEPRINT that will be refined into a photorealistic image by a downstream AI model. Therefore, focus ONLY on correct structure (count, color, position, size) -- NOT on visual realism or detail.

  **CRITICAL: Do NOT include ANY conversational text, explanations, disclaimers, or commentary in your output. Do NOT say things like "I understand...", "Let me...", "Here is...", "Note that...". Output ONLY the three parts described below. Nothing else.**

  Given a text description, you must output EXACTLY in THREE parts separated by the exact delimiters:

  ===PLANNING_START===
  PART 1 - LAYOUT PLANNING (Chain-of-Thought):
  Analyze the prompt step by step and plan how you will construct the SVG. Think through:
  - What CANVAS SIZE and ASPECT RATIO best fit this scene? Choose width W and height H (in pixels) that match the content's natural framing.
  - What are the PRIMARY SUBJECTS vs background/secondary elements? Subjects must be drawn LARGE.
  - What distinct objects need to be drawn? List each with its color and shape type.
  - How many instances of each object? Verify exact counts.
  - What spatial arrangement is required? (grid, row, cluster, scattered, left-right, etc.)
  - How to divide the chosen W x H canvas so that all objects fit without overlapping?
  - Calculate approximate size for each subject relative to the chosen canvas.
  - **CRITICAL BOUNDARY CHECK**: For each object, verify that its FULL extent stays within 0-W on the x axis and 0-H on the y axis.
  - Any potential pitfalls (objects too small, too close, canvas overflow, ambiguous groupings)?
  Keep this section concise but thorough (5-10 lines). Write in the SAME language as the user input.

  ===LEGEND_START===
  PART 2 - LAYOUT JSON (a single valid JSON object, values in the SAME language as the user input):
  Describe the planned image as a structured layout. Output ONLY the JSON object (no markdown fences, no commentary), with this shape:
  {
    "high_level_description": "one-sentence summary of the whole scene",
    "style_description": {
      "aesthetics": "overall mood/feel",
      "lighting": "lighting description",
      "color_palette": ["#RRGGBB", "#RRGGBB"]
    },
    "compositional_deconstruction": {
      "background": "background / environment description",
      "elements": [
        { "type": "obj", "bbox": [x1, y1, x2, y2], "desc": "what this object is, its color and where it sits" }
      ]
    }
  }

  ===SVG_CODE_START===
  PART 3 - SVG CODE:
  The raw SVG code, with NO markdown fences.

  Code rules:
  1. Output a standalone <svg> with explicit width="W" height="H" and a matching viewBox="0 0 W H".
  2. Use vivid, clearly distinguishable fill colors for each object.
  3. Position elements precisely according to the description.
  4. **NEVER use <text> elements.** No labels, captions, or annotations.
  5. **SIMPLICITY IS KEY.** Represent ALL objects as simple, iconic shapes -- circles, ellipses, rectangles, rounded-rects, and basic polygons.
  6. **Do NOT use complex SVG features**: no gradients, no filters, no masks, no clip-paths, no animations. Plain fills only.
  7. Use a white or light background.
  8. Ensure every element mentioned in the description is present with the **EXACT** count, color, and position.
  9. **MINIMIZE overlapping and occlusion.** Every object must be fully visible.
  10. **SUBJECT OBJECTS MUST BE LARGE AND PROMINENT.** Primary subjects should collectively fill at least 60-80% of the canvas.
  """

  @svg_revise_system_prompt """
  You are a structural layout SVG generator. You are given:
  1. The original prompt
  2. Your previous SVG code that was reviewed and found lacking
  3. Specific feedback on what needs to be fixed

  You must output in THREE parts separated by the exact delimiters:

  ===PLANNING_START===
  PART 1: Analyze the feedback and plan your fixes step by step.

  ===LEGEND_START===
  PART 2: Updated LAYOUT JSON (a single valid JSON object). Output ONLY the JSON.

  ===SVG_CODE_START===
  PART 3: The corrected SVG code (NO markdown fences).

  Rules: explicit width="W" height="H" and matching viewBox, vivid plain fill colors, NO <text> elements, only simple iconic shapes. Keep the code SHORT. Exact object counts. Fix ALL issues mentioned in the feedback.
  """

  @review_prompt """
  You are evaluating whether an SVG structural draft correctly represents the CONTENT of a text prompt.

  **This SVG is an intentionally simplified structural blueprint -- it uses flat colors and basic shapes on purpose. A downstream AI model will convert it into a photorealistic image later. Therefore, you must COMPLETELY IGNORE style, realism, artistic quality, lighting, shadows, or any visual fidelity concerns. ONLY check structural correctness.**

  **Prompt**: "{prompt}"

  Check ONLY these structural criteria:
  1. Are ALL objects mentioned in the prompt present with the correct count?
  2. Are the COLORS assigned to each object correct as described?
  3. Is each object roughly identifiable as what it represents through its shape?
  4. Are any objects missing, cut off, or positioned outside the visible canvas?
  5. Does the code contain forbidden <text> elements?
  6. Are objects excessively overlapping such that some become unidentifiable?
  7. Are the PRIMARY SUBJECT objects large enough?

  Respond with ONE of three options:
  - PASS  (all checks pass)
  - OPTIMIZE  (objects correct but too small — output <viewBox>minX minY width height</viewBox>)
  - FAIL  (output <feedback>concise description of the structural issue</feedback>)
  """

  def build do
    descriptions = ToolCards.load_all_descriptions()
    desc = Map.get(descriptions, "code_scene_draft", default_description())

    LangChain.Function.new!(%{
      name: "code_scene_draft",
      description: desc,
      parameters_schema: %{
        "type" => "object",
        "properties" => %{
          "prompt" => %{
            "type" => "string",
            "description" => "What to draft, including all object/colour/position constraints."
          },
          "self_review" => %{
            "type" => "boolean",
            "default" => true,
            "description" => "VLM self-review + up to 3 revise rounds. Set false to skip for speed."
          }
        },
        "required" => ["prompt"],
        "additionalProperties" => false
      },
      function: fn args, _ctx ->
        prompt = Map.get(args, "prompt", "")
        self_review = Map.get(args, "self_review", true)

        if not is_binary(prompt) or String.trim(prompt) == "" do
          Jason.encode!(%{"status" => "failed", "error" => "code_scene_draft requires non-empty 'prompt'"})
        else
          try do
            result = run_draft_pipeline(prompt, self_review)

            out_dir = Session.tool_output_dir("code_scene_draft")

            # Write meta.json
            File.write!(Path.join(out_dir, "meta.json"), Jason.encode!(Map.take(result, [
              "prompt", "self_review", "review_verdict", "review_feedback", "draft_png_path"
            ]), pretty: true))

            Session.record_artifacts_in_dir(out_dir, tool_name: "code_scene_draft")

            case Map.get(result, "draft_png_path") do
              draft_path when is_binary(draft_path) and draft_path != "" ->
                Session.record_artifact(draft_path,
                  tool_name: "code_scene_draft",
                  role: "draft",
                  kind: "image",
                  label: "Structural draft"
                )
              _ -> :ok
            end

            Jason.encode!(Map.take(result, [
              "draft_png_path", "svg_description", "review_verdict", "review_passed", "status", "error"
            ]))
          rescue
            e ->
              Jason.encode!(%{"status" => "failed", "error" => "code_scene_draft failed: #{inspect(e)}"})
          end
        end
      end
    })
  end

  # ─── Pipeline ─────────────────────────────────────────────────────────────

  defp run_draft_pipeline(prompt, self_review) do
    out_dir = Session.tool_output_dir("code_scene_draft")

    # Step 1: generate SVG
    Logger.info("[Genclaw.CodeSceneDraft] generating SVG for: #{String.slice(prompt, 0, 80)}")
    gen = generate_svg(prompt)
    svg_code = gen["svg_code"]
    svg_description = gen["svg_description"]
    code_type = gen["code_type"]

    # Save step1 source
    ext = if code_type == "html", do: "html", else: "svg"
    File.write!(Path.join(out_dir, "step1.#{ext}"), svg_code)

    # Step 2: render PNG
    Logger.info("[Genclaw.CodeSceneDraft] rendering PNG (#{code_type})")
    draft_png_path = Path.join(out_dir, "step2_draft.png")
    render_to_png(svg_code, draft_png_path, code_type)

    # Step 3: optional VLM review + revise loop
    {review_passed, review_verdict, review_feedback, svg_code, svg_description, draft_png_path} =
      if self_review do
        run_review_loop(prompt, svg_code, svg_description, code_type, draft_png_path, out_dir)
      else
        {true, "SKIPPED", "", svg_code, svg_description, draft_png_path}
      end

    # Save final SVG
    final_ext = if code_type == "html", do: "html", else: "svg"
    File.write!(Path.join(out_dir, "final.#{final_ext}"), svg_code)

    # Save layout.json
    if svg_description != "" do
      File.write(Path.join(out_dir, "layout.json"), svg_description)
    end

    status = if draft_png_path != "" and File.exists?(draft_png_path), do: "success", else: "failed"

    %{
      "draft_png_path" => draft_png_path,
      "svg_description" => String.slice(svg_description, 0, 2000),
      "review_verdict" => review_verdict,
      "review_passed" => review_passed,
      "review_feedback" => String.slice(review_feedback, 0, 1000),
      "status" => status
    }
  end

  defp run_review_loop(prompt, svg_code, svg_description, code_type, draft_png_path, out_dir) do
    rev = review_draft(prompt, draft_png_path, svg_code)
    review_passed = rev["passed"]
    review_verdict = rev["verdict"]
    review_feedback = rev["feedback"]

    # FAIL → revise loop (up to 3 rounds)
    {review_passed, review_verdict, review_feedback, svg_code, svg_description, draft_png_path} =
      if not review_passed and review_verdict == "FAIL" do
        Enum.reduce_while(1..@max_revise_rounds, {false, review_verdict, review_feedback, svg_code, svg_description, draft_png_path}, fn round, acc ->
          {_, _, feedback, old_svg, _old_desc, _} = acc
          Logger.info("[Genclaw.CodeSceneDraft] FAIL revise round #{round}: #{String.slice(feedback, 0, 100)}")

          revised = revise_svg(prompt, old_svg, feedback)
          new_svg = revised["svg_code"]
          new_desc = revised["svg_description"]

          new_path = Path.join(out_dir, "step3_revise#{round}.png")
          render_to_png(new_svg, new_path, code_type)

          rev = review_draft(prompt, new_path, new_svg)
          passed = rev["passed"]
          verdict = rev["verdict"]
          fb = rev["feedback"]

          if passed do
            {:halt, {true, verdict, fb, new_svg, new_desc, new_path}}
          else
            {:cont, {passed, verdict, fb, new_svg, new_desc, new_path}}
          end
        end)
      else
        {review_passed, review_verdict, review_feedback, svg_code, svg_description, draft_png_path}
      end

    {review_passed, review_verdict, review_feedback, svg_code, svg_description, draft_png_path}
  end

  # ─── SVG Generation ────────────────────────────────────────────────────────

  defp generate_svg(prompt) do
    case call_llm(@svg_system_prompt, "Generate SVG code for:\n#{prompt}") do
      {:ok, raw} ->
        parsed = parse_svg_response(raw)
        if String.length(parsed["svg_code"]) < @min_svg_chars do
          Logger.warning("[Genclaw.CodeSceneDraft] SVG too short (#{String.length(parsed["svg_code"])} chars), retrying")
          generate_svg_retry(prompt, 1)
        else
          parsed
        end

      {:error, e, model} ->
        Logger.error("[Genclaw.CodeSceneDraft] generate failed (model=#{model}): #{inspect(e)}")
        raise RuntimeError, "SVG generation failed: #{inspect(e)}"
    end
  end

  defp generate_svg_retry(prompt, attempt) when attempt <= @max_retries do
    case call_llm(@svg_system_prompt, "Generate SVG code for:\n#{prompt}\n\n(Previous attempt was too short. Please output complete SVG code.)") do
      {:ok, raw} ->
        parsed = parse_svg_response(raw)
        if String.length(parsed["svg_code"]) < @min_svg_chars do
          generate_svg_retry(prompt, attempt + 1)
        else
          parsed
        end

      {:error, e, model} ->
        Logger.warning("[Genclaw.CodeSceneDraft] retry #{attempt} failed (model=#{model}): #{inspect(e)}")
        generate_svg_retry(prompt, attempt + 1)
    end
  end

  defp generate_svg_retry(_, _), do: raise(RuntimeError, "SVG generation failed after #{@max_retries} retries")

  defp revise_svg(prompt, old_svg, feedback) do
    user_msg = """
    Original prompt: #{prompt}

    Previous SVG code:
    ```
    #{old_svg}
    ```

    Review feedback: #{feedback}

    Please generate a corrected SVG that fixes all the issues.
    """

    case call_llm(@svg_revise_system_prompt, user_msg) do
      {:ok, raw} -> parse_svg_response(raw)
      {:error, e, model} ->
        Logger.error("[Genclaw.CodeSceneDraft] revise failed (model=#{model}): #{inspect(e)}")
        raise RuntimeError, "SVG revision failed: #{inspect(e)}"
    end
  end

  defp parse_svg_response(raw) do
    {planning, legend, svg_code} = extract_parts(raw)

    # Strip markdown fences
    svg_description = legend
    |> String.replace(~r/^```(?:json)?\s*/, "")
    |> String.replace(~r/\s*```$/, "")
    |> String.trim()

    svg_code = svg_code
    |> String.replace(~r/^```(?:svg|html|xml)?\s*/, "")
    |> String.replace(~r/\s*```$/, "")
    |> String.trim()

    # Strip leftover delimiters
    svg_code = svg_code
    |> String.replace(@planning_delim, "")
    |> String.replace(@legend_delim, "")
    |> String.replace(@svg_delim, "")
    |> String.replace(~r/={2,}(?:PLANNING|LEGEND|SVG)[_\s]*(?:START|CODE)[_\s]*={2,}/, "")

    # Extract from first <svg> or <html> tag
    svg_code = extract_code_block(svg_code)

    # Remove <text> elements
    svg_code = Regex.replace(~r/<text[^>]*>.*?<\/text>/s, svg_code, "")

    code_type = if String.contains?(String.downcase(svg_code), "<html") or
                    String.contains?(String.downcase(svg_code), "<!doctype"),
                  do: "html", else: "svg"

    %{
      "svg_code" => svg_code,
      "svg_description" => svg_description,
      "svg_planning" => planning,
      "code_type" => code_type
    }
  end

  defp extract_parts(raw) do
    has_planning = String.contains?(raw, @planning_delim)
    has_legend = String.contains?(raw, @legend_delim)
    has_svg = String.contains?(raw, @svg_delim)

    cond do
      has_planning and has_svg ->
        after_planning = String.split(raw, @planning_delim, parts: 2) |> List.last()
        if has_legend do
          [planning_part, after_legend] = String.split(after_planning, @legend_delim, parts: 2)
          [legend_part, code_part] = String.split(after_legend, @svg_delim, parts: 2)
          {String.trim(planning_part), String.trim(legend_part), String.trim(code_part)}
        else
          [planning_part, code_part] = String.split(after_planning, @svg_delim, parts: 2)
          {String.trim(planning_part), "", String.trim(code_part)}
        end

      has_svg ->
        [desc_part, code_part] = String.split(raw, @svg_delim, parts: 2)
        {"", String.trim(desc_part), String.trim(code_part)}

      true ->
        {"", "", String.trim(raw)}
    end
  end

  defp extract_code_block(code) do
    svg_start = Regex.run(~r/<svg[\s>]/, code, return: :index)
    html_start = Regex.run(~r/(?:<html|<!doctype)/i, code, return: :index)

    case {svg_start, html_start} do
      {nil, nil} ->
        code

      {[{s, _}], nil} ->
        binary_part(code, s, byte_size(code) - s)

      {nil, [{s, _}]} ->
        binary_part(code, s, byte_size(code) - s)

      {[{s1, _}], [{s2, _}]} ->
        s = min(s1, s2)
        binary_part(code, s, byte_size(code) - s)
    end
  end

  # ─── VLM Review ────────────────────────────────────────────────────────────

  defp review_draft(prompt, png_path, svg_code) do
    review_text = String.replace(@review_prompt, "{prompt}", prompt)
    |> String.replace("{svg_code}", String.slice(svg_code, 0, 8000))

    case BuiltInRegistry.call_tool("look", "look", %{
      "image" => png_path,
      "prompt" => review_text
    }) do
      {:ok, result} ->
        reply = extract_text(result)
        parse_review_reply(reply)

      {:error, e} ->
        Logger.warning("[Genclaw.CodeSceneDraft] review failed: #{inspect(e)}")
        %{"passed" => true, "verdict" => "SKIPPED", "feedback" => ""}
    end
  end

  defp parse_review_reply(reply) do
    reply = String.trim(reply)

    cond do
      String.starts_with?(reply, "PASS") ->
        %{"passed" => true, "verdict" => "PASS", "feedback" => ""}

      String.starts_with?(reply, "OPTIMIZE") ->
        %{"passed" => false, "verdict" => "OPTIMIZE", "feedback" => "Objects too small, needs viewBox crop"}

      String.starts_with?(reply, "FAIL") ->
        feedback = case Regex.run(~r/<feedback>\s*(.*?)\s*<\/feedback>/s, reply) do
          [_, f] -> f
          _ -> String.replace(reply, "FAIL", "") |> String.trim()
        end
        %{"passed" => false, "verdict" => "FAIL", "feedback" => feedback}

      true ->
        %{"passed" => true, "verdict" => "SKIPPED", "feedback" => ""}
    end
  end

  # ─── Rendering ─────────────────────────────────────────────────────────────

  defp render_to_png(code, output_path, code_type) do
    File.mkdir_p!(Path.dirname(output_path))

    html_content = if code_type == "html", do: code, else: wrap_svg_in_html(code)

    # Parse canvas size from SVG
    {w, h} = parse_canvas_size(code)
    Logger.info("[Genclaw.CodeSceneDraft] render #{code_type} #{w}x#{h} -> #{output_path}")

    tmp_html = Path.join(System.tmp_dir!(), "genclaw_render_#{:crypto.strong_rand_bytes(4) |> Base.encode16(case: :lower)}.html")
    File.write!(tmp_html, html_content)

    try do
      chrome = chrome_path()

      args = [
        "--headless",
        "--disable-gpu",
        "--no-sandbox",
        "--disable-dev-shm-usage",
        "--screenshot=#{output_path}",
        "--window-size=#{w},#{h}",
        "file://#{tmp_html}"
      ]

      case Exile.stream([chrome | args], stderr: :consume) |> Enum.reduce({0, ""}, fn
        {:stdout, _}, {code, acc} -> {code, acc}
        {:exit, code}, {_, acc} -> {code, acc}
      end) do
        {0, _} ->
          Logger.info("[Genclaw.CodeSceneDraft] render done: #{output_path}")
          output_path

        {code, _} ->
          Logger.warning("[Genclaw.CodeSceneDraft] chrome exited #{code}, trying qlmanage fallback")
          render_with_qlmanage(tmp_html, output_path)
      end
    rescue
      e ->
        Logger.warning("[Genclaw.CodeSceneDraft] chrome render failed: #{inspect(e)}, trying qlmanage")
        render_with_qlmanage(tmp_html, output_path)
    after
      File.rm(tmp_html)
    end
  end

  defp render_with_qlmanage(input_file, output_path) do
    out_dir = Path.dirname(output_path)
    File.mkdir_p!(out_dir)

    {_, 0} = System.cmd("qlmanage", ["-t", "-s", "1024", "-o", out_dir, input_file])

    # qlmanage outputs <basename>.png
    ql_output = Path.join(out_dir, Path.basename(input_file, ".html") <> ".png")

    if File.exists?(ql_output) do
      File.rename(ql_output, output_path)
      output_path
    else
      raise RuntimeError, "rendering failed: neither chrome nor qlmanage produced output"
    end
  end

  defp wrap_svg_in_html(svg_code) do
    """
    <!DOCTYPE html>
    <html><head><meta charset="utf-8">
    <style>
    body { margin: 0; display: flex; justify-content: center; align-items: center; min-height: 100vh; background: white; }
    svg { max-width: 100%; max-height: 100vh; }
    </style>
    </head><body>
    #{svg_code}
    </body></html>
    """
  end

  defp parse_canvas_size(code, default \\ {1024, 1024}) do
    max_side = 2048
    min_side = 64

    svg_tag = case Regex.run(~r/<svg\b[^>]*>/, code, return: :index) do
      [{s, len}] -> String.slice(code, s, len)
      _ -> code
    end

    w = extract_num(svg_tag, "width")
    h = extract_num(svg_tag, "height")

    {w, h} =
      if (is_nil(w) or is_nil(h)) do
        case Regex.run(~r/viewBox\s*=\s*["']\s*[-0-9.]+\s+[-0-9.]+\s+([0-9.]+)\s+([0-9.]+)/, svg_tag) do
          [_, vw, vh] ->
            {w || String.to_float(vw), h || String.to_float(vh)}
          _ ->
            {w || elem(default, 0), h || elem(default, 1)}
        end
      else
        {w, h}
      end

    wi = max(min_side, min(round(w), max_side))
    hi = max(min_side, min(round(h), max_side))
    {wi, hi}
  end

  defp extract_num(str, attr) do
    case Regex.run(~r/#{attr}\s*=\s*["']?\s*([0-9]+(?:\.[0-9]+)?)/, str) do
      [_, n] -> String.to_float(n)
      _ -> nil
    end
  end

  defp chrome_path do
    System.get_env("CHROME_PATH") ||
      "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
  end

  # ─── LLM ───────────────────────────────────────────────────────────────────

  defp call_llm(system_prompt, user_prompt) do
    case LlmConfigServer.get_default_llm_config() do
      {:ok, llm_config} ->
        model = build_langchain_model(llm_config)

        messages = [
          LangChain.Message.new_system!(system_prompt),
          LangChain.Message.new_user!(user_prompt)
        ]

        chain =
          LangChain.Chains.LLMChain.new!(%{llm: model})
          |> LangChain.Chains.LLMChain.add_messages(messages)

        case LangChain.Chains.LLMChain.run(chain) do
          {:ok, chain} ->
            case List.last(chain.messages) do
              %LangChain.Message{role: :assistant, content: content} ->
                {:ok, extract_text(content)}
              _ ->
                {:error, "no assistant response", llm_config[:model]}
            end

          {:error, _chain, reason} ->
            {:error, reason, llm_config[:model]}
        end

      {:error, e} ->
        {:error, e, nil}
    end
  end

  defp extract_text(content) when is_binary(content), do: content
  defp extract_text(content) when is_list(content) do
    content |> Enum.map_join("", fn
      text when is_binary(text) -> text
      %LangChain.Message.ContentPart{type: :text, content: c} -> c || ""
      %LangChain.Message.ContentPart{content: c} when is_binary(c) -> c
      %{content: c} when is_binary(c) -> c
      _ -> ""
    end)
  end
  defp extract_text(%LangChain.Message.ContentPart{type: :text, content: c}), do: c || ""
  defp extract_text(%LangChain.Message.ContentPart{content: c}) when is_binary(c), do: c
  defp extract_text(_), do: ""

  defp build_langchain_model(config) do
    [provider, model_name] = String.split(config[:model], "/", parts: 2)

    base = %{model: model_name, api_key: config[:api_key], receive_timeout: @llm_receive_timeout}

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

  defp default_description do
    "Code-draw an SVG draft to lock precise multi-object geometry (count, colour, position). Output is a draft PNG; refine it to photoreal with i2i."
  end
end
