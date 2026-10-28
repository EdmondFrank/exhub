defmodule Exhub.Toonflow.ShotAssist do
  @moduledoc """
  One-click AI writing help for the canvas' shot inspector.

  The inspector edits one field at a time; `assist/4` takes the text currently in
  that field (including unsaved edits) plus the action the user picked and
  returns a rewritten version. The result is *not* persisted — the canvas puts
  it back into the form and the user saves when satisfied.

  The call runs on the configured director model (`toonflow.agents.director`)
  through `Exhub.Toonflow.LLM`, and is grounded in the shot's own fields, the
  shots on either side (for continuity), the cast's appearances and the
  project's worldview. Prompt templates are `shot.assist.system` /
  `shot.assist.user`, so a project can override them like every other stage.

  `fields/0`, `actions/0` and `field_label/1` are pure and shared with the
  canvas, so the available buttons and the accepted fields never drift apart.
  """

  alias Exhub.Toonflow
  alias Exhub.Toonflow.{Assets, Config, Keyframe, LLM, Prompts, Store, Storyboard, Worldview}

  # Only prose fields get AI buttons. `size`/`motion`/`speaker` are closed
  # vocabularies and `camera`/`lighting` are short cinematography labels, so a
  # rewrite there would either break the select or overshoot the field.
  @fields ~w(scene description dialogue prompt keyframe_start_prompt keyframe_end_prompt)

  @actions ~w(polish continue expand shorten)

  @field_labels %{
    "scene" => "场景",
    "description" => "画面描述",
    "dialogue" => "台词",
    "prompt" => "文生图提示词",
    "keyframe_start_prompt" => "首帧提示词",
    "keyframe_end_prompt" => "尾帧提示词"
  }

  @action_labels %{
    "polish" => "润色",
    "continue" => "续写",
    "expand" => "扩写",
    "shorten" => "精简"
  }

  @doc "The fields the inspector may hand to the assistant. Pure."
  @spec fields() :: [String.t()]
  def fields, do: @fields

  @doc "The actions the inspector offers. Pure."
  @spec actions() :: [String.t()]
  def actions, do: @actions

  @doc "A field's human label (the one used in the prompt). Pure."
  @spec field_label(String.t()) :: String.t()
  def field_label(field), do: Map.get(@field_labels, field, field)

  @doc "An action's human label (the one used in the prompt). Pure."
  @spec action_label(String.t()) :: String.t()
  def action_label(action), do: Map.get(@action_labels, action, action)

  @doc """
  Rewrite `text` for one shot field with the configured director model.

  `opts`:

    * `:field` — one of `fields/0` (required).
    * `:action` — one of `actions/0` (default `"polish"`).
    * `:text` — the text currently in the editor (required).
    * `:instruction` — optional extra guidance from the user.

  Returns `{:ok, %{"field", "action", "text", "model"}}` or `{:error, reason}`
  (`:missing_field`, `:missing_text`, `{:invalid_field, field}`,
  `{:invalid_action, action}`, `:shot_not_found`, `{:llm_failed, reason}`).
  """
  @spec assist(String.t(), String.t(), keyword(), GenServer.server()) ::
          {:ok, map()} | {:error, term()}
  def assist(project, shot_id, opts \\ [], server \\ Store) do
    field = Toonflow.blank(Keyword.get(opts, :field))
    action = Toonflow.blank(Keyword.get(opts, :action)) || "polish"
    text = Toonflow.blank(Keyword.get(opts, :text))
    instruction = Toonflow.blank(Keyword.get(opts, :instruction))
    model = Config.agent_model("director")

    with :ok <- validate_field(field),
         :ok <- validate_action(action),
         :ok <- validate_text(text),
         {:ok, shot} <- Storyboard.get_shot(project, shot_id, server),
         {:ok, characters} <- Assets.list_characters(project, [], server),
         {:ok, neighbors} <- neighbors(project, shot, server),
         {:ok, worldview} <- Worldview.digest_for(project, server),
         {:ok, system} <- Prompts.resolve("shot.assist.system", %{}, project, server),
         {:ok, user} <-
           Prompts.resolve(
             "shot.assist.user",
             assigns(field, action, text, instruction, shot, characters, neighbors, worldview),
             project,
             server
           ),
         {:ok, raw} <- LLM.call_llm(system, user, model_opts(model)) do
      {:ok,
       %{
         "field" => field,
         "action" => action,
         "text" => output(action, raw, text),
         "model" => model || "default"
       }}
    end
  end

  defp validate_field(field) when is_binary(field) do
    if field in @fields, do: :ok, else: {:error, {:invalid_field, field}}
  end

  defp validate_field(_field), do: {:error, :missing_field}

  defp validate_action(action) when is_binary(action) do
    if action in @actions, do: :ok, else: {:error, {:invalid_action, action}}
  end

  defp validate_action(_action), do: {:error, :missing_action}

  defp validate_text(nil), do: {:error, :missing_text}
  defp validate_text(_text), do: :ok

  defp model_opts(nil), do: []
  defp model_opts(model), do: [model: model]

  defp assigns(field, action, text, instruction, shot, characters, neighbors, worldview) do
    %{
      "field" => field_label(field),
      "action" => action_label(action),
      "text" => text,
      "instructions" => instruction || "（无）",
      "shot" => shot_digest(shot),
      "neighbors" => neighbors_digest(neighbors),
      "characters" => characters_digest(characters),
      "worldview" => blank_digest(worldview)
    }
  end

  defp neighbors(project, shot, server) do
    case Storyboard.list_shots(project, [script_id: shot["script_id"]], server) do
      {:ok, shots} -> {:ok, Keyframe.adjacent(shots, shot["id"])}
      {:error, reason} -> {:error, reason}
    end
  end

  defp shot_digest(shot) do
    [
      {"场景", shot["scene"]},
      {"画面", shot["shot_desc"]},
      {"景别", shot["size"]},
      {"机位", shot["camera"]},
      {"运镜", shot["motion"]},
      {"光线", shot["lighting"]},
      {"角色", Enum.join(Storyboard.characters_of(shot), "、")},
      {"说话人", meta(shot, "speaker")},
      {"台词", meta(shot, "dialogue")},
      {"提示词", shot["prompt"]},
      {"首帧", Keyframe.keyframe_prompt(shot, "start")},
      {"尾帧", Keyframe.keyframe_prompt(shot, "end")}
    ]
    |> Enum.flat_map(fn {label, value} ->
      case Toonflow.blank(value) do
        nil -> []
        value -> ["#{label}：#{value}"]
      end
    end)
    |> Enum.join("\n")
  end

  defp neighbors_digest({nil, nil}), do: "（无相邻镜头）"

  defp neighbors_digest({previous, next}) do
    [{"前一个镜头", previous}, {"后一个镜头", next}]
    |> Enum.flat_map(fn {label, shot} ->
      case shot do
        nil -> []
        shot -> ["#{label}（##{shot["idx"]}）：#{neighbor_text(shot)}"]
      end
    end)
    |> Enum.join("\n")
  end

  defp neighbor_text(shot) do
    Toonflow.blank(shot["shot_desc"]) || Toonflow.blank(shot["scene"]) || "（无描述）"
  end

  defp characters_digest([]), do: "（未提取角色）"

  defp characters_digest(characters) do
    characters
    |> Enum.map_join("\n", fn character ->
      appearance = character["appearance"] || character["role"] || ""
      "- #{character["name"]}：#{appearance}"
    end)
  end

  defp blank_digest(nil), do: "（未设置）"

  defp blank_digest(text) when is_binary(text),
    do: if(String.trim(text) == "", do: "（未设置）", else: text)

  defp blank_digest(_), do: "（未设置）"

  defp meta(shot, key), do: (shot["meta"] || %{})[key]

  # A continuation is appended to the editor's text, so a model that echoes the
  # whole paragraph would duplicate it. Strip a leading copy of the original.
  defp output("continue", raw, text) do
    raw
    |> clean()
    |> strip_prefix(String.trim(text))
  end

  defp output(_action, raw, _text), do: clean(raw)

  defp strip_prefix(result, ""), do: result

  defp strip_prefix(result, text) do
    if String.starts_with?(result, text) do
      result |> String.replace_prefix(text, "") |> String.trim_leading()
    else
      result
    end
  end

  # The prompts ask for bare text, but a model still sometimes wraps its reply in
  # a fence or quotes. Strip both so the canvas does not show markdown debris.
  defp clean(raw) when is_binary(raw) do
    raw
    |> String.replace(~r/^\s*```[a-zA-Z0-9_-]*\s*\n?/, "")
    |> String.replace(~r/\n?\s*```\s*$/, "")
    |> String.trim()
    |> strip_quotes()
  end

  defp clean(raw), do: to_string(raw)

  defp strip_quotes(text) do
    if String.starts_with?(text, "\"") and String.ends_with?(text, "\"") and
         String.length(text) >= 2 do
      text |> String.slice(1, String.length(text) - 2) |> String.trim()
    else
      text
    end
  end
end
