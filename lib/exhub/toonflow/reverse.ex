defmodule Exhub.Toonflow.Reverse do
  @moduledoc """
  Reverse-engineer a subject's description from its reference sheet.

  A sheet uploaded by the user is the look the later frames should follow, but
  the *text* is what the storyboard hands to the image model — so the picture is
  read back into a description (`appearance` for a character, `description` for a
  scene or prop) and saved with `Exhub.Toonflow.Assets.update_text/5`.

  The prompts are deliberately narrower than "describe this image":

    * a character sheet yields the character's **constant** look and the 人设 it
      conveys — never a pose, expression, camera angle or background, which
      would leak one frame into every shot;
    * a scene yields the environment itself — not the actors, framing or
      resolution-dependent detail that cannot travel to a new camera setup.

  `clean/1` and `prompt/1` are pure and unit-tested; `infer/4` goes through the
  injectable `Exhub.Toonflow.Vision` seam.
  """

  alias Exhub.Toonflow
  alias Exhub.Toonflow.{Assets, Config, Store, Vision}

  # Long enough for a full sheet description, short enough to stay a prompt.
  @max_chars 600

  @prompts %{
    "character" => """
    这是短剧的角色定妆图。请输出一段可直接用于 AI 生成的角色设定描述（100-180 字，中文，一段话），覆盖：年龄感、性别、体型、发型与发色、面部特征（脸型、眉眼、鼻子）、瞳色、服装与配色、随身配饰或标志性细节，以及由外形传达出的气质与人设倾向。
    只描述角色本身恒定的外观与气质；不要描述动作、姿势、表情幅度、镜头角度、画面构图、背景环境、画风与分辨率。不要加小标题、前后缀或解释，直接给描述。
    """,
    "scene" => """
    这是短剧的场景设定图。请输出一段可直接用于 AI 生成的场景描述（100-180 字，中文，一段话），覆盖：空间类型与用途、建筑结构与体量、时代与地域特征、主要材质与陈设、整体色调、光线来源与氛围、天气与时间感。
    只描述环境本身可复用的特征；不要描述画面中的人物、动作、镜头机位、构图、景深、分辨率、水印或画风。不要加小标题、前后缀或解释，直接给描述。
    """,
    "prop" => """
    这是短剧的道具设定图。请输出一段可直接用于 AI 生成的道具描述（60-120 字，中文，一段话），覆盖：物件类型与用途、尺寸感、材质与工艺、颜色与磨损痕迹、标志性纹样或细节。
    只描述道具本身；不要描述持握动作、人物、镜头角度、构图、背景与画风。不要加小标题、前后缀或解释，直接给描述。
    """
  }

  @doc "The vision prompt for a subject kind (`character` | `scene` | `prop`). Pure."
  @spec prompt(String.t()) :: String.t()
  def prompt(kind) when is_binary(kind), do: Map.fetch!(@prompts, kind)

  @doc """
  Read a subject's current reference sheet and save the inferred description.

  `opts`: `:model` (vision model, default `media.vision_model`). Returns
  `{:ok, %{"kind", "name", "text", "path"}}` or `{:error, reason}` — e.g.
  `:no_reference` when the subject has no sheet yet, `{:vision_failed, reason}`
  when the model call fails, `:empty_vision_result` when it answered nothing
  usable.
  """
  @spec infer(String.t(), String.t(), String.t(), keyword(), GenServer.server()) ::
          {:ok, map()} | {:error, term()}
  def infer(project, kind, name, opts \\ [], server \\ Store) do
    kind = normalize_kind(kind)
    name = Toonflow.blank(name)
    model = Keyword.get(opts, :model) || Config.vision_model()

    with :ok <- validate(kind, name),
         {:ok, path} <- Assets.reference_path(project, kind, name, server),
         :ok <- ensure_file(path),
         {:ok, raw} <- Vision.analyze(path, prompt: prompt(kind), model: model),
         {:ok, text} <- usable(clean(raw)),
         {:ok, _saved} <- Assets.update_text(project, kind, name, text, server) do
      {:ok, %{"kind" => kind, "name" => name, "text" => text, "path" => path}}
    end
  end

  @doc """
  Normalize a vision reply into one description paragraph.

  Drops Markdown fences, a leading `描述：` label, surrounding quotes, collapses
  whitespace and caps the length. Returns `nil` when nothing is left. Pure.
  """
  @spec clean(String.t() | nil) :: String.t() | nil
  def clean(text) when is_binary(text) do
    cleaned =
      text
      |> String.replace("```", "")
      |> String.replace(~r/^\s*(?:描述|设定|角色描述|场景描述|道具描述|外观描述)\s*[:：]\s*/u, "")
      |> String.replace(~r/\s+/u, " ")
      |> String.trim()
      |> String.replace(~r/^["'“”‘’]+|["'“”‘’]+$/u, "")
      |> String.slice(0, @max_chars)
      |> String.trim()

    if cleaned == "", do: nil, else: cleaned
  end

  def clean(_text), do: nil

  # --- helpers ---

  defp normalize_kind(kind) when is_binary(kind) do
    kind = kind |> String.trim() |> String.downcase()
    if Map.has_key?(@prompts, kind), do: kind, else: nil
  end

  defp normalize_kind(_kind), do: nil

  defp validate(nil, _name), do: {:error, :invalid_reference_kind}
  defp validate(_kind, nil), do: {:error, :missing_name}
  defp validate(_kind, _name), do: :ok

  defp ensure_file(path) do
    if File.exists?(path), do: :ok, else: {:error, :reference_missing}
  end

  defp usable(nil), do: {:error, :empty_vision_result}
  defp usable(text), do: {:ok, text}
end
