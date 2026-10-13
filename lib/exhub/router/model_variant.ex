defmodule Exhub.Router.ModelVariant do
  @moduledoc """
  Encodes per-request reasoning controls in the model name (Qwen family).

  Some Qwen chat models accept reasoning controls as request-body extras —
  `chat_template_kwargs` (`enable_thinking`, `preserve_thinking`) and a
  top-level `reasoning_effort` (`xhigh`, `medium`, `low`). This module lets a
  client select them through the model name itself, so no body edit is needed:

      qwen3.6-flash--no-think
      qwen3.6-flash--no-think+effort-medium
      qwen3.6-flash--effort-xhigh
      qwen3.6-flash--kv-top_p=0.9

  ## Grammar

  The name is `<base>--<modifier>[+<modifier>...]`. `--` separates the upstream
  model name from one or more `+`-joined modifiers:

  | Modifier | Effect |
  |----------|--------|
  | `think` / `no-think` | `chat_template_kwargs.enable_thinking` (and `preserve_thinking`) |
  | `keep-think` / `no-keep-think` | `chat_template_kwargs.preserve_thinking` |
  | `effort-low` / `effort-medium` / `effort-xhigh` | top-level `reasoning_effort` |
  | `kv-<key>=<value>` | extra `chat_template_kwargs` entry; `true`/`false` become booleans |

  Later modifiers win over earlier ones for the same key.

  ## Routing, billing and precedence

  The suffix is stripped before the name reaches routing
  (`Exhub.Router.Config`) and the upstream request body (`Exhub.ProxyPlug`), so
  `qwen3.6-flash--no-think` routes and bills exactly like `qwen3.6-flash`.

  Only Qwen-family base names (`qwen*`) are recognised. Any other name — or one
  whose modifiers do not all parse — is returned unchanged, so unrelated models
  that contain `--` keep working and are forwarded verbatim.

  The suffix is the explicit per-request override, so its `reasoning_effort`
  wins over `reasoning_effort` rules configured in `Exhub.Router.Settings`
  (`~/.config/exhub/router.json`).
  """

  @separator "--"

  @type modifier ::
          {:enable_thinking, boolean()}
          | {:preserve_thinking, boolean()}
          | {:reasoning_effort, String.t()}
          | {:kv, {String.t(), String.t() | boolean()}}

  @doc """
  Splits `model` into its base name and recognised modifiers.

  Returns `{model, []}` when the name carries no recognised Qwen variant suffix.

  ## Examples

      iex> Exhub.Router.ModelVariant.split("qwen3.6-flash")
      {"qwen3.6-flash", []}

      iex> Exhub.Router.ModelVariant.split("qwen3.6-flash--no-think")
      {"qwen3.6-flash", [enable_thinking: false, preserve_thinking: false]}

      iex> Exhub.Router.ModelVariant.split("kimi-k3--no-think")
      {"kimi-k3--no-think", []}
  """
  @spec split(String.t()) :: {String.t(), [modifier()]}
  def split(model) when is_binary(model) do
    if String.contains?(model, @separator) do
      do_split(model)
    else
      {model, []}
    end
  end

  def split(model), do: {model, []}

  @doc """
  Returns the base model name, with a recognised variant suffix stripped.

  ## Examples

      iex> Exhub.Router.ModelVariant.normalize("qwen3.6-flash--no-think")
      "qwen3.6-flash"

      iex> Exhub.Router.ModelVariant.normalize("deepseek-v3")
      "deepseek-v3"
  """
  @spec normalize(String.t()) :: String.t()
  def normalize(model) when is_binary(model), do: elem(split(model), 0)
  def normalize(model), do: model

  @doc "Returns whether `model` carries a recognised variant suffix."
  @spec variant?(String.t()) :: boolean()
  def variant?(model) when is_binary(model), do: elem(split(model), 1) != []
  def variant?(_model), do: false

  @doc "Returns whether `model`'s base name is a Qwen-family model."
  @spec qwen?(String.t()) :: boolean()
  def qwen?(model) when is_binary(model), do: model |> normalize() |> qwen_base?()
  def qwen?(_model), do: false

  @doc """
  Returns the `reasoning_effort` requested by the name, or `nil`.

  ## Examples

      iex> Exhub.Router.ModelVariant.reasoning_effort("qwen3.6-flash--effort-medium")
      "medium"

      iex> Exhub.Router.ModelVariant.reasoning_effort("qwen3.6-flash--no-think")
      nil
  """
  @spec reasoning_effort(String.t()) :: String.t() | nil
  def reasoning_effort(model) when is_binary(model) do
    case split(model) do
      {_base, []} ->
        nil

      {_base, mods} ->
        Enum.find_value(mods, fn
          {:reasoning_effort, effort} -> effort
          _ -> nil
        end)
    end
  end

  @doc """
  Applies `model`'s variant modifiers to an OpenAI-style request `body`.

  Returns `body` unchanged when the name is not a Qwen variant. Existing
  `chat_template_kwargs` entries are preserved and overridden per modifier.

  ## Examples

      iex> Exhub.Router.ModelVariant.apply_params(%{"model" => "x"}, "qwen3.6-flash--no-think")["chat_template_kwargs"]
      %{"enable_thinking" => false, "preserve_thinking" => false}

      iex> Exhub.Router.ModelVariant.apply_params(%{"model" => "x"}, "deepseek-v3--no-think")
      %{"model" => "x"}
  """
  @spec apply_params(map(), String.t()) :: map()
  def apply_params(body, model) when is_map(body) and is_binary(model) do
    case split(model) do
      {_base, []} -> body
      {base, mods} -> if qwen_base?(base), do: merge(body, mods), else: body
    end
  end

  def apply_params(body, _model), do: body

  # --- parsing ---

  defp do_split(model) do
    case String.split(model, @separator, parts: 2) do
      [base, mods] when base != "" and mods != "" ->
        with true <- qwen_base?(base),
             {:ok, parsed} <- parse_mods(mods) do
          {base, parsed}
        else
          _ -> {model, []}
        end

      _ ->
        {model, []}
    end
  end

  defp parse_mods(mods) do
    mods
    |> String.split("+")
    |> Enum.reduce_while({:ok, []}, fn raw, {:ok, acc} ->
      case parse_mod(raw) do
        {:ok, parsed} -> {:cont, {:ok, acc ++ parsed}}
        :error -> {:halt, :error}
      end
    end)
  end

  defp parse_mod("think"), do: {:ok, [enable_thinking: true, preserve_thinking: true]}
  defp parse_mod("no-think"), do: {:ok, [enable_thinking: false, preserve_thinking: false]}
  defp parse_mod("keep-think"), do: {:ok, [preserve_thinking: true]}
  defp parse_mod("no-keep-think"), do: {:ok, [preserve_thinking: false]}
  defp parse_mod("effort-low"), do: {:ok, [reasoning_effort: "low"]}
  defp parse_mod("effort-medium"), do: {:ok, [reasoning_effort: "medium"]}
  defp parse_mod("effort-xhigh"), do: {:ok, [reasoning_effort: "xhigh"]}

  defp parse_mod("kv-" <> rest) do
    case String.split(rest, "=", parts: 2) do
      [key, value] when key != "" -> {:ok, [kv: {key, coerce(value)}]}
      _ -> :error
    end
  end

  defp parse_mod(_raw), do: :error

  defp coerce("true"), do: true
  defp coerce("false"), do: false
  defp coerce(value), do: value

  # --- application ---

  defp merge(body, mods) do
    {kwargs, effort} =
      Enum.reduce(mods, {%{}, nil}, fn
        {:enable_thinking, value}, {kwargs, effort} ->
          {Map.put(kwargs, "enable_thinking", value), effort}

        {:preserve_thinking, value}, {kwargs, effort} ->
          {Map.put(kwargs, "preserve_thinking", value), effort}

        {:reasoning_effort, value}, {kwargs, _effort} ->
          {kwargs, value}

        {:kv, {key, value}}, {kwargs, effort} ->
          {Map.put(kwargs, key, value), effort}
      end)

    body
    |> put_chat_template_kwargs(kwargs)
    |> put_reasoning_effort(effort)
  end

  defp put_chat_template_kwargs(body, kwargs) when map_size(kwargs) == 0, do: body

  defp put_chat_template_kwargs(body, kwargs) do
    existing =
      case Map.get(body, "chat_template_kwargs") do
        value when is_map(value) -> value
        _ -> %{}
      end

    Map.put(body, "chat_template_kwargs", Map.merge(existing, kwargs))
  end

  defp put_reasoning_effort(body, nil), do: body
  defp put_reasoning_effort(body, effort), do: Map.put(body, "reasoning_effort", effort)

  defp qwen_base?(base), do: base |> String.downcase() |> String.starts_with?("qwen")
end
