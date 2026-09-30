defmodule Exhub.Toonflow.Prompts do
  @moduledoc """
  Prompt template loading and rendering for the Toonflow pipeline.

  Templates live in `priv/toonflow/prompts/<name>.md` and use `{{key}}`
  placeholders, substituted by `render_string/2`. `render/2` returns
  `{:ok, text}` (or `{:error, reason}` when the template is missing).
  """

  @doc "Directory holding the prompt templates."
  @spec dir() :: String.t()
  def dir, do: Path.join(:code.priv_dir(:exhub), "toonflow/prompts")

  @doc "Load a template by name (without the `.md` suffix)."
  @spec load(String.t()) :: {:ok, String.t()} | {:error, term()}
  def load(name) when is_binary(name), do: File.read(Path.join(dir(), name <> ".md"))

  @doc "Load and render a template, substituting `{{key}}` placeholders."
  @spec render(String.t(), map()) :: {:ok, String.t()} | {:error, term()}
  def render(name, assigns \\ %{}) do
    with {:ok, template} <- load(name) do
      {:ok, render_string(template, assigns)}
    end
  end

  @doc "Render a template string, substituting `{{key}}` placeholders."
  @spec render_string(String.t(), map()) :: String.t()
  def render_string(template, assigns) when is_binary(template) and is_map(assigns) do
    Enum.reduce(assigns, template, fn {key, value}, acc ->
      String.replace(acc, "{{#{key}}}", stringify(value))
    end)
  end

  defp stringify(nil), do: ""
  defp stringify(value) when is_binary(value), do: value
  defp stringify(value), do: to_string(value)
end
