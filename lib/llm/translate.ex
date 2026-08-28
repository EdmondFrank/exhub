defmodule Exhub.Llm.Translate do
  alias LangChain.Message

  alias Exhub.Llm.Chain
  alias Exhub.Llm.LlmConfigServer

  @doc """
  Translates `content` into `to_lang`, returning only the translated text.

  By default the default LLM model (the one used by `Exhub.Llm.Chain`) is
  used. A dedicated translation model can be configured in two ways:

    * app env `:exhub, :translate_llm` (e.g.
      `config :exhub, translate_llm: "codestral/codestral-latest"`), or
    * per-call via `opts[:llm]`, which takes precedence over the app env.

  When no custom model is configured — or the configured name is not a valid
  LLM name — the default LLM model is used as a fallback.
  """
  def execute(content, to_lang, opts \\ []) do
    llm_chain = build_llm_chain(opts)
    to_lang = if to_lang |> String.trim() |> String.length() == 0, do: "EN", else: to_lang

    initial_messages = [
      Message.new_system!("""
      You are a helpful AI translator, expert in converting user input between ``` and ``` into a specific language and returning only the translated content. Be careful not to make any grammatical or spelling errors, and please help refine the translation."
      """),
      Message.new_user!(
        "help me translate ```#{content}``` to `#{to_lang}`, only return the translated content."
      )
    ]

    Chain.execute(llm_chain, initial_messages)
  end

  @doc false
  # Resolves the custom LLM name for a translation request: opts[:llm] wins
  # over the `:exhub, :translate_llm` app env. Returns nil when no custom
  # model is requested (the default LLM should be used).
  def resolve_llm_name(opts) do
    opts
    |> Keyword.get(:llm)
    |> case do
      nil -> Application.get_env(:exhub, :translate_llm)
      name -> name
    end
    |> normalize_name()
  end

  defp build_llm_chain(opts) do
    case resolve_llm_name(opts) do
      nil ->
        Chain.create_llm_chain()

      name ->
        if llm_configured?(name) do
          Chain.create_llm_chain(name)
        else
          Chain.create_llm_chain()
        end
    end
  end

  defp llm_configured?(llm_name) do
    Process.whereis(LlmConfigServer) != nil and
      match?({:ok, _}, LlmConfigServer.get_llm_config(llm_name))
  end

  defp normalize_name(name) when is_binary(name) do
    case String.trim(name) do
      "" -> nil
      trimmed -> trimmed
    end
  end

  defp normalize_name(_), do: nil
end
