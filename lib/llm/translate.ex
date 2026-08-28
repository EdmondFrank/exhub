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

  @system_prompt """
  You are a helpful AI assistant that corrects grammar, spelling, and punctuation errors. Respond with ONLY the final corrected text result. No explanations, no markdown formatting, no bullet points, no preambles, no quotes around the result. Just the direct text output. CRITICAL: Preserve the original language of the input text - if the input is in a specific language, respond in that same language.
  """

  @doc """
  Fixes grammar, spelling, and punctuation errors in `content` while keeping
  the original meaning, tone, and style (minimal-change correction).

  Uses the same model selection as `execute/3` — the optional custom
  `:exhub, :translate_llm` model, falling back to the default LLM model.
  """
  def fix_grammar(content, opts \\ []) do
    llm_chain = build_llm_chain(opts)

    initial_messages = [
      Message.new_system!(@system_prompt),
      Message.new_user!(fix_grammar_prompt(content))
    ]

    Chain.execute(llm_chain, initial_messages)
  end

  @doc false
  # Builds the user prompt for a grammar-correcting request. Mirrors the
  # "Fix Grammar & Spelling" mode of the Ai-rewrite extension: minimal
  # corrections, unchanged meaning/tone/style, corrected text only.
  def fix_grammar_prompt(content) do
    """
    Fix only the grammar, spelling, and punctuation errors in this text. Keep the original meaning, tone, and style exactly the same. Make minimal changes - only correct actual errors without changing the author's voice or intent.

    Input text:
    "#{content}"

    Output:
    """
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
