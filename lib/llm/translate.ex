defmodule Exhub.Llm.Translate do
  alias LangChain.Message

  alias Exhub.Llm.Chain

  def execute(content, to_lang) do
    llm_chain = Chain.create_llm_chain()
    to_lang = if to_lang |> String.trim() |> String.length() == 0, do: "EN", else: to_lang
    initial_messages = [
      Message.new_system!("""
      You are a helpful AI translator, expert in converting user input between ``` and ``` into a specific language and returning only the translated content. Be careful not to make any grammatical or spelling errors, and please help refine the translation."
      """),
      Message.new_user!("help me translate ```#{content}``` to `#{to_lang}`, only return the translated content.")
    ]
    Chain.execute(llm_chain, initial_messages)
  end
end
