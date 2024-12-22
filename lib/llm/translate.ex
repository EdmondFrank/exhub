defmodule Exhub.Llm.Translate do
  alias LangChain.Message
  alias LangChain.Chains.LLMChain
  alias LangChain.Utils.ChainResult
  alias LangChain.ChatModels.ChatOpenAI

  @config Application.compile_env(:exhub, :llm, %{model: "Qwen/Qwen2.5-7B-Instruct"})

  def execute(content, to_lang) do
    llm_chain = %{
      llm: ChatOpenAI.new!(%{endpoint: "#{@config[:api_base]}/chat/completions", model: @config[:model]}),
      verbose: false
    }
    initial_messages = [
      Message.new_system!("""
      You are a helpful AI translator, Expertise in converting user input into a specific language and returning only the translated content..
      """),
      Message.new_user!("help me translate ```#{content}``` to ```#{to_lang}```")
    ]
    {:ok, updated_chain} =
    LLMChain.new!(llm_chain)
    |> LLMChain.add_messages(initial_messages)
    |> LLMChain.run( mode: :while_needs_response)
    updated_chain |> ChainResult.to_string()
  end
end