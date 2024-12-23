defmodule Exhub.Llm.Chat do
  alias LangChain.Message
  alias LangChain.Chains.LLMChain
  alias LangChain.Utils.ChainResult
  alias LangChain.ChatModels.ChatOpenAI

  @config Application.compile_env(:exhub, :llm, %{model: "Qwen/Qwen2.5-7B-Instruct"})

  def execute(user_message) do
    llm_chain = %{
      llm: ChatOpenAI.new!(%{endpoint: "#{@config[:api_base]}/chat/completions", model: @config[:model]}),
      verbose: false
    }
    initial_messages = [
      Message.new_system!("""
      you are a helpful assistant.
      """),
      Message.new_user!(user_message)
    ]
    {:ok, updated_chain} =
    LLMChain.new!(llm_chain)
    |> LLMChain.add_messages(initial_messages)
    |> LLMChain.run( mode: :while_needs_response)
    updated_chain |> ChainResult.to_string()
  end
end
