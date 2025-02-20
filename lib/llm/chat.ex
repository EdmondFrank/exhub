defmodule Exhub.Llm.Chat do
  alias LangChain.Message

  alias Exhub.Llm.Chain

  def execute(user_message) do
    llm_chain = Chain.create_llm_chain()
    initial_messages = [
      Message.new_system!("""
      you are a helpful assistant.
      """),
      Message.new_user!(user_message)
    ]
    Chain.execute(llm_chain, initial_messages)
  end

  def execute(system_message, user_message) do
    llm_chain = Chain.create_llm_chain()
    initial_messages = [
      Message.new_system!(system_message),
      Message.new_user!(user_message)
    ]
    Chain.execute(llm_chain, initial_messages)
  end
end
