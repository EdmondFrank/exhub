defmodule Exhub.Llm.Chain do
  alias LangChain.Chains.LLMChain
  alias LangChain.Utils.ChainResult
  alias LangChain.ChatModels.ChatOpenAI
  alias LangChain.ChatModels.ChatMistralAI

  @config Application.compile_env(:exhub, :llm, %{model: "openai/gpt-3.5-turbo"})

  def create_llm_chain do
    [provider, model_name] = String.split(@config[:model], "/")

    llm_config = %{endpoint: "#{@config[:api_base]}/chat/completions", model: model_name, api_key: @config[:api_key]}

    llm =
      case provider do
        "mistral" -> ChatMistralAI.new!(llm_config)
        _ -> ChatOpenAI.new!(llm_config)
      end

    %{
      llm: llm,
      verbose: false
    }
  end

  def execute(llm_chain, initial_messages) do
    {:ok, updated_chain} =
      LLMChain.new!(llm_chain)
      |> LLMChain.add_messages(initial_messages)
      |> LLMChain.run(mode: :while_needs_response)

    updated_chain |> ChainResult.to_string()
  end
end
