defmodule Exhub.Llm.Chain do
  alias LangChain.Chains.LLMChain
  alias LangChain.Utils.ChainResult
  alias LangChain.ChatModels.ChatOpenAI
  alias LangChain.ChatModels.ChatMistralAI
  alias LangChain.ChatModels.ChatGoogleAI
  alias LangChain.ChatModels.ChatAnthropic
  alias LangChain.LangChainError
  alias Exhub.Llm.LlmConfigServer
  require Logger

  def create_llm_chain do
    {:ok, config} = LlmConfigServer.get_default_llm_config()
    create_llm_chain_from_config(config)
  end

  def create_llm_chain(llm_name, opts \\ %{}) do
    {:ok, config} = LlmConfigServer.get_llm_config(llm_name)
    create_llm_chain_from_config(config, opts)
  end

  def execute(llm_chain, initial_messages) do
    case LLMChain.new!(llm_chain)
         |> LLMChain.add_messages(initial_messages)
         |> LLMChain.run(mode: :while_needs_response) do
      {:ok, updated_chain} ->
        updated_chain |> ChainResult.to_string()
      {:error, _, %LangChainError{message: message} = error} ->
        Logger.error("LLMChain.run failed: #{inspect(error)}")
        message
    end
  end

  def execute(llm_chain, initial_messages, functions, custom_context) do
    case LLMChain.new!(Map.put(llm_chain, :custom_context, custom_context))
         |> LLMChain.add_tools(functions)
         |> LLMChain.add_messages(initial_messages)
         |> LLMChain.run(mode: :while_needs_response) do
      {:ok, updated_chain} ->
        updated_chain |> ChainResult.to_string()
      {:error, _, %LangChainError{message: message} = error} ->
        Logger.error("LLMChain.run failed: #{inspect(error)}")
        message
    end
  end

  def run(llm_chain, initial_messages, functions, custom_context, callbacks \\ %{}) do
    chain = LLMChain.new!(Map.put(llm_chain, :custom_context, custom_context))
    # chain = if functions != [], do: LLMChain.add_tools(chain, functions), else: chain
    chain = LLMChain.add_messages(chain, initial_messages)
    chain = if map_size(callbacks) > 0, do: LLMChain.add_callback(chain, callbacks), else: chain
    LLMChain.run(chain)
  end

  defp create_llm_chain_from_config(config, opts \\ %{}) do
    [provider, model_name] = String.split(config[:model], "/", parts: 2)

    # Base configuration
    base_config =
      case provider do
        "google" -> %{endpoint: config[:api_base], model: model_name, api_key: config[:api_key]}
        "anthropic" -> %{endpoint: "#{config[:api_base]}/messages", model: model_name, api_key: config[:api_key]}
        _ -> %{endpoint: "#{config[:api_base]}/chat/completions", model: model_name, api_key: config[:api_key]}
      end

    # Merge user-provided options (max_tokens, temperature, stream, etc.)
    llm_config = Map.merge(base_config, opts)

    # Create LLM struct with merged configuration
    llm =
      case provider do
        "google" -> ChatGoogleAI.new!(llm_config)
        "mistral" -> ChatMistralAI.new!(llm_config)
        "anthropic" -> ChatAnthropic.new!(llm_config)
        _ -> ChatOpenAI.new!(llm_config)
      end

    %{
      llm: llm,
      verbose: false
    }
  end
end
