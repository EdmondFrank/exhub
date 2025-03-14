defmodule Exhub.Llm.World.Agent do
  alias Exhub.Llm.Chain
  alias LangChain.Message
  alias LangChain.Chains.LLMChain
  alias LangChain.Utils.ChainResult
  use SwarmEx.Agent

  require Logger

  @impl true
  def init(opts) do
    initial_messages =
    if opts[:system_message] do
      [Message.new_system!(opts[:system_message])]
    else
      []
    end

    llm_chain = Chain.create_llm_chain()
    updated_chain =
      LLMChain.new!(llm_chain)
      |> LLMChain.add_messages(initial_messages)
    opts = Map.put(opts, :llm_chain, updated_chain)
    {:ok, opts}
  end

  @impl true
  def terminate(_reason, _state), do: :ok

  @impl true
  def handle_message(message, state) when is_binary(message) do
    {:ok, updated_chain} =
      state[:llm_chain]
      |> LLMChain.add_message(Message.new_user!(message))
      |> LLMChain.run(mode: :while_needs_response)

    {:ok, response} = updated_chain |> ChainResult.to_string()
    {:ok, response, %{state| llm_chain: updated_chain}}
  end
end
