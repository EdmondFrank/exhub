defmodule Exhub.Llm.Chat do
  alias LangChain.Message
  alias LangChain.Function

  alias Hermes.Client
  alias Exhub.Llm.Chain
  alias Exhub.Llm.Mcp.ClientManager
  require Logger


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

  def execute(system_message, user_message, tool_server, context) do
    %{client: client} = ClientManager.get_info(tool_server)
    {:ok, %{"tools" => tools}} = Client.list_tools(client)

    custom_fns = Enum.map(tools, fn tool ->
      Function.new!(%{
            name: tool["name"],
            description: tool["description"],
            parameters_schema: %{
              type: "object",
              properties: tool["inputSchema"]["properties"],
              required: tool["required"]
            },
            function: fn arguments, context ->
              real_args = Map.merge(arguments, context)
              Logger.debug("Excecuting tool #{tool["name"]} with arguments: #{inspect real_args}")
              with {:ok, %{"content" => content} } <- Client.call_tool(client, tool["name"], real_args) do
                {:ok, Jason.encode!(content)}
              end || {:error, "failed to excecute tool #{tool["name"]} with arguments: #{inspect real_args}"}
            end
          })
    end)

    llm_chain = Chain.create_llm_chain()
    initial_messages = [
      Message.new_system!(system_message),
      Message.new_user!(user_message)
    ]
    Chain.execute(llm_chain, initial_messages, custom_fns, context)
  end
end
