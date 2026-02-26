defmodule Exhub.Llm.Chat do
  alias LangChain.Message
  alias LangChain.Function

  alias Exhub.Llm.Chain
  alias Hermes.Client.Base, as: ClientBase
  alias Exhub.Llm.Mcp.ClientManager
  require Logger

  @all_tools "mcp-server-all"

  def execute(user_message) do
    execute("you are a helpful assistant.", user_message)
  end

  def execute(system_message, user_message) do
    execute(system_message, user_message, nil, %{})
  end

  def execute(system_message, user_message, nil, context) do
    execute_chain(system_message, user_message, [], context)
  end

  def execute(system_message, user_message, @all_tools, context) do
    all_info = ClientManager.get_all_info()
    custom_fns = handle_all_tools(all_info)
    execute_chain(system_message, user_message, custom_fns, context)
  end

  def execute(system_message, user_message, tool_server, context) do
    %{client: client} = ClientManager.get_info(tool_server)
    {:ok, %{"tools" => tools}} = ClientBase.list_tools(client)
    custom_fns = handle_tools(tools, client)
    execute_chain(system_message, user_message, custom_fns, context)
  end

  def execute_with_schema(system_message, user_message, json_schema) do
    llm_chain = Chain.create_llm_chain()
    initial_messages = [
      Message.new_system!(system_message),
      Message.new_user!(user_message)
    ]
    Chain.execute_with_schema(llm_chain, initial_messages, json_schema)
  end

  defp execute_chain(system_message, user_message, custom_fns, context) do
    llm_chain = Chain.create_llm_chain()
    initial_messages = [
      Message.new_system!(system_message),
      Message.new_user!(user_message)
    ]
    Chain.execute(llm_chain, initial_messages, custom_fns, context)
  end

  defp handle_all_tools(all_info) do
    Enum.flat_map(all_info, fn {_client_name, %{client: client}} ->
      {:ok, %{"tools" => tools}} = ClientBase.list_tools(client)
      handle_tools(tools, client)
    end)
  end

  defp handle_tools(tools, client) do
    Enum.map(tools, fn tool ->
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
          Logger.debug("Executing tool #{tool["name"]} with arguments: #{inspect real_args}")
          with {:ok, %{"content" => content}} <- ClientBase.call_tool(client, tool["name"], real_args) do
            {:ok, Jason.encode!(content)}
          end || {:error, "failed to execute tool #{tool["name"]} with arguments: #{inspect real_args}"}
        end
      })
    end)
  end
end
