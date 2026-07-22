defmodule Exhub.Sagents.Factory do
  @moduledoc """
  Factory for creating sagents agents using exhub's LLM configuration.

  Implements the `Sagents.Factory` behaviour. Agents are defined as maps
  with `system_prompt`, `mcp_tools`, and optional `middleware`.

  Uses `Exhub.Llm.LlmConfigServer` for the default LLM model.
  """

  alias Exhub.Llm.LlmConfigServer
  alias Exhub.Sagents.McpAdapter
  require Logger

  def create_agent(agent_id, config) do
    Logger.info("[Sagents.Factory] create_agent: '#{agent_id}' config keys: #{inspect(Map.keys(config))}")

    with {:ok, model} <- build_langchain_model(config) do
      mcp_tools = McpAdapter.build_tools(config[:mcp_tools] || [])
      native_tools = config[:tools] || []
      middleware = config[:middleware] || default_middleware()

      all_tools = mcp_tools ++ native_tools
      Logger.info("[Sagents.Factory] create_agent: '#{agent_id}' mcp_tools=#{length(mcp_tools)} native_tools=#{length(native_tools)} middleware=#{length(middleware)}")
      Logger.info("[Sagents.Factory] create_agent: '#{agent_id}' tool names: #{inspect(Enum.map(all_tools, & &1.name))}")

      agent =
        Sagents.Agent.new!(%{
          agent_id: agent_id,
          model: model,
          base_system_prompt: config[:system_prompt] || "You are a helpful assistant.",
          middleware: middleware,
          tools: all_tools
        })

      session_opts = config[:session_opts] || []
      {:ok, agent, session_opts}
    else
      {:error, reason} ->
        Logger.error("[Sagents.Factory] Failed to create agent #{agent_id}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @doc """
  Returns the map of all registered agent definitions.
  """
  def agents do
    %{
      "coder" => %{
        system_prompt: """
        You are an expert coding assistant. You can read, write, and edit files,
        execute commands, and search code. Always explain what you're doing before
        taking action. Use the available tools to help the user with their coding tasks.
        """,
        mcp_tools: [:desktop, :"web-tools"],
        middleware:
          default_middleware() ++
            [
              {Sagents.Middleware.HumanInTheLoop,
               [interrupt_on: %{"execute_command" => true, "write_file" => true}]}
            ]
      },
      "researcher" => %{
        system_prompt: """
        You are a research assistant. You can search the web, read documents,
        and access the knowledge base. Provide thorough, well-sourced answers.
        """,
        mcp_tools: [:"web-tools", :brain, :look]
      },
      "assistant" => %{
        system_prompt: """
        You are a general-purpose assistant. You can help with a wide range of
        tasks including file management, web search, and information retrieval.
        """,
        mcp_tools: [:desktop, :"web-tools", :todo]
      },
      "genclaw" => Exhub.Genclaw.FactoryEntry.agent_config(model: "kimi-k2.6")
    }
  end

  defp build_langchain_model(config) do
    case Map.get(config, :model) do
      nil ->
        case LlmConfigServer.get_default_llm_config() do
          {:ok, llm_config} -> {:ok, create_langchain_model(llm_config)}
          {:error, reason} -> {:error, reason}
        end

      model_name when is_binary(model_name) ->
        case LlmConfigServer.get_llm_config(model_name) do
          {:ok, llm_config} -> {:ok, create_langchain_model(llm_config)}
          {:error, reason} -> {:error, reason}
        end
    end
  end

  @doc "Create a LangChain chat model struct from an LLM config map."
  def create_langchain_model(config) do
    [provider, model_name] = String.split(config[:model], "/", parts: 2)

    base_config = %{
      model: model_name,
      api_key: config[:api_key]
    }

    llm_config =
      case provider do
        "google" ->
          Map.put(base_config, :endpoint, config[:api_base])

        "anthropic" ->
          Map.put(base_config, :endpoint, "#{config[:api_base]}/messages")

        _ ->
          Map.put(base_config, :endpoint, "#{config[:api_base]}/chat/completions")
      end

    case provider do
      "google" -> LangChain.ChatModels.ChatGoogleAI.new!(llm_config)
      "anthropic" -> LangChain.ChatModels.ChatAnthropic.new!(llm_config)
      _ -> LangChain.ChatModels.ChatOpenAI.new!(llm_config)
    end
  end

  defp default_middleware do
    [
      {Sagents.Middleware.TodoList, []},
      {Sagents.Middleware.FileSystem, []},
      {Sagents.Middleware.Summarization, []},
      {Sagents.Middleware.PatchToolCalls, []}
    ]
  end
end
