defmodule Exhub.Llm.World.SearchAgent do
  alias Exhub.Llm.Chain
  alias LangChain.Message
  alias LangChain.Function
  alias LangChain.Chains.LLMChain
  alias LangChain.Utils.ChainResult
  alias LangChain.LangChainError
  use SwarmEx.Agent

  require Logger

  @default_llm_name "openai/Qwen2.5-72B-Instruct"
  @proxy Application.compile_env(:exhub, :proxy, "")

  @impl true
  def init(opts) do
    initial_messages = [
      Message.new_system!("""
      **Role:** Web Search Assistant
      **Skills:**
      1. **Intent Analysis:** Understand and analyze user queries to determine the user's intent and generate relevant keywords.
      2. **Web Search:** Utilize a `web_search` tool to execute searches using the generated keywords and retrieve a list of relevant results.
      3. **Web Fetch:** Use a `web_fetch` tool to retrieve the details of the most relevant content.
      4. **Content Matching:** Compare the summaries of the search results with the user's intent to identify the most relevant content.
      5. **Answer Generation:** Combine the fetched content with the user's question to provide a coherent and accurate answer.

      **Constraints:**
      - Focus solely on the tasks outlined: intent analysis, web search, content matching, web fetch, and answer generation.
      - Do not engage in conversations, provide opinions, or answer questions beyond the scope of the user's query.
      - Prioritize accuracy and relevance in search results and answer generation.
      """)
    ]

    llm_chain = Chain.create_llm_chain(@default_llm_name)
    updated_chain =
      LLMChain.new!(llm_chain)
      |> LLMChain.add_messages(initial_messages)
      |> LLMChain.add_tools([
      Function.new!(
        %{
          name: "web_search",
          description: "Search the web using keywords, and return the titles, summaries, and links of the retrieved relevant content.",
          parameters_schema: %{
            type: "object",
            properties: %{
              keywords: %{
                type: "string",
                description: "keywords for search, multiple words are split by spaces"
              }
            },
            required: ["keywords"]
          },
          function: fn args, _context ->
            Logger.debug("Executing search with arguments: #{inspect args}")
            with {result, 0} <- System.cmd("python", ["-m", "webscout", "text", "-k", args["keywords"]], env: [{"HTTPS_PROXY", @proxy}]) do
              Logger.debug("After search with result: #{result}")
              {:ok, result}
            end || {:ok, "failed to execute search tool with arguments: #{inspect args}"}
          end
        }
      ),
      Function.new!(
        %{
          name: "web_fetch",
          description: "Fetch the web page detail content by href.",
          parameters_schema: %{
            type: "object",
            properties: %{
              href: %{
                type: "string",
                description: "Web URL or href to fetch"
              }
            },
            required: ["href"]
          },
          function: fn args, _context ->
            Logger.debug("Executing fetch with arguments: #{inspect args}")
            case HTTPoison.get(args["href"], [], proxy: @proxy) do
              {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
                {:ok, html} = Floki.parse_document(body)
                text = Floki.find(html, "body") |> Floki.text

                {:ok, text}
              {:ok, %HTTPoison.Response{status_code: status_code}} ->
                {:ok, "HTTP request failed with status code: #{status_code}"}
              {:error, %HTTPoison.Error{reason: reason}} ->
                {:ok, "HTTP request failed: #{inspect reason}"}
            end
          end
        }
      ),
    ])
      opts = Map.put(opts, :llm_chain, updated_chain)
      {:ok, opts}
  end

  @impl true
  def terminate(_reason, _state), do: :ok

  @impl true
  def handle_message(message, state) when is_binary(message) do
    case state[:llm_chain]
         |> LLMChain.add_message(Message.new_user!(message))
         |> LLMChain.run(mode: :while_needs_response) do
      {:ok, updated_chain} ->
        {:ok, response} = updated_chain |> ChainResult.to_string()
        {:ok, response, %{state | llm_chain: updated_chain}}
      {:error, _, %LangChainError{message: message} = error} ->
        Logger.error("LLMChain.run failed: #{inspect(error)}")
        {:ok, message, state}
    end
  end
end
