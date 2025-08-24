defmodule Exhub.Llm.World.SupervisorAgent do
  alias Exhub.Llm.Chain

  use Ecto.Schema
  use Instructor
  use SwarmEx.Agent

  require Logger

  @default_llm_name "mistral/mistral-small-latest"

  @llm_doc """
  You are AgentMatcher, an intelligent assistant designed to analyze user queries and match them with the most suitable agent or department. Your task is to understand the user's request, identify key entities and intents, and determine which agent or department would be best equipped to handle the query.

  Important: The user's input may be a follow-up response to a previous interaction. The conversation history, including the name of the previously selected agent, is provided. If the user's input appears to be a continuation of the previous conversation (e.g., "yes", "ok", "I want to know more", "1"), select the same agent as before.

  Analyze the user's input and categorize it into one of the following agent types:
  <agents>
  assistant_agent: A helpful assistant can efficiently handle and complete a variety of normal tasks.
  search_agent: A Web Search Assistant skilled in analyzing query tasks, able to use search tools to query documents online, obtain the latest data, and combine data to complete task requirements.
  </agents>
  If you are unable to select an agent put "unkwnown"

  Guidelines for classification:

  Agent Type: Choose the most appropriate agent type based on the nature of the query. For follow-up responses, use the same agent type as the previous interaction.
  Priority: Assign based on urgency and impact.
  High: Issues affecting service, billing problems, or urgent technical issues
  Medium: Non-urgent product inquiries, sales questions
  Low: General information requests, feedback
  Key Entities: Extract important nouns, product names, or specific issues mentioned. For follow-up responses, include relevant entities from the previous interaction if applicable.
  For follow-ups, relate the intent to the ongoing conversation.
  Confidence: Indicate how confident you are in the classification.
  High: Clear, straightforward requests or clear follow-ups
  Medium: Requests with some ambiguity but likely classification
  Low: Vague or multi-faceted requests that could fit multiple categories
  Is Followup: Indicate whether the input is a follow-up to a previous interaction.

  Handle variations in user input, including different phrasings, synonyms, and potential spelling errors. For short responses like "yes", "ok", "I want to know more", or numerical answers, treat them as follow-ups and maintain the previous agent selection.

  Respond in JSON format with 'userinput', 'selected_agent', and 'confidence' keys.
  """

  @primary_key false
  embedded_schema do
    field(:userinput, :string)
    field(:selected_agent, Ecto.Enum, values: [:assistant_agent, :search_agent])
    field(:confidence, :float)
  end

  @impl true
  def validate_changeset(changeset) do
    changeset
    |> Ecto.Changeset.validate_number(:confidence,
    greater_than_or_equal_to: 0.0,
    less_than_or_equal_to: 1.0
    )
  end

  @impl true
  @spec init(keyword()) :: {:ok, map()} | {:error, term()}
  def init(opts) do
    {:ok, opts}
  end

  @impl true
  def terminate(_reason, _state), do: :ok

  @impl true
  @spec handle_message(String.t(), map()) :: {:ok, any(), map()}
  def handle_message(message, state) when is_binary(message) do
    %{llm: %{endpoint: endpoint, model: model, api_key: api_key}} = Chain.create_llm_chain(@default_llm_name)
    uri = URI.parse(endpoint)
    case Instructor.chat_completion([
          model: model,
          response_model: __MODULE__,
          max_retries: 3,
          messages: [
            %{
              role: "user",
              content: message
            }
          ]
        ],
          [
            adapter: Instructor.Adapters.OpenAI,
            api_key: api_key,
            api_url: "#{uri.scheme}://#{uri.authority}",
            api_path: uri.path
          ]
        ) do
      {:ok, %__MODULE__{userinput: user_message, selected_agent: agent, confidence: _confidence}} ->
        case SwarmEx.send_message_to_pid(state[agent], user_message) do
          {:ok, reply} ->
            combined_reply = "#{reply} | Selected Agent: #{agent}"  # Combine selected_agent with reply
            {:ok, combined_reply, state}  # Ensure reply is binary
          error ->
            Logger.error("Supervisor agent failed: #{inspect(error)}")
            {:ok, inspect(error), state}
        end
      {:error, reason} ->
        {:ok, inspect(reason), state}
    end
  end
end
