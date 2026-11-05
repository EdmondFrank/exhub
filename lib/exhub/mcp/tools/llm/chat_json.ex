defmodule Exhub.MCP.Tools.Llm.ChatJson do
  @moduledoc """
  MCP tool: `chat_json` (server `llm`)

  Runs one LangChain chat turn constrained by a JSON Schema and returns both the
  raw text and the decoded JSON. Backed by `Exhub.Llm.Client.chat_json/2`;
  usable from Lua as `llm.chat_json({...})`.
  """

  alias Anubis.Server.Response
  alias Exhub.Llm.Client

  use Anubis.Server.Component, type: :tool

  def name, do: "chat_json"

  @impl true
  def description do
    """
    Run one chat turn constrained by a JSON Schema and return the parsed result.

    Same parameters as `chat`, plus `schema` (a JSON Schema object or its JSON
    string) and `strict` (default `true`). OpenAI-compatible and Google models
    use native structured output; other providers get a schema instruction
    appended to the prompt.

    Returns `{content, json, model, usage}`; `json` is `nil` when the reply is
    not valid JSON (the raw `content` is still returned).

    From a Lua snippet:

        local out = llm.chat_json({
          prompt = "Extract the title and tags",
          schema = {type = "object", properties = {title = {type = "string"}}, required = {"title"}}
        })
        return out.json.title
    """
  end

  schema do
    field(:model, :string,
      description:
        "Model name: full (`openai/...`), bare (`kimi-k2.6`), or variant (`...--no-think`). Default: the configured default LLM."
    )

    field(:messages, :any,
      description:
        "Conversation as a list of {role, content} objects. `content` may be a string or a list of content parts."
    )

    field(:prompt, :string, description: "Single user message, used when `messages` is absent.")

    field(:system, :string,
      description: "System prompt, prepended when no system message is present."
    )

    field(:schema, :any,
      description: "JSON Schema the reply must conform to (object or JSON object string)."
    )

    field(:strict, :boolean,
      description:
        "Request strict schema adherence from providers that support it (default: true).",
      default: true
    )

    field(:temperature, :float,
      description: "Sampling temperature (provider default when omitted)."
    )

    field(:max_tokens, :integer,
      description: "Maximum output tokens (default: the configured `default_max_tokens`)."
    )

    field(:timeout_ms, :integer,
      description: "Receive timeout in milliseconds, clamped to the configured maximum."
    )
  end

  @impl true
  def execute(params, frame) do
    case Client.chat_json(params) do
      {:ok, result} -> {:reply, Response.tool() |> Response.json(result), frame}
      {:error, reason} -> {:reply, Response.tool() |> Response.error(to_string(reason)), frame}
    end
  end
end
