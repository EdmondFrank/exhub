defmodule Exhub.MCP.Tools.Llm.Chat do
  @moduledoc """
  MCP tool: `chat` (server `llm`)

  Runs one LangChain chat turn against a configured LLM. Backed by
  `Exhub.Llm.Client.chat/2`; usable both as a normal MCP tool and, from the
  `lua` server, as `llm.chat({...})`.
  """

  alias Anubis.Server.Response
  alias Exhub.Llm.Client

  use Anubis.Server.Component, type: :tool

  def name, do: "chat"

  @impl true
  def description do
    """
    Run one chat turn against a configured LLM (LangChain) and return its reply.

    Use `models` first to discover the configured names. `model` accepts a full
    name (`openai/deepseek-v4.1-flash`), a bare model (`kimi-k2.6`), or a
    variant (`openai/qwen3.8-flash--no-think`); omit it to use the default.

    Messages are `{role, content}` objects with `role` = `system`, `user`, or
    `assistant`. `content` is a string or a list of content parts:

        {"type": "text", "text": "..."}
        {"type": "image_url", "url": "https://..."}
        {"type": "image", "data": "<base64>", "media": "image/png"}

    Returns `{content, model, usage}` (`usage` is `{input, output, total}` when
    the provider reports it). Streaming and model-side tool calling are not
    supported — call the real MCP tools directly instead.

    From a Lua snippet:

        local out = llm.chat({model = "kimi-k2.6", prompt = "Say hi"})
        return out.content
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
    case Client.chat(params) do
      {:ok, result} -> {:reply, Response.tool() |> Response.json(result), frame}
      {:error, reason} -> {:reply, Response.tool() |> Response.error(to_string(reason)), frame}
    end
  end
end
