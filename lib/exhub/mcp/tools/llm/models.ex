defmodule Exhub.MCP.Tools.Llm.Models do
  @moduledoc """
  MCP tool: `models` (server `llm`)

  Lists the LLMs configured in `Exhub.Llm.LlmConfigServer` — the models an
  `llm.chat` call can use — plus the current default and any allow-list. API
  keys and base URLs are never returned.
  """

  alias Anubis.Server.Response
  alias Exhub.Llm.Client

  use Anubis.Server.Component, type: :tool

  def name, do: "models"

  @impl true
  def description do
    """
    List the LLMs configured in ExHub, with the current default.

    Each entry has `name` (the configured key to pass to `chat`), `provider`
    and the bare `model` name. Also reports whether calls are enabled and, when
    an allow-list is configured, which models `chat` accepts.

    No parameters required.
    """
  end

  schema do
    # No parameters required
  end

  @impl true
  def execute(_params, frame) do
    resp = Response.tool() |> Response.json(Client.models())
    {:reply, resp, frame}
  end
end
