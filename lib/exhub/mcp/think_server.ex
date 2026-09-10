defmodule Exhub.MCP.ThinkServer do
  @moduledoc """
  MCP Server for think and plan functionality.

  This server exposes two tools via the Model Context Protocol:
  1. think - Record thoughts for complex reasoning
  2. plan - Plan steps for complex reasoning

  Both tools are backed by a per-session scratchpad
  (`Exhub.MCP.Tools.Scratchpad`) stored in the session frame's assigns: every
  call appends its entry and returns the accumulated notes as a JSON envelope
  (`recorded` / `scratchpad` / `next`). Entries are truncated individually and
  bounded in count, so long sessions get consolidated working memory without
  unbounded context growth. No extra supervision children are required — the
  Anubis session process persists the frame between requests.

  The server uses HTTP transport and can be accessed at the /think/mcp endpoint.
  """

  use Anubis.Server,
    name: "exhub-think-server",
    version: "1.0.0",
    capabilities: [:tools]

  # Register the tool components
  component(Exhub.MCP.Tools.Think)
  component(Exhub.MCP.Tools.Plan)

  @impl true
  def init(client_info, frame) do
    _ = client_info

    {:ok, frame}
  end

  @impl true
  def handle_request(request, frame) do
    Exhub.MCP.ServerHelpers.handle_request_with_filtered_tools(__MODULE__, request, frame)
  end
end
