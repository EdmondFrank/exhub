defmodule Exhub.Genclaw.FactoryEntry do
  @moduledoc """
  Registration helper for the GenClaw agent in Exhub.Sagents.Factory.

  Provides the agent configuration (system prompt, tools, middleware)
  for the "genclaw" agent entry.
  """

  alias Exhub.Genclaw.SystemPrompt
  alias Exhub.Genclaw.Tools.{T2I, I2I, Search, Reason, FormatPrompt, VLMReview}

  @doc """
  Build the list of LangChain.Function tools for the GenClaw agent.
  """
  def build_tools do
    [
      T2I.build(),
      I2I.build(),
      Search.build(),
      Reason.build(),
      FormatPrompt.build(),
      VLMReview.build()
    ]
  end

  @doc """
  Build the GenClaw agent configuration map for Factory registration.
  """
  def agent_config(opts \\ []) do
    model = Keyword.get(opts, :model)

    %{
      system_prompt: SystemPrompt.render(model: model || "kimi-k2.6"),
      mcp_tools: [],
      middleware: [
        {Exhub.Genclaw.Middleware.Perception, []},
        {Exhub.Genclaw.Middleware.CompletionGuard, []}
      ],
      tools: build_tools()
    }
  end
end
