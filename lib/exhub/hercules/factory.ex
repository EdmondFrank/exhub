defmodule Exhub.Hercules.Factory do
  @moduledoc """
  Builds the Hercules Planner agent with nav-agent subagents.

  The Planner delegates steps to specialized SubAgents (browser, api, time_keeper)
  via the SubAgent middleware's `task` tool.
  """

  alias Sagents.Agent
  alias Sagents.SubAgent
  alias Exhub.Sagents.McpAdapter
  alias Exhub.Sagents.Factory, as: SagentsFactory
  alias Exhub.Llm.LlmConfigServer
  alias Exhub.Hercules.Prompts

  require Logger

  @doc """
  Create the Hercules Planner agent struct.

  ## Options

    * `:planner_model` — LLM config name for the planner (default: uses default LLM)
    * `:nav_model` — LLM config name for nav agents (default: same as planner)
    * `:max_planner_rounds` — Max LLM calls for planner (default: 500)
    * `:max_nav_rounds` — Max LLM calls per nav agent (default: 50)
    * `:test_data` — Test data string to inject into planner prompt

  """
  def create_planner_agent(opts \\ []) do
    with {:ok, planner_model} <- build_model(opts[:planner_model]),
         {:ok, nav_model} <- build_model(opts[:nav_model] || opts[:planner_model]) do
      subagents = [
        browser_subagent(opts),
        api_subagent(opts),
        time_keeper_subagent()
      ]

      test_data = Keyword.get(opts, :test_data, "")

      agent =
        Agent.new!(
          %{
            agent_id: "hercules_planner_#{System.unique_integer([:positive])}",
            model: planner_model,
            base_system_prompt: Prompts.planner(),
            max_runs: opts[:max_planner_rounds] || 500,
            middleware: [
              {Sagents.Middleware.SubAgent,
               [
                 model: nav_model,
                 subagents: subagents,
                 block_middleware: [Sagents.Middleware.ConversationTitle]
               ]},
              {Sagents.Middleware.Summarization,
               [
                 model: planner_model,
                 max_tokens_before_summary: 100_000,
                 messages_to_keep: 10
               ]},
              {Sagents.Middleware.PatchToolCalls, []},
              {Exhub.Hercules.Middleware.TestTracker, [test_data: test_data]}
            ]
          },
          replace_default_middleware: true
        )

      {:ok, agent}
    end
  end

  # ─── SubAgent Configs ────────────────────────────────────────────────────

  defp browser_subagent(opts) do
    browser_tools = McpAdapter.build_tools([:browser_use])

    SubAgent.Config.new!(%{
      name: "browser",
      description:
        "Web browser navigation: open URLs, click elements, fill forms, " <>
          "read page content, take screenshots. Use for all web UI interactions.",
      system_prompt: Prompts.browser_nav(),
      tools: browser_tools,
      max_runs: opts[:max_nav_rounds] || 50
    })
  end

  defp api_subagent(opts) do
    SubAgent.Config.new!(%{
      name: "api",
      description:
        "HTTP API testing: send GET/POST/PUT/DELETE requests, " <>
          "validate response status codes and body content.",
      system_prompt: Prompts.api_nav(),
      tools: [http_request_tool()],
      max_runs: opts[:max_nav_rounds] || 20
    })
  end

  defp time_keeper_subagent do
    SubAgent.Config.new!(%{
      name: "time_keeper",
      description: "Time-related operations: wait/sleep for specified duration.",
      system_prompt: Prompts.time_keeper(),
      tools: [wait_tool()],
      max_runs: 5
    })
  end

  # ─── Tool Definitions ────────────────────────────────────────────────────

  defp http_request_tool do
    LangChain.Function.new!(%{
      name: "http_request",
      description:
        "Send an HTTP request. Returns status code, headers, and body. " <>
          "Use for API testing and validation.",
      parameters: [
        %{name: "method", type: :string, description: "HTTP method: GET, POST, PUT, DELETE, PATCH", required: true},
        %{name: "url", type: :string, description: "Full URL to send the request to", required: true},
        %{name: "headers", type: :object, description: "Request headers as key-value pairs", required: false},
        %{name: "body", type: :string, description: "Request body (JSON string or raw text)", required: false}
      ],
      function: fn args, _context ->
        method = String.to_atom(String.downcase(args["method"] || "get"))
        url = args["url"]
        headers = args["headers"] || %{}
        body = args["body"]

        req_opts = [
          method: method,
          url: url,
          headers: Enum.map(headers, fn {k, v} -> {k, v} end)
        ]

        req_opts =
          if body && body != "" do
            Keyword.put(req_opts, :body, body)
          else
            req_opts
          end

        case Req.request(req_opts) do
          {:ok, %Req.Response{status: status, headers: resp_headers, body: resp_body}} ->
            Jason.encode!(%{
              status: status,
              headers: Map.new(resp_headers),
              body: truncate_body(resp_body)
            })

          {:error, reason} ->
            Jason.encode!(%{error: inspect(reason)})
        end
      end
    })
  end

  defp wait_tool do
    LangChain.Function.new!(%{
      name: "wait",
      description: "Wait/sleep for a specified number of seconds.",
      parameters: [
        %{name: "seconds", type: :number, description: "Number of seconds to wait", required: true}
      ],
      function: fn args, _context ->
        seconds = args["seconds"] || 1
        ms = round(seconds * 1000)
        Process.sleep(ms)
        Jason.encode!(%{waited_seconds: seconds, status: "completed"})
      end
    })
  end

  # ─── Model Builder ───────────────────────────────────────────────────────

  defp build_model(nil) do
    case LlmConfigServer.get_default_llm_config() do
      {:ok, config} -> {:ok, SagentsFactory.create_langchain_model(config)}
      {:error, reason} -> {:error, reason}
    end
  end

  defp build_model(model_name) when is_binary(model_name) do
    case LlmConfigServer.get_llm_config(model_name) do
      {:ok, config} ->
        {:ok, SagentsFactory.create_langchain_model(config)}

      {:error, _reason} ->
        Logger.warning("[Hercules.Factory] LLM config '#{model_name}' not found, using default")
        build_model(nil)
    end
  end

  # ─── Helpers ─────────────────────────────────────────────────────────────

  defp truncate_body(body) when is_binary(body) do
    if byte_size(body) > 10_000 do
      String.slice(body, 0, 10_000) <> "... (truncated)"
    else
      body
    end
  end

  defp truncate_body(body), do: inspect(body)
end
