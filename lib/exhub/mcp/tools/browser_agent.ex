defmodule Exhub.MCP.Tools.BrowserAgent do
  @moduledoc """
  MCP Tool that runs the Jev-style browser loop with ExHub's existing browser
  automation (`kuri-agent` / CDP) and the Smart Decide decision model.

  Give it one natural-language goal. The agent observes the attached tab,
  builds an indexed element table, asks Smart Decide for an operation plus a
  target in one request, executes the chosen operation, and repeats until
  `DONE`, `BLOCKED`, or the step budget. `TYPE_TEXT` values come from
  `Exhub.BrowserAgent.TextHelper`.

  The tab must already be attached with `browser_tabs` (`use`) or opened with
  `kuri-agent open`.

  Commands:

    * `run`    — run the loop synchronously and return the full trace
    * `start`  — create a session for interactive `step` control
    * `step`   — advance a session one cycle
    * `status` — read a session's current observation and history
    * `stop`   — discard a session
    * `list`   — list active sessions
  """

  alias Anubis.Server.Response
  alias Exhub.BrowserAgent.{Agent, Store}

  use Anubis.Server.Component, type: :tool

  @valid_commands ~w(run start step status stop list)

  def name, do: "browser_agent"

  @impl true
  def description do
    """
    Run a Jev-style browser agent that *chooses* an operation and target via the
    Smart Decide (System One) model instead of generating selectors or code.

    Give it one natural-language `goal`; it observes the attached Chrome tab,
    numbers the interactive elements, has Smart Decide pick an operation
    (CLICK / TYPE_TEXT / SCROLL_UP / SCROLL_DOWN / WAIT / DONE / BLOCKED) plus a
    target, and executes only that. `TYPE_TEXT` values are written by a small
    text model. Stops on DONE, BLOCKED, or `max_steps`.

    Requires an attached tab (see `browser_tabs`).

    **Commands:**
    - `run`    — run to completion and return the trace (needs `goal`; optional `url`, `max_steps`, `model`)
    - `start`  — create a session and return its `session_id` (needs `goal`)
    - `step`   — advance one cycle (needs `session_id`)
    - `status` — read a session (needs `session_id`)
    - `stop`   — discard a session (needs `session_id`)
    - `list`   — list active sessions
    """
  end

  schema do
    field(:command, {:required, :string},
      description: "One of: #{Enum.join(@valid_commands, ", ")}"
    )

    field(:goal, :string,
      description: "The natural-language goal to accomplish (required for `run` and `start`)"
    )

    field(:url, :string,
      description:
        "Optional URL to navigate to before the first observation. Example: https://example.com"
    )

    field(:session_id, :string,
      description: "Session id from `start` (required for `step`, `status`, `stop`)"
    )

    field(:max_steps, :integer, description: "Maximum decision cycles for `run` (default 15)")

    field(:model, :string, description: "System One model override (default APUS-OpenJev-v1-9B)")
  end

  @impl true
  def execute(params, frame) do
    case Map.get(params, :command) do
      "run" ->
        run(params, frame)

      "start" ->
        start(params, frame)

      "step" ->
        step(params, frame)

      "status" ->
        status(params, frame)

      "stop" ->
        stop(params, frame)

      "list" ->
        list(frame)

      command ->
        error(
          frame,
          "Unknown command: #{inspect(command)}. Valid: #{Enum.join(@valid_commands, ", ")}"
        )
    end
  end

  defp run(params, frame) do
    with {:ok, goal} <- require_goal(params) do
      agent = goal |> Agent.new(build_opts(params)) |> Agent.run()
      id = Store.new_id()
      Store.put(id, agent)
      respond(frame, %{"session_id" => id, "result" => Agent.snapshot(agent)})
    else
      {:error, message} -> error(frame, message)
    end
  end

  defp start(params, frame) do
    with {:ok, goal} <- require_goal(params) do
      id = Store.new_id()
      agent = Agent.new(goal, build_opts(params))
      Store.put(id, agent)
      respond(frame, %{"session_id" => id, "result" => Agent.snapshot(agent)})
    else
      {:error, message} -> error(frame, message)
    end
  end

  defp step(params, frame) do
    with {:ok, id} <- require_session_id(params),
         {:ok, session} <- fetch(id) do
      agent = session.agent |> Agent.step()
      Store.put(id, agent)
      respond(frame, %{"session_id" => id, "result" => Agent.snapshot(agent)})
    else
      {:error, message} -> error(frame, message)
    end
  end

  defp status(params, frame) do
    with {:ok, id} <- require_session_id(params),
         {:ok, session} <- fetch(id) do
      respond(frame, %{"session_id" => id, "result" => Agent.snapshot(session.agent)})
    else
      {:error, message} -> error(frame, message)
    end
  end

  defp stop(params, frame) do
    with {:ok, id} <- require_session_id(params) do
      Store.delete(id)
      respond(frame, %{"session_id" => id, "result" => %{"status" => "stopped"}})
    else
      {:error, message} -> error(frame, message)
    end
  end

  defp list(frame) do
    sessions =
      Store.list()
      |> Enum.map(fn session ->
        %{
          "session_id" => session.id,
          "status" => to_string(session.agent.status),
          "goal" => session.agent.goal,
          "step" => length(session.agent.history)
        }
      end)

    respond(frame, %{"sessions" => sessions, "count" => length(sessions)})
  end

  # --- helpers ---

  defp require_goal(params) do
    case Map.get(params, :goal) do
      goal when is_binary(goal) and goal != "" -> {:ok, String.trim(goal)}
      _ -> {:error, "`goal` is required for this command"}
    end
  end

  defp require_session_id(params) do
    case Map.get(params, :session_id) do
      id when is_binary(id) and id != "" -> {:ok, id}
      _ -> {:error, "`session_id` is required for this command"}
    end
  end

  defp fetch(id) do
    case Store.get(id) do
      {:ok, session} -> {:ok, session}
      {:error, :not_found} -> {:error, "session #{inspect(id)} not found"}
    end
  end

  defp build_opts(params) do
    []
    |> put_opt(:model, Map.get(params, :model))
    |> put_opt(:max_steps, Map.get(params, :max_steps))
    |> put_opt(:start_url, Map.get(params, :url))
  end

  defp put_opt(opts, _key, nil), do: opts
  defp put_opt(opts, _key, ""), do: opts
  defp put_opt(opts, key, value), do: Keyword.put(opts, key, value)

  defp respond(frame, payload) do
    resp = Response.tool() |> Response.json(payload)
    {:reply, resp, frame}
  end

  defp error(frame, message) do
    resp = Response.tool() |> Response.error(message)
    {:reply, resp, frame}
  end
end
