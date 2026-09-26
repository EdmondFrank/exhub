defmodule Exhub.BrowserAgent.Agent do
  @moduledoc """
  The Jev loop: observe → choose → execute.

  Give it one natural-language goal. Each step takes an accessibility snapshot
  (the indexed element table), asks Smart Decide for an operation plus a
  target, and executes only the chosen operation — `TYPE_TEXT` additionally
  asks `Exhub.BrowserAgent.TextHelper` for the field value. The loop stops on
  `DONE`, `BLOCKED`, an error, or the step budget.

  Every collaborator is injectable through `opts` (`:kuri`, `:decider`,
  `:generator`), so the whole loop can be exercised without a browser or
  network access in tests.
  """

  alias Exhub.BrowserAgent.{Executor, Policy, Snapshot, TextHelper}

  @default_max_steps 15

  # Smart Decide accepts at most 8191 input tokens, and documentation sites
  # render thousands of actionable nodes, so the element table handed to the
  # model is bounded (document order; the offered target heads come from the
  # first `max_targets` of each role, which lie well inside this window).
  @default_max_elements 120
  @page_text_limit 4000

  # A target action that changes nothing and cannot reveal new candidates is a
  # dead end; a few in a row mean the offered head is exhausted and no control
  # operation will help.
  @max_stalls 3

  # Documentation sites put thousands of characters of navigation and sidebar
  # ahead of the article, so slicing the whole-body text from the top hides the
  # very content the goal is about (DevDocs' Rails `before_action` sits ~5,600
  # characters in). Read the main content pane first, fall back to body text.
  @main_text_script """
  (function () {
    var el = document.querySelector("main, [role=main], article");
    var text = el ? el.innerText : document.body.innerText;
    return JSON.stringify(text || "");
  })()
  """

  @type t :: %__MODULE__{
          id: String.t() | nil,
          goal: String.t(),
          page: map(),
          elements: [map()],
          targets: map(),
          decision: map() | nil,
          history: [map()],
          status: atom(),
          error: String.t() | nil,
          fingerprint: String.t() | nil,
          observation: String.t() | nil,
          window: non_neg_integer(),
          stall: non_neg_integer(),
          started_at: integer() | nil,
          opts: keyword()
        }

  defstruct id: nil,
            goal: "",
            page: %{},
            elements: [],
            targets: %{},
            decision: nil,
            history: [],
            status: :ready,
            error: nil,
            fingerprint: nil,
            observation: nil,
            window: 0,
            stall: 0,
            started_at: nil,
            opts: []

  @doc "Builds a new agent for `goal`."
  @spec new(String.t(), keyword()) :: t()
  def new(goal, opts \\ []) when is_binary(goal) do
    %__MODULE__{goal: String.trim(goal), opts: opts}
  end

  @doc """
  Runs the loop to completion (or the step budget).

  Returns the final agent with the full `history`. The step budget comes from
  `opts[:max_steps]` (default #{@default_max_steps}).
  """
  @spec run(t()) :: t()
  def run(%__MODULE__{} = agent) do
    max_steps = Keyword.get(agent.opts, :max_steps, @default_max_steps)
    run_loop(agent, max_steps)
  end

  defp run_loop(agent, remaining) do
    cond do
      agent.status in [:done, :blocked, :error] -> agent
      remaining <= 0 -> %{agent | status: :blocked, error: "reached the step budget"}
      true -> agent |> step() |> run_loop(remaining - 1)
    end
  end

  @doc """
  Advances the loop one cycle: observe, choose, execute.

  Returns the updated agent. Terminal states (`DONE`, `BLOCKED`) are sticky. A
  step that observes `#{@max_stalls}` consecutive no-progress target actions in
  a row blocks instead of choosing again.
  """
  @spec step(t()) :: t()
  def step(%__MODULE__{status: status} = agent) when status in [:done, :blocked, :error],
    do: agent

  def step(%__MODULE__{} = agent) do
    started = agent.started_at || System.monotonic_time(:millisecond)
    agent = %{agent | started_at: started}

    # Observe first and thread the observed agent into the next stage, so a
    # decision failure still returns the observation instead of discarding it.
    with {:ok, agent} <- ensure_observed(agent) do
      if agent.stall >= @max_stalls do
        %{agent | status: :blocked, error: "no progress after #{@max_stalls} repeated actions"}
      else
        predict_and_execute(agent)
      end
    else
      {:error, message} -> %{agent | status: :error, error: message}
    end
  end

  defp predict_and_execute(agent) do
    with {:ok, agent} <- predict(agent),
         {:ok, agent} <- execute(agent) do
      agent
    else
      {:error, message} -> %{agent | status: :error, error: message}
    end
  end

  @doc """
  Takes a fresh snapshot and rebuilds the element table.

  The accessibility snapshot is preferred; when the backend cannot fetch it
  (Chrome's CDP accessibility tree fails on wide DOMs — measured between ~4,000
  and ~6,000 nodes, which covers documentation sites like DevDocs), the backend's
  stamped-DOM table is used instead and recorded as the observation source.

  The resulting table is bounded to `opts[:max_elements]` (default 120) so the
  decision prompt fits the model's input limit. Each observation also settles
  the previous step: whether it changed the page (stamped on its history entry,
  so the model has real evidence for "do not repeat satisfied steps") and, when
  a target action changed nothing, whether to slide the offered candidate
  window past a head that made no progress.
  """
  @spec observe(t()) :: {:ok, t()} | {:error, String.t()}
  def observe(%__MODULE__{} = agent) do
    kuri = kuri(agent)

    with {:ok, payload, observation} <- observe_payload(kuri),
         {:ok, page} <- fetch_page(kuri, payload) do
      elements =
        payload
        |> Snapshot.parse()
        |> Snapshot.index()
        |> Enum.take(max_elements(agent))

      new_fingerprint = fingerprint(elements, page)
      {history, window, stall} = advance_progress(agent, elements, new_fingerprint)

      {indexed, targets} =
        Snapshot.action_space(elements,
          max_targets: max_targets(agent),
          offset: window,
          prefer: Snapshot.goal_terms(agent.goal)
        )

      {:ok,
       %{
         agent
         | page: page,
           elements: indexed,
           targets: targets,
           history: history,
           window: window,
           stall: stall,
           observation: to_string(observation),
           fingerprint: new_fingerprint
       }}
    end
  end

  # Settles the previous step against the fresh observation.
  #
  # * a page change means the candidates are new, so the window resets;
  # * a target action (CLICK/TYPE_TEXT) that changed nothing but left more
  #   candidates unoffered slides the window on — that is how an element the
  #   model was never shown (a deep sidebar link on a docs page) becomes
  #   choosable, and it counts as progress, not a stall;
  # * a target action that changed nothing with no candidates left is a stall.
  defp advance_progress(%{fingerprint: nil} = agent, _elements, _fingerprint),
    do: {agent.history, 0, 0}

  defp advance_progress(agent, elements, fingerprint) do
    changed = fingerprint != agent.fingerprint
    history = stamp_page_changed(agent.history, changed)
    target_action? = last_target_action?(agent.history)

    cond do
      changed ->
        {history, 0, 0}

      target_action? and window_has_more?(agent, elements) ->
        {history, agent.window + max_targets(agent), 0}

      target_action? ->
        {history, agent.window, agent.stall + 1}

      true ->
        {history, agent.window, agent.stall}
    end
  end

  defp window_has_more?(agent, elements) do
    agent.window + max_targets(agent) < Snapshot.candidate_ceiling(elements)
  end

  defp stamp_page_changed([], _changed), do: []

  defp stamp_page_changed(history, changed) do
    List.update_at(history, -1, &Map.put(&1, :page_changed, changed))
  end

  defp last_target_action?([]), do: false

  defp last_target_action?(history) do
    match?(%{operation: operation} when operation in ["CLICK", "TYPE_TEXT"], List.last(history))
  end

  # Chrome's CDP accessibility snapshot fails on wide pages (measured between
  # ~4,000 and ~6,000 nodes), which would strand documentation sites like
  # DevDocs. Fall back to the backend's stamped-DOM table so the loop can still
  # observe — and act on — the page. The source is kept for debugging, and the
  # ref schemes differ (`eN`/`e1_24` vs `dN`), so a single backend must serve
  # both observation and execution.
  defp observe_payload(kuri) do
    case kuri.snap() do
      {:ok, payload} -> {:ok, payload, :a11y}
      {:error, a11y_error} -> dom_payload(kuri, a11y_error)
    end
  end

  defp dom_payload(kuri, a11y_error) do
    case kuri.dom_snapshot() do
      {:ok, payload} ->
        {:ok, payload, :dom}

      {:error, message} ->
        {:error,
         "accessibility snapshot failed (#{a11y_error}) and so did the DOM fallback (#{message})"}
    end
  end

  @doc "Asks Smart Decide for the next operation and target."
  @spec predict(t()) :: {:ok, t()} | {:error, String.t()}
  def predict(%__MODULE__{status: :predicted} = agent), do: {:ok, agent}

  def predict(%__MODULE__{} = agent) do
    opts = filter_opts(agent.opts, [:decider, :model])

    case Policy.choose(agent.page, agent.elements, agent.targets, agent.goal, agent.history, opts) do
      {:ok, decision} -> {:ok, %{agent | decision: decision, status: :predicted}}
      {:error, message} -> {:error, message}
    end
  end

  @doc "Executes the pending decision (generating text for TYPE_TEXT)."
  @spec execute(t()) :: {:ok, t()} | {:error, String.t()}
  def execute(%__MODULE__{decision: nil}), do: {:error, "no decision to execute"}

  def execute(%__MODULE__{} = agent) do
    decision = agent.decision

    with {:ok, text} <- text_for(agent, decision),
         {:ok, _description} <- Executor.execute(decision, text, filter_opts(agent.opts, [:kuri])) do
      entry = history_entry(agent, decision, text)

      {:ok,
       %{agent | decision: nil, history: agent.history ++ [entry], status: terminal(decision)}}
    end
  end

  @doc """
  Renders the agent's current observation and history as a JSON-ready map.

  Includes the bounded page `text` the model was shown, so an API-lookup style
  goal can return what the agent read.
  """
  @spec snapshot(t()) :: map()
  def snapshot(%__MODULE__{} = agent) do
    %{
      "status" => to_string(agent.status),
      "goal" => agent.goal,
      "url" => agent.page[:url],
      "title" => agent.page[:title],
      "observation" => agent.observation,
      "elements" => Snapshot.render(agent.elements),
      "text" => agent.page[:text],
      "step" => length(agent.history),
      "history" => agent.history,
      "error" => agent.error
    }
  end

  # --- internals ---

  defp ensure_observed(%__MODULE__{elements: []} = agent), do: navigate_and_observe(agent)
  defp ensure_observed(agent), do: observe(agent)

  defp navigate_and_observe(%__MODULE__{opts: opts} = agent) do
    with :ok <- maybe_navigate(opts),
         {:ok, agent} <- observe(agent) do
      {:ok, agent}
    end
  end

  defp maybe_navigate(opts) do
    case Keyword.get(opts, :start_url) do
      url when is_binary(url) and url != "" ->
        case kuri_from(opts).go(url) do
          {:ok, _} -> :ok
          {:error, message} -> {:error, message}
        end

      _ ->
        :ok
    end
  end

  defp fetch_page(kuri, _snapshot) do
    text = page_text(kuri) |> String.slice(0, @page_text_limit)
    meta = page_meta(kuri)
    {:ok, %{url: meta["url"], title: meta["title"], text: text}}
  end

  # Prefer the main content pane's text, falling back to the whole-page text
  # when it is empty or the backend cannot read it (see `@main_text_script`).
  defp page_text(kuri) do
    case main_text(kuri) do
      {:ok, text} when text != "" -> text
      _ -> body_text(kuri)
    end
  end

  defp main_text(kuri) do
    with {:ok, value} <- kuri.eval(@main_text_script),
         {:ok, text} <- decode_text(value) do
      {:ok, text}
    end
  end

  # `eval/1` returns the JSON-encoded value; a stub or a non-JSON backend may
  # hand back the text itself, which is used as-is.
  defp decode_text(value) when is_binary(value) do
    case Jason.decode(value) do
      {:ok, decoded} when is_binary(decoded) -> {:ok, decoded}
      {:ok, _} -> {:error, :not_text}
      {:error, _} -> {:ok, value}
    end
  end

  defp decode_text(_value), do: {:error, :not_text}

  defp body_text(kuri) do
    case kuri.text() do
      {:ok, value} when is_binary(value) -> value
      _ -> ""
    end
  end

  defp page_meta(kuri) do
    case kuri.eval("JSON.stringify({url: location.href, title: document.title})") do
      {:ok, value} -> decode_meta(value)
      {:error, _} -> %{}
    end
  end

  defp decode_meta(value) do
    with {:ok, decoded} <- Jason.decode(value) do
      if is_binary(decoded), do: decode_meta(decoded), else: decoded
    else
      _ -> %{}
    end
  end

  defp text_for(%__MODULE__{opts: opts} = agent, %{operation: "TYPE_TEXT"} = decision) do
    helper_opts = filter_opts(opts, [:generator, :model])

    case TextHelper.field_text(agent.goal, decision, agent.page, agent.history, helper_opts) do
      {:ok, value, _meta} -> {:ok, value}
      {:error, message} -> {:error, message}
    end
  end

  defp text_for(_agent, _decision), do: {:ok, nil}

  defp history_entry(agent, decision, text) do
    %{
      step: length(agent.history) + 1,
      operation: decision.operation,
      target: decision.target,
      ref: decision.ref,
      label: decision.label,
      text: text,
      confidence: decision.confidence,
      # Settled by the next `observe/1` (nil until then).
      page_changed: nil,
      elapsed_ms: elapsed(agent)
    }
  end

  defp terminal(%{operation: "DONE"}), do: :done
  defp terminal(%{operation: "BLOCKED"}), do: :blocked
  defp terminal(_), do: :ready

  # Field values # Field values and toggle states are part of the fingerprint, so typing into a
  # field or switching a control reads as a change even though the element set and
  # article text are untouched. Clicking moves focus and a docs sidebar anchor
  # only changes the URL fragment, neither of which is a page change, so focus is
  # dropped and the fragment is ignored — otherwise every click would look like a
  # navigation and reset the candidate window.
  defp fingerprint(elements, page) do
    :erlang.phash2(
      {Enum.map(elements, &{&1.ref, &1.value, focus_free(&1.state)}),
       url_without_fragment(page[:url]), page[:text]}
    )
    |> Integer.to_string(16)
  end

  defp focus_free(state) when is_binary(state) do
    state
    |> String.split()
    |> Enum.reject(&(&1 == "focused"))
    |> Enum.join(" ")
  end

  defp focus_free(_state), do: ""

  defp url_without_fragment(nil), do: ""

  defp url_without_fragment(url), do: url |> String.split("#") |> hd()

  defp elapsed(%__MODULE__{started_at: nil}), do: 0

  defp elapsed(%__MODULE__{started_at: started}),
    do: System.monotonic_time(:millisecond) - started

  defp kuri(agent), do: kuri_from(agent.opts)
  defp kuri_from(opts), do: Keyword.get(opts, :kuri, Exhub.BrowserAgent.Kuri)
  defp max_targets(agent), do: Keyword.get(agent.opts, :max_targets, 16)
  defp max_elements(agent), do: Keyword.get(agent.opts, :max_elements, @default_max_elements)

  defp filter_opts(opts, keys) do
    Enum.filter(opts, fn {key, _} -> key in keys end)
  end
end
