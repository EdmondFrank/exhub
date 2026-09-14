defmodule Exhub.MCP.Tools.Think do
  @moduledoc """
  MCP Tool for thinking about something — an external reasoning scratchpad.

  Each call appends the thought to a per-session scratchpad (see
  `Exhub.MCP.ScratchpadStore`, keyed on `frame.context.session_id`) and returns
  the accumulated notes as a JSON envelope instead of echoing the single thought
  back:

      {"recorded": N, "scratchpad": [...], "next": "<nudge>"}

  This gives the model consolidated working memory it can re-read on every
  turn, a counter that makes runaway loops visible, and a nudge to act on the
  recorded state rather than re-thinking it.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Scratchpad

  use Anubis.Server.Component, type: :tool

  @assigns_key :think_notes
  @invalid_placeholder "The tool call did not contain a valid thought."
  @nudge "Resolve an obligation already listed in the scratchpad or test a " <>
           "claimed assumption before answering. Call think again only when " <>
           "you have materially new reasoning state."

  def name, do: "think"

  @impl true
  def description do
    """
    Use the tool to think about something. It will not obtain new information
    or change anything, but records the thought in a persistent scratchpad and
    returns all thoughts so far, so you can reason step by step against a
    durable working memory. Restate the problem, break it into sub-problems,
    resolve each explicitly with intermediate results, then check your work
    (including at least one boundary case) before producing the final answer.
    Call again only for materially new reasoning state.
    """
  end

  schema do
    field(:thought, {:required, :string}, description: "A thought to think about.")
  end

  @impl true
  def execute(params, frame) do
    thought = Scratchpad.normalize(Map.get(params, :thought), @invalid_placeholder)

    entries = Scratchpad.append(session_id(frame), @assigns_key, thought)

    resp = Response.tool() |> Response.json(Scratchpad.envelope(entries, @nudge))
    {:reply, resp, frame}
  end

  # Transport-independent bucket key. Populated by both Anubis.Server.Session
  # and Exhub.MCP.ConcurrentToolDispatcher; falls back to a shared default so a
  # missing id degrades to one scratchpad rather than crashing the tool.
  defp session_id(%{context: %{session_id: sid}}) when is_binary(sid), do: sid
  defp session_id(_frame), do: "__default__"
end
