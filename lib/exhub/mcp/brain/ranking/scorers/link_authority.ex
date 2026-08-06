defmodule Exhub.MCP.Brain.Ranking.Scorers.LinkAuthority do
  @moduledoc """
  Backlink authority scorer.

  Ranks notes by how many other notes in the vault link to them via
  `[[wikilink]]` syntax (analogous to PageRank / citation count). The count is
  provided lazily in the context `:backlinks` map (`%{note_path => count}`).
  Scores are normalized against `@max_backlinks`. Notes with no backlinks get
  `0.0`.
  """

  @behaviour Exhub.MCP.Brain.Ranking.Scorer

  @max_backlinks 20

  @impl true
  def name, do: :link_authority

  @impl true
  def weight, do: 0.05

  @impl true
  def score(note, context) do
    backlinks = Map.get(context, :backlinks, %{})
    count = Map.get(backlinks, Map.get(note, :file), 0)
    min(1.0, count / @max_backlinks)
  end
end