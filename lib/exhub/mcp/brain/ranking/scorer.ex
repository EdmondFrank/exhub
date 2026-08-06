defmodule Exhub.MCP.Brain.Ranking.Scorer do
  @moduledoc """
  Behaviour for individual scoring functions used by the Brain ranking pipeline.

  Each scorer produces a normalized score in `[0, 1]` for a note given a
  search context. Scorers are composed by `Exhub.MCP.Brain.Ranking.Ranker`
  and combined via a fusion strategy.

  ## Context

  The `context` map passed to `score/2` carries per-search data:

    * `:query`       - the raw search query string
    * `:query_terms` - tokenized query terms
    * `:is_tag_search` - whether the query is an explicit `tag:` search
    * `:tag_query`   - the normalized tag term for `tag:` searches
    * `:vault`       - the vault root path
    * `:docs_data`   - precomputed document statistics (BM25)
    * `:avgdl`, `:doc_count` - BM25 corpus statistics
    * `:backlinks`   - precomputed `%{note_path => count}` (lazy)
  """

  @type note :: map()
  @type context :: map()
  @type score :: float()

  @callback name() :: atom()
  @callback score(note(), context()) :: score()
  @callback weight() :: float()
end