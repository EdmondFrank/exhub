import Config

# Use a random available port during tests to avoid conflicts with a running dev instance.
config :exhub, :port, 0

# Keep Brain vault search deterministic and offline in tests: the Smart Decide
# relevance filter is exercised directly (with an injected decider) in
# test/exhub/mcp/brain/search/relevance_test.exs.
config :exhub, Exhub.MCP.Brain.Search.Relevance, enabled: false
