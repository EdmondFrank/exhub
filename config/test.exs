import Config

# Use a random available port during tests to avoid conflicts with a running dev instance.
config :exhub, :port, 0

# Keep Brain vault search deterministic and offline in tests: the Smart Decide
# relevance filter is exercised directly (with an injected decider) in
# test/exhub/mcp/brain/search/relevance_test.exs.
config :exhub, Exhub.MCP.Brain.Search.Relevance, enabled: false

# Likewise for Desktop `search_files` semantic mode: disable the Smart Decide
# relevance filter by default so the probe-backed tool tests stay offline. The
# filter is exercised directly (with an injected decider) in
# test/exhub/mcp/desktop/search/relevance_test.exs and
# test/exhub/mcp/tools/desktop/search_files_filter_test.exs.
config :exhub, Exhub.MCP.Desktop.Search.Relevance, enabled: false

# Likewise for the Desktop shell tools' working-dir gate: disable the Smart
# Decide decision by default so `execute_command`/`start_process` tests stay
# offline and deterministic. The gate is exercised directly (with an injected
# decider) in test/exhub/mcp/tools/desktop/working_dir_test.exs.
config :exhub, Exhub.MCP.Desktop.WorkingDir, enabled: false

# Same reasoning for the proxy-env gate: `execute_command`/`start_process` tests
# must stay offline and deterministic, so no retry and no TCP probe happen by
# default. Exercised directly (with injected decider/probe) in
# test/exhub/mcp/tools/desktop/proxy_env_test.exs.
config :exhub, Exhub.MCP.Desktop.ProxyEnv, enabled: false, target_probe: false
