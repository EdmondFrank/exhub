# PRD: ExHub Hercules — AI-Powered E2E Testing Agent

**Version:** 1.0  
**Date:** 2026-07-21  
**Author:** ExHub Team  
**Status:** Draft

---

## 1. Executive Summary

ExHub Hercules is an Elixir/OTP port of [TestZeus Hercules](https://github.com/test-zeus-ai/testzeus-hercules), the world's first open-source AI testing agent. It converts natural-language test descriptions (Gherkin BDD or plain English) into fully automated end-to-end tests — no test code required.

The port leverages ExHub's existing infrastructure (kuri-agent browser automation, Anubis MCP servers, LangChain.ex, Sagents orchestration) to deliver a **concurrent, fault-tolerant, MCP-native** testing agent that runs on the BEAM VM.

---

## 2. Problem Statement

| Pain Point | Description |
|---|---|
| **Test authoring cost** | Writing and maintaining Selenium/Playwright test scripts requires engineering effort proportional to feature velocity. |
| **Brittleness** | Selector-based tests break on UI changes; AI-driven tests adapt via accessibility-tree reasoning. |
| **Python GIL & concurrency** | The original Hercules runs one test at a time per process; parallel execution requires external orchestration. |
| **Integration gap** | ExHub already has browser automation (kuri-agent), MCP servers, and agent orchestration — but no testing workflow. |
| **Long-running stability** | Python processes leak memory over multi-hour test suites; BEAM VM is designed for long-lived processes. |

---

## 3. Goals & Non-Goals

### Goals

1. **G1** — Execute Gherkin `.feature` files and plain-English test descriptions as automated E2E tests.
2. **G2** — Provide a Planner→Executor→Assertion orchestration loop with multi-turn tool-calling nav agents.
3. **G3** — Reuse ExHub's kuri-agent (CDP) for browser automation — zero Playwright dependency.
4. **G4** — Expose as an MCP server (streamable-http) so any MCP client (Claude Code, Cursor, AiderDesk) can trigger tests.
5. **G5** — Support parallel test execution via OTP process isolation.
6. **G6** — Produce JUnit XML reports + screenshot proof + agent thought logs.
7. **G7** — Hot-reloadable prompts and tool definitions without restart.

### Non-Goals (v1)

- Mobile app testing (iOS/Android)
- Visual regression / pixel-diff comparison (phase 2)
- Distributed cluster execution across multiple nodes
- Full parity with every Hercules extra tool (geo, clipboard, drag-drop)
- CI/CD pipeline integration (GitHub Actions, Jenkins) — phase 2

---

## 4. User Personas

| Persona | Description | Primary Use Case |
|---|---|---|
| **QA Engineer** | Writes Gherkin features, reviews results | Author tests in natural language, get JUnit reports |
| **Developer** | Wants quick smoke tests during PR review | Paste a scenario, get pass/fail in minutes |
| **AI Agent (MCP client)** | Claude Code / AiderDesk orchestrating multi-step workflows | Call `run_test` tool programmatically |
| **Tech Lead** | Oversees test coverage and reliability | Review agent thought logs, tune prompts |

---

## 5. Functional Requirements

### 5.1 Test Input

| ID | Requirement | Priority |
|---|---|---|
| FR-1 | Accept Gherkin `.feature` file content (string or file path) | P0 |
| FR-2 | Accept plain-English test description and auto-generate Gherkin | P1 |
| FR-3 | Support test data injection (YAML/JSON data files, `$variable` substitution) | P1 |
| FR-4 | Support multiple scenarios per feature file (sequential execution) | P0 |

### 5.2 Orchestration

| ID | Requirement | Priority |
|---|---|---|
| FR-5 | Planner agent produces structured JSON: `{plan, next_step, target_helper, terminate, is_assert, ...}` | P0 |
| FR-6 | Executor routes to the correct nav agent based on `target_helper` | P0 |
| FR-7 | Nav agents run multi-turn tool-calling loops until `##TERMINATE TASK##` or max rounds | P0 |
| FR-8 | Planner receives helper response and decides: continue / assert / terminate | P0 |
| FR-9 | Context-limit fallback: compress message history when token limit hit | P1 |
| FR-10 | Configurable max planner rounds (default 500) and nav rounds (default 50) | P0 |

### 5.3 Browser Automation (via kuri-agent CDP)

| ID | Requirement | Priority |
|---|---|---|
| FR-11 | Navigate to URL | P0 |
| FR-12 | Click elements (by selector, text, or accessibility ref) | P0 |
| FR-13 | Fill input fields | P0 |
| FR-14 | Read page content / accessibility snapshot | P0 |
| FR-15 | Take screenshots (proof logging) | P0 |
| FR-16 | Handle tabs (open, switch, close) | P1 |
| FR-17 | Select dropdown options | P1 |
| FR-18 | Press key combinations (Enter, Tab, Escape) | P1 |
| FR-19 | Hover elements | P2 |
| FR-20 | Upload files | P2 |

### 5.4 API Testing

| ID | Requirement | Priority |
|---|---|---|
| FR-21 | HTTP GET/POST/PUT/DELETE with custom headers/body | P1 |
| FR-22 | Response validation (status code, body assertions) | P1 |

### 5.5 Reporting & Observability

| ID | Requirement | Priority |
|---|---|---|
| FR-23 | JUnit XML report output | P0 |
| FR-24 | Screenshot proof directory per test run | P0 |
| FR-25 | Agent inner thoughts JSON log (planner decisions + helper responses) | P1 |
| FR-26 | Token usage & cost accounting per run | P2 |
| FR-27 | Step timing breakdown (planner vs executor durations) | P2 |

### 5.6 MCP Server Interface

| ID | Requirement | Priority |
|---|---|---|
| FR-28 | `run_test` tool: execute a feature/description, return results | P0 |
| FR-29 | `generate_gherkin` tool: convert plain English → Gherkin | P1 |
| FR-30 | `get_test_results` tool: fetch last run's JUnit XML + metadata | P0 |
| FR-31 | `list_test_runs` tool: list historical runs with status | P2 |

### 5.7 Configuration

| ID | Requirement | Priority |
|---|---|---|
| FR-32 | Per-agent LLM model config (planner model, nav model) via `LlmConfigServer` | P0 |
| FR-33 | Custom system prompts per agent (overridable at runtime) | P1 |
| FR-34 | Test data directory configuration | P1 |

---

## 6. Non-Functional Requirements

| ID | Requirement | Target |
|---|---|---|
| NFR-1 | **Concurrency** | Support ≥5 parallel test runs without interference |
| NFR-2 | **Fault tolerance** | Nav agent crash → orchestrator retries or reports failure, doesn't die |
| NFR-3 | **Latency** | Planner decision < 30s (LLM-dependent); tool execution < 10s per call |
| NFR-4 | **Memory** | < 256MB per test run process (excluding browser) |
| NFR-5 | **Uptime** | Orchestrator GenServer survives individual test failures |
| NFR-6 | **Hot reload** | Prompt/tool changes take effect without BEAM restart |
| NFR-7 | **Observability** | Structured logging (Logger) with run_id correlation |

---

## 7. Success Metrics

| Metric | Target (v1 GA) |
|---|---|
| Gherkin scenario pass rate (on stable demo apps) | ≥ 80% |
| Average time per scenario (5-step flow) | < 3 minutes |
| MCP tool call success rate | ≥ 95% |
| Zero test-code required for standard web flows | ✓ |
| Parallel runs supported | ≥ 5 |

---

## 8. Milestones

| Phase | Deliverable | Timeline |
|---|---|---|
| **Phase 1: Core Loop** | Orchestrator + Planner + Browser NavAgent + kuri-agent tools | Week 1-2 |
| **Phase 2: Reporting** | JUnit XML, screenshots, thought logs | Week 2 |
| **Phase 3: MCP Server** | Anubis server with run_test / get_results / generate_gherkin | Week 3 |
| **Phase 4: API Agent** | HTTP request tools + response validation | Week 3 |
| **Phase 5: Gherkin Parser** | Parse .feature files, scenario iteration, data tables | Week 4 |
| **Phase 6: Polish** | Config UI, parallel runs, token accounting, hot-reload prompts | Week 4-5 |

---

## 9. Risks & Mitigations

| Risk | Impact | Mitigation |
|---|---|---|
| kuri-agent lacks a Hercules tool (e.g., drag-drop) | Missing test coverage | Implement as direct CDP commands via Exile; or defer to phase 6 |
| LLM hallucination in planner (invents steps) | False failures | Strict JSON schema validation + step dedup + max-round guard |
| Context window overflow on long tests | Crash / degraded quality | Message compression fallback (summarize old turns) |
| Browser state drift (popups, modals) | Nav agent stuck | Auto-dismiss strategy + stale-tool-call detection (port from Hercules) |
| Token cost explosion on retries | Budget overrun | Per-run token budget cap; abort with report when exceeded |

---

## 10. Dependencies

| Dependency | Purpose | Status |
|---|---|---|
| `kuri-agent` (Zig binary) | Chrome CDP automation | ✅ Already in ExHub |
| `LangChain.ex` | LLM interaction, tool-calling | ✅ Already in ExHub |
| `Anubis` | MCP server framework | ✅ Already in ExHub |
| `Exhub.Llm.LlmConfigServer` | Model configuration | ✅ Already in ExHub |
| `Req` / `Finch` | HTTP client for API testing | ✅ Available |
| `ElixirMake` / `JunitFormatter` | JUnit XML generation | ⬜ Add dependency |
| Gherkin parser lib (or custom) | Parse .feature files | ⬜ Evaluate `gherkin` hex pkg or custom |

---

## 11. Open Questions

1. Should the planner use a dedicated "reasoning" model (e.g., o3, DeepSeek-R1) vs. a fast model (GPT-4o)?
2. Do we need a persistent test-run database (SQLite/ETS) or is file-based output sufficient for v1?
3. Should parallel runs share a single browser instance (tabs) or each get their own?
4. Do we expose the orchestrator as a Sagents agent (for AiderDesk delegation) in addition to MCP?

---

## Appendix A: Hercules → ExHub Concept Mapping

| Hercules (Python) | ExHub Hercules (Elixir) |
|---|---|
| `SimpleHercules` (LangGraph) | `Exhub.Hercules.Orchestrator` (GenServer/Task) |
| `PlannerAgent` | `Exhub.Hercules.Planner` |
| `BrowserNavAgent` | `Exhub.Hercules.NavAgents.Browser` |
| `ApiNavAgent` | `Exhub.Hercules.NavAgents.Api` |
| `PlaywrightManager` | `Exhub.KuriDaemon` + `Exhub.MCP.Tools.BrowserUse.*` |
| `tool_registry` (dict) | `LangChain.Function` definitions per nav agent |
| `@tool` decorator | `def tool_name(...)` + `LangChain.Function.new!` |
| `StaticLTM` | `Exhub.Hercules.TestData` (loads YAML/JSON) |
| `FastMCP` server | `Exhub.Hercules.MCPServer` (Anubis) |
| `BaseRunner` | `Exhub.Hercules.Runner` (GenServer) |
| `agents_llm_config.json` | `Exhub.Llm.LlmConfigServer` entries |
