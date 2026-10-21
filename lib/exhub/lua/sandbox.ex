defmodule Exhub.Lua.Sandbox do
  @moduledoc """
  Behaviour for a code-execution sandbox, shaped after Legion's
  `Legion.Sandbox`.

  `Exhub.Lua.Engine` drives a sandbox through these callbacks, so the execution
  language can be swapped (or a second one added) without touching the
  orchestration, resource bounds or result formatting.

    * `check/2` — validate the request (`code`, `tools`) before any process is
      spawned; returns `:ok` or `{:error, message}`.
    * `execute/4` — run one evaluation (inside the bounded process) and return
      `{:ok, results, logs}` | `{:partial, report, logs}` | `{:lua_error, message}`.
    * `binding_names/0` — the globals the sandbox owns (reserved names).
    * `prompt_info/0` — language facts for prompt / tool-description rendering.
  """

  @type tools :: [map()]
  @type call_fun :: (String.t(), String.t(), map() -> {:ok, term()} | {:error, term()})
  @type config :: keyword()

  @callback check(code :: term(), tools :: term()) :: :ok | {:error, String.t()}

  @callback execute(
              code :: String.t(),
              tools :: tools(),
              call_fun :: call_fun(),
              config :: config()
            ) ::
              {:ok, term(), [String.t()]}
              | {:partial, String.t(), [String.t()]}
              | {:lua_error, String.t()}

  @callback binding_names() :: [String.t()]

  @callback prompt_info() :: map()
end
