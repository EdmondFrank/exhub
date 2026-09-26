defmodule Exhub.BrowserAgent.Kuri do
  @moduledoc """
  Facade over the browser backends the Jev loop can drive.

  `Exhub.BrowserAgent.Agent` and `Exhub.BrowserAgent.Executor` talk to this
  module, which forwards every call to the configured backend:

    * `Exhub.BrowserAgent.KuriHttp` — the ExHub-managed `kuri` daemon HTTP API
      (default). Refs live server-side, so `snap` → `click` works.
    * `Exhub.BrowserAgent.KuriCli` — the `kuri-agent` CLI, one process per
      command. Its refs are per-process, so ref-based actions cannot succeed.

  Configure with:

      config :exhub, Exhub.BrowserAgent, backend: :http | :cli

  Tests inject a stub module instead (`Agent.new(goal, kuri: StubKuri)`), which is
  why `backend_for/1` stays a pure function.
  """

  alias Exhub.BrowserAgent.{KuriCli, KuriHttp}

  @backends %{http: KuriHttp, cli: KuriCli}

  # The setting lives under the feature namespace `Exhub.BrowserAgent` (see
  # config/config.exs), not under this facade module — reading it from
  # `__MODULE__` silently ignored `backend: :cli`.
  @config_key Exhub.BrowserAgent

  @doc "Returns the configured backend module."
  @spec backend() :: module()
  def backend, do: backend_for(Application.get_env(:exhub, @config_key, [])[:backend])

  @doc """
  Maps a `:backend` setting (or an explicit module) to a backend module.

  `nil` and `:http` select the daemon; `:cli` selects `kuri-agent`.
  """
  @spec backend_for(term()) :: module()
  def backend_for(nil), do: KuriHttp
  def backend_for(key) when is_map_key(@backends, key), do: Map.fetch!(@backends, key)
  def backend_for(module) when is_atom(module), do: module

  @doc "Takes an accessibility snapshot of the attached tab."
  @spec snap(keyword()) :: {:ok, term()} | {:error, String.t()}
  def snap(opts \\ []), do: backend().snap(opts)

  @doc "Reads the page as a stamped DOM table (fallback for wide pages)."
  @spec dom_snapshot(keyword()) :: {:ok, term()} | {:error, String.t()}
  def dom_snapshot(opts \\ []), do: backend().dom_snapshot(opts)

  @doc "Returns the visible page text."
  @spec text() :: {:ok, String.t()} | {:error, String.t()}
  def text, do: backend().text()

  @doc "Clicks the element with the given ref."
  @spec click(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def click(ref), do: backend().click(ref)

  @doc "Clears and fills an editable element."
  @spec fill(String.t(), String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def fill(ref, value), do: backend().fill(ref, value)

  @doc "Types text into an element without clearing it first."
  @spec type(String.t(), String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def type(ref, value), do: backend().type(ref, value)

  @doc "Selects a value from a dropdown."
  @spec select(String.t(), String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def select(ref, value), do: backend().select(ref, value)

  @doc "Scrolls the page one viewport in `direction` (`:up` or `:down`)."
  @spec scroll(:up | :down) :: {:ok, String.t()} | {:error, String.t()}
  def scroll(direction \\ :down), do: backend().scroll(direction)

  @doc "Evaluates a JavaScript expression in the page."
  @spec eval(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def eval(expression), do: backend().eval(expression)

  @doc "Navigates the attached tab to `url`."
  @spec go(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def go(url), do: backend().go(url)

  @doc "Lists open Chrome tabs."
  @spec tabs() :: {:ok, String.t()} | {:error, String.t()}
  def tabs, do: backend().tabs()

  @doc "Attaches to a tab by CDP WebSocket URL."
  @spec use(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def use(ws_url), do: backend().use(ws_url)

  @doc "Shows the current session."
  @spec status() :: {:ok, String.t()} | {:error, String.t()}
  def status, do: backend().status()
end
