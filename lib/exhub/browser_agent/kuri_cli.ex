defmodule Exhub.BrowserAgent.KuriCli do
  @moduledoc """
  The `kuri-agent` CLI backend: one OS process per browser command.

  Each function runs `kuri-agent` with the given argv via `Exile` and returns
  `{:ok, stdout}` on a zero exit status, or `{:error, message}` (including the
  captured stderr) otherwise. This is the only module in the browser-agent stack
  that shells out, which keeps the loop, policy, and snapshot logic pure and
  unit-testable behind an injectable `:kuri` module.

  `kuri-agent` assigns accessibility refs per process, so a ref printed by one
  `snap` invocation cannot be resolved by a later `click` (it fails with
  ``ref 'eN' not found. Run `kuri-agent snap` first.``). DOM-derived refs (`dN`)
  are therefore replayed through `eval` instead of the ref-taking commands, and
  `Exhub.BrowserAgent.KuriHttp` (the default backend) keeps refs server-side.
  """

  alias Exhub.BrowserAgent.{DomBridge, Scroll}

  @default_timeout 60_000

  @typedoc "Result of a kuri-agent invocation."
  @type result :: {:ok, String.t()} | {:error, String.t()}

  @doc """
  Runs `kuri-agent` with `argv` (which must not include the binary name).

  Returns `{:ok, stdout}` when the process exits `0`, otherwise `{:error, message}`
  with the captured stderr.
  """
  @spec run([String.t()]) :: result()
  def run(argv) when is_list(argv) do
    full_argv = ["kuri-agent" | argv]

    # Exile.stream/2 has no `:timeout` option, so bound the call by running it
    # in a task and killing it (which closes the port) if it overruns.
    task = Task.async(fn -> collect(full_argv) end)

    case Task.yield(task, @default_timeout) || Task.shutdown(task, :brutal_kill) do
      {:ok, {stdout, stderr, exit_status}} ->
        if exit_status == 0 do
          {:ok, stdout}
        else
          {:error, error_message(argv, exit_status, stderr)}
        end

      {:exit, reason} ->
        {:error, "Failed to run kuri-agent: #{failure(reason)}"}

      nil ->
        {:error, "kuri-agent #{Enum.join(argv, " ")} timed out after #{@default_timeout}ms"}
    end
  end

  defp collect(full_argv) do
    Exile.stream(full_argv, stderr: :consume)
    |> Enum.reduce({"", "", 0}, fn
      {:stdout, data}, {out, err, code} -> {out <> data, err, code}
      {:stderr, data}, {out, err, code} -> {out, err <> data, code}
      {:exit, {:status, code}}, {out, err, _} -> {out, err, code}
      {:exit, :epipe}, {out, err, _} -> {out, err, 0}
      _, acc -> acc
    end)
  end

  defp failure({exception, _stacktrace}) when is_exception(exception),
    do: Exception.message(exception)

  defp failure(reason), do: inspect(reason)

  @doc "Takes an accessibility snapshot of the attached tab (compact text tree)."
  @spec snap(keyword()) :: result()
  def snap(opts \\ []) do
    argv =
      ["snap"]
      |> maybe_flag("--interactive", Keyword.get(opts, :interactive))
      |> maybe_opt("--depth", Keyword.get(opts, :depth))

    run(argv)
  end

  @doc """
  Reads the page as a stamped DOM table, for pages whose accessibility tree
  cannot be fetched.
  """
  @spec dom_snapshot(keyword()) :: result()
  def dom_snapshot(_opts \\ []) do
    case run(["eval", DomBridge.snapshot_script()]) do
      {:ok, output} -> DomBridge.extract_json(output)
      {:error, message} -> {:error, message}
    end
  end

  @doc "Returns the visible page text."
  @spec text() :: result()
  def text, do: run(["text"])

  @doc "Clicks the element with the given ref (`eN`, `@eN`, or a `dN` DOM ref)."
  @spec click(String.t()) :: result()
  def click(ref) do
    if DomBridge.dom_ref?(ref), do: dom_action(ref, :click, nil), else: run(["click", ref])
  end

  @doc "Clears and fills an editable element. Prefer over `type/2` to replace."
  @spec fill(String.t(), String.t()) :: result()
  def fill(ref, value) do
    if DomBridge.dom_ref?(ref), do: dom_action(ref, :fill, value), else: run(["fill", ref, value])
  end

  @doc "Types text into an element without clearing it first."
  @spec type(String.t(), String.t()) :: result()
  def type(ref, value) do
    if DomBridge.dom_ref?(ref), do: dom_action(ref, :type, value), else: run(["type", ref, value])
  end

  @doc "Selects a value from a dropdown."
  @spec select(String.t(), String.t()) :: result()
  def select(ref, value) do
    if DomBridge.dom_ref?(ref),
      do: dom_action(ref, :select, value),
      else: run(["select", ref, value])
  end

  @doc """
  Scrolls the page one viewport in `direction` (`:up` or `:down`).

  The CLI has a down-only `scroll` command, so `:up` replays the shared scroller
  script through `eval`.
  """
  @spec scroll(:up | :down) :: result()
  def scroll(:down), do: run(["scroll"])

  def scroll(:up) do
    case eval(Scroll.script(:up)) do
      {:ok, _offset} -> {:ok, "scrolled up"}
      {:error, message} -> {:error, message}
    end
  end

  @doc "Evaluates a JavaScript expression in the page."
  @spec eval(String.t()) :: result()
  def eval(expression), do: run(["eval", expression])

  @doc "Navigates the attached tab to `url`."
  @spec go(String.t()) :: result()
  def go(url), do: run(["go", url])

  @doc "Lists open Chrome tabs."
  @spec tabs() :: result()
  def tabs, do: run(["tabs"])

  @doc "Attaches to the tab with the given CDP WebSocket URL."
  @spec use(String.t()) :: result()
  def use(ws_url), do: run(["use", ws_url])

  @doc "Shows the current session."
  @spec status() :: result()
  def status, do: run(["status"])

  defp dom_action(ref, action, value) do
    case run(["eval", DomBridge.action_script(ref, action, value)]) do
      {:ok, output} ->
        case DomBridge.interpret_action(output) do
          :ok -> {:ok, "dom #{action} #{ref}"}
          {:error, message} -> {:error, message}
        end

      {:error, message} ->
        {:error, message}
    end
  end

  defp maybe_flag(argv, _flag, nil), do: argv
  defp maybe_flag(argv, _flag, false), do: argv
  defp maybe_flag(argv, flag, true), do: argv ++ [flag]

  defp maybe_opt(argv, _opt, nil), do: argv
  defp maybe_opt(argv, opt, value), do: argv ++ [opt, to_string(value)]

  defp error_message(argv, exit_status, stderr) do
    detail = String.trim(stderr)

    detail =
      if detail == "" do
        "no stderr output"
      else
        detail
      end

    "kuri-agent #{Enum.join(argv, " ")} exited #{exit_status}: #{detail}"
  end
end
