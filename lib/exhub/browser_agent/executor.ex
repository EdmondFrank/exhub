defmodule Exhub.BrowserAgent.Executor do
  @moduledoc """
  Turns a chosen decision into a `kuri-agent` action.

  The model never emits selectors, coordinates, or code: a decision carries an
  operation plus a reference to an observed element, and this module maps that
  to the matching kuri command. `TYPE_TEXT` receives the value produced by
  `Exhub.BrowserAgent.TextHelper`.

  The `:kuri` module is injectable for tests (default
  `Exhub.BrowserAgent.Kuri`).
  """

  @wait_ms 100

  @typedoc "A decision produced by `Exhub.BrowserAgent.Policy.choose/6`."
  @type decision :: map()

  @doc """
  Executes `decision`, typing `text` for `TYPE_TEXT`.

  Returns `{:ok, description}` on success or `{:error, message}`. `DONE` and
  `BLOCKED` are terminal and perform no browser action.
  """
  @spec execute(decision(), String.t() | nil, keyword()) ::
          {:ok, String.t()} | {:error, String.t()}
  def execute(decision, text \\ nil, opts \\ []) do
    kuri = Keyword.get(opts, :kuri, Exhub.BrowserAgent.Kuri)

    case decision.operation do
      "CLICK" ->
        with_ref(decision, fn ref -> kuri.click(ref) end, "clicked #{describe(decision)}")

      "TYPE_TEXT" ->
        type_text(decision, text, kuri)

      "SCROLL_UP" ->
        wrap(kuri.scroll(:up), "scrolled up")

      "SCROLL_DOWN" ->
        wrap(kuri.scroll(:down), "scrolled down")

      "WAIT" ->
        Process.sleep(@wait_ms)
        {:ok, "waited #{@wait_ms}ms"}

      "DONE" ->
        {:ok, "done"}

      "BLOCKED" ->
        {:ok, "blocked"}

      other ->
        {:error, "unsupported operation #{inspect(other)}"}
    end
  end

  defp type_text(_decision, nil, _kuri), do: {:error, "TYPE_TEXT requires a text value"}

  defp type_text(decision, text, kuri) do
    with_ref(
      decision,
      fn ref -> kuri.fill(ref, text) end,
      "filled #{describe(decision)} with #{inspect(text)}"
    )
  end

  defp with_ref(decision, fun, success) do
    case decision[:ref] do
      ref when is_binary(ref) and ref != "" -> wrap(fun.(ref), success)
      _ -> {:error, "#{decision.operation} requires an element ref"}
    end
  end

  defp wrap({:ok, _stdout}, success), do: {:ok, success}
  defp wrap({:error, message}, _success), do: {:error, message}

  defp describe(decision), do: decision[:label] || decision[:ref] || decision.operation
end
