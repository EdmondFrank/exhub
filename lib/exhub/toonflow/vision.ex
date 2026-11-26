defmodule Exhub.Toonflow.Vision do
  @moduledoc """
  Vision seam for Toonflow: image → text.

  `Exhub.Toonflow.Reverse` uses it to read a reference sheet back into a
  description. Kept behind a behaviour so tests never hit the network:

      Application.put_env(:exhub, :toonflow_vision, MyStub)
  """

  @callback analyze(path :: String.t(), opts :: keyword()) ::
              {:ok, String.t()} | {:error, term()}

  @doc "The configured implementation module."
  @spec impl() :: module()
  def impl, do: Application.get_env(:exhub, :toonflow_vision, Exhub.Toonflow.Vision.Default)

  @doc """
  Describe the image at `path`.

  Options: `:prompt` (required), `:model` (defaults to the implementation's
  model). Returns `{:ok, text}` or `{:error, reason}`.
  """
  @spec analyze(String.t(), keyword()) :: {:ok, String.t()} | {:error, term()}
  def analyze(path, opts \\ []), do: impl().analyze(path, opts)
end

defmodule Exhub.Toonflow.Vision.Default do
  @moduledoc """
  Default `Exhub.Toonflow.Vision` — one MoArk vision request, sent through
  `Exhub.MCP.Tools.Look.analyze/2` (the same path the `look` tool uses).

  `mode: "analyze"` and `thinking: false` keep the reply a single description
  instead of a reasoning transcript, and the call runs against a local file so
  the canvas never has to expose an upload URL.
  """

  @behaviour Exhub.Toonflow.Vision

  @impl true
  def analyze(path, opts) do
    opts
    |> look_opts()
    |> then(&Exhub.MCP.Tools.Look.analyze(path, &1))
    |> case do
      {:ok, text} -> {:ok, text}
      {:error, reason} -> {:error, {:vision_failed, reason}}
    end
  end

  defp look_opts(opts) do
    base = [
      prompt: Keyword.fetch!(opts, :prompt),
      mode: "analyze",
      thinking: false
    ]

    case Keyword.get(opts, :model) do
      model when is_binary(model) and model != "" -> Keyword.put(base, :model, model)
      _ -> base
    end
  end
end
