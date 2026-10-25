defmodule Exhub.Toonflow.LLM do
  @moduledoc """
  LLM seam for the Toonflow pipeline.

  Keeps `Exhub.Toonflow.*` decoupled from any particular client by routing
  through a behaviour (default: `Exhub.Toonflow.LLM.Default`, which delegates to
  `Exhub.Genclaw.LLMHelper`). Tests can install a stub with:

      Application.put_env(:exhub, :toonflow_llm, MyStub)
  """

  @callback call_llm(system :: String.t(), user :: String.t(), keyword()) ::
              {:ok, String.t()} | {:error, term()}

  @doc "The configured implementation module."
  @spec impl() :: module()
  def impl, do: Application.get_env(:exhub, :toonflow_llm, Exhub.Toonflow.LLM.Default)

  @doc "Call the configured LLM with a system and a user prompt."
  @spec call_llm(String.t(), String.t(), keyword()) :: {:ok, String.t()} | {:error, term()}
  def call_llm(system, user, opts \\ []), do: impl().call_llm(system, user, opts)

  @doc """
  Reduce an LLM failure to a JSON-safe, human-readable reason.

  Failures arrive as arbitrary terms: a `%LangChain.LangChainError{}` struct for
  a transport timeout, an exception, a plain string. The reason travels into the
  MCP response and the progress websocket, both of which JSON-encode it — and
  `JSON.Encoder` implements nothing for a struct — so it is wrapped as
  `{:llm_failed, text}` here, at the seam, rather than in every caller.
  """
  @spec error_reason(term()) :: {:llm_failed, String.t()}
  def error_reason(reason) when is_binary(reason), do: {:llm_failed, reason}
  def error_reason(reason), do: {:llm_failed, inspect(reason)}
end

defmodule Exhub.Toonflow.LLM.Default do
  @moduledoc "Default `Exhub.Toonflow.LLM` implementation — delegates to GenClaw's helper."

  @behaviour Exhub.Toonflow.LLM

  @impl true
  def call_llm(system, user, opts) do
    opts = Keyword.put_new(opts, :receive_timeout, receive_timeout())

    case Exhub.Genclaw.LLMHelper.call_llm(system, user, opts) do
      {:ok, text} -> {:ok, text}
      {:error, reason, _model} -> {:error, Exhub.Toonflow.LLM.error_reason(reason)}
    end
  end

  defp receive_timeout do
    Exhub.Toonflow.Config.get("llm", %{}) |> Map.get("receive_timeout", 300_000)
  end
end
