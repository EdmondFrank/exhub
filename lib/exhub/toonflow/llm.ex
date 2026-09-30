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
end

defmodule Exhub.Toonflow.LLM.Default do
  @moduledoc "Default `Exhub.Toonflow.LLM` implementation — delegates to GenClaw's helper."

  @behaviour Exhub.Toonflow.LLM

  @impl true
  def call_llm(system, user, opts) do
    case Exhub.Genclaw.LLMHelper.call_llm(system, user, opts) do
      {:ok, text} -> {:ok, text}
      {:error, reason, _model} -> {:error, reason}
    end
  end
end
