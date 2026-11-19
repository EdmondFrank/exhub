defmodule Exhub.HTTP.Pools do
  @moduledoc """
  Connection-pool sizing for Exhub's outbound HTTP.

  Exhub funnels every outbound HTTP call through hackney (via `HTTPoison`).
  Unless told otherwise, all of them share hackney's single `:default` pool,
  whose cap is **50 connections**. That one budget is shared by:

    * the streaming LLM proxy (`Exhub.ProxyPlug`), where a request holds a
      connection for the whole completion — with the contexts ExHub actually
      serves (p50 ~80k, p90 ~200k, max ~2M tokens) that is seconds to minutes;
    * every short-lived call: `web_fetch`, image/TTS generation, Gitee, brain
      RAG and, on **every** desktop tool call, the Smart Decide gates
      (`CostGuard`, `SecretGuard`, `ProxyEnv`, `WorkingDir`).

  A handful of long proxy streams could therefore starve the gates, and each
  gated tool call then waits for a connection: nothing fails, nothing is
  logged, ExHub accepts TCP but answers nothing. That is the "ExHub stopped
  responding after a long run" symptom.

  Proxy traffic now gets its own pool (`:exhub_proxy`, 200 connections) and the
  shared pool is widened, so tool traffic can never queue behind streams.

  Called once from `Exhub.Application.start/2`. Both operations are idempotent,
  so it is also safe to run on a live VM after a hot reload.
  """

  require Logger

  @proxy_pool :exhub_proxy
  @proxy_max_connections 200
  @shared_max_connections 200
  @idle_timeout_ms 150_000

  @doc """
  Creates the dedicated proxy pool and widens the shared `:default` pool.

  Returns `:ok` even when hackney is unavailable (the caller is a supervision
  tree boot path — a pool-sizing warning must not take the app down).
  """
  @spec setup() :: :ok
  def setup do
    ensure_pool(:default, [])
    resize(:default, @shared_max_connections, @idle_timeout_ms)

    # hackney auto-creates a pool on first use from whatever options the
    # request carries (`pool: :exhub_proxy` and nothing else) — which yields
    # the 50-connection default. So never rely on `start_pool/2` alone: always
    # resize afterwards, which is also what makes this callable on a live VM.
    ensure_pool(@proxy_pool, max_connections: @proxy_max_connections, timeout: @idle_timeout_ms)
    resize(@proxy_pool, @proxy_max_connections, @idle_timeout_ms)
    :ok
  end

  @doc "Name of the hackney pool reserved for streaming proxy traffic."
  @spec proxy_pool() :: atom()
  def proxy_pool, do: @proxy_pool

  @doc "Configured max connections for a known pool (introspection helper)."
  def max_connections(pool) do
    :hackney_pool.max_connections(pool)
  catch
    _, _ -> nil
  end

  defp ensure_pool(name, opts) do
    case :hackney_pool.start_pool(name, opts) do
      :ok -> :ok
      other -> Logger.warning("[HTTP.Pools] could not start pool #{inspect(name)}: #{inspect(other)}")
    end
  catch
    kind, reason ->
      Logger.warning(
        "[HTTP.Pools] hackney unavailable for pool #{inspect(name)}: #{inspect({kind, reason})}"
      )
  end

  defp resize(name, max, idle_timeout_ms) do
    set_max_connections(name, max)

    :hackney_pool.set_timeout(name, idle_timeout_ms)
  catch
    kind, reason ->
      Logger.warning("[HTTP.Pools] could not resize pool #{inspect(name)}: #{inspect({kind, reason})}")
  end

  defp set_max_connections(name, max) do
    :hackney_pool.set_max_connections(name, max)
  catch
    kind, reason ->
      Logger.warning(
        "[HTTP.Pools] could not size pool #{inspect(name)}: #{inspect({kind, reason})}"
      )
  end
end