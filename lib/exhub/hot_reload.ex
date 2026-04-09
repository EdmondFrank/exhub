defmodule Exhub.HotReload do
  @moduledoc """
  BEAM hot-reload for the Exhub application.

  Uses Erlang's `:code.load_file/1` to atomically swap every module that
  belongs to the `:exhub` OTP application.  The running VM is never
  restarted, so:

  * In-flight HTTP requests (each runs in its own Cowboy process) finish
    with the old code — no connection is dropped.
  * New incoming requests immediately use the new code.
  * GenServers continue with old code until the next message dispatch,
    at which point the new module version is used automatically.

  Because modules are discovered by scanning the ebin directory on disk
  (rather than reading the in-memory application spec), **new modules
  added since the last start are also loaded** — not just existing ones
  hot-swapped.

  ## Typical workflow

  1. Build a new release:  `mix release --overwrite`
  2. Trigger reload via the release RPC:
       `bin/exhub rpc "Exhub.HotReload.reload()"`
     or via the Emacs `exhub-reload` command, or via `POST /system/reload`.
  """

  require Logger

  @doc """
  Hot-reload all modules belonging to the `:exhub` OTP application.

  Returns `%{ok: non_neg_integer(), errors: non_neg_integer()}`.
  """
  @spec reload() :: %{ok: non_neg_integer(), errors: non_neg_integer()}
  def reload do
    # Locate the ebin directory by finding the .app file for :exhub in the
    # code path — avoids the deprecated :code.lib_dir/2.
    ebin =
      :code.where_is_file(~c"exhub.app")
      |> to_string()
      |> Path.dirname()

    modules =
      File.ls!(ebin)
      |> Enum.filter(&String.ends_with?(&1, ".beam"))
      |> Enum.map(&String.replace_suffix(&1, ".beam", ""))
      |> Enum.map(&String.to_atom/1)

    Logger.info("[HotReload] Reloading #{length(modules)} modules from #{ebin}")

    results =
      Enum.map(modules, fn mod ->
        # Purge the old "old" code slot first so we don't accumulate stale code.
        :code.soft_purge(mod)

        case :code.load_file(mod) do
          {:module, ^mod} ->
            Logger.debug("[HotReload] ✓ #{mod}")
            :ok

          {:error, reason} ->
            Logger.warning("[HotReload] ✗ #{mod}: #{inspect(reason)}")
            :error
        end
      end)

    ok_count = Enum.count(results, &(&1 == :ok))
    err_count = Enum.count(results, &(&1 == :error))

    Logger.info("[HotReload] Done — #{ok_count} reloaded, #{err_count} errors")

    %{ok: ok_count, errors: err_count}
  end

  @doc """
  Returns a human-readable summary string suitable for sending back to Emacs.
  """
  @spec reload_and_summarize() :: String.t()
  def reload_and_summarize do
    %{ok: ok, errors: errors} = reload()

    if errors == 0 do
      "[Exhub] Hot reload complete: #{ok} modules reloaded successfully."
    else
      "[Exhub] Hot reload finished with issues: #{ok} ok, #{errors} errors (check server log)."
    end
  end
end
