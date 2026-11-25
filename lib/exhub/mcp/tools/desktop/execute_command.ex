defmodule Exhub.MCP.Tools.Desktop.ExecuteCommand do
  @moduledoc """
  MCP Tool: execute_command

  Runs a shell command in a login shell and returns its output. The command is
  always a tracked process (`Exhub.MCP.Desktop.ProcessStore`), so a command that
  outlives the call can still be read, fed input, and killed.

  Three modes, chosen by `interactive` and `async`:

    * `async: true` (default) — wait up to `wait_ms` (default 5000 ms). A command
      that finishes in that window returns its `stdout`, `stderr` and
      `exit_code` directly; one that does not returns a `process_id` with
      `status: running` plus the output captured so far, to be followed with
      `read_process_output`.
    * `async: false` — block up to `timeout_ms` (default 30000 ms) and kill the
      command at the timeout, returning the output captured before termination.
      `timeout_ms` is only consulted in this mode.
    * `interactive: true` — keep stdin open for `interact_with_process` and
      answer immediately with the `process_id` (a PTY is the default backend).

  Mode selection ignores `timeout_ms`: honouring it only in blocking mode keeps
  the async default (and its `wait_ms` wait) in force for callers that merely
  pass a timeout — notably Lua `code_mode`, whose own wall clock is the outer
  bound.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers
  alias Exhub.MCP.Desktop.ProcessLauncher
  alias Exhub.MCP.Desktop.ProcessStore
  alias Exhub.MCP.Desktop.ProxyEnv
  alias Exhub.MCP.Desktop.WorkingDir

  use Anubis.Server.Component, type: :tool

  def name, do: "execute_command"

  @impl true
  def description do
    """
    Run a shell command in a login shell (`sh -l -c`) as a tracked process.

    Default (`async: true`) waits up to `wait_ms` (5000 ms). A command that
    finishes returns `stdout`, `stderr` and `exit_code`; one that does not
    returns a `process_id` with `status: running` plus the output captured so
    far — follow it with `read_process_output`, feed it with
    `interact_with_process`, stop it with `kill_process`.

    `async: false` blocks up to `timeout_ms` and kills the command there.
    `interactive: true` keeps stdin open and answers immediately with the id.

    Processes are cleaned up after 1 hour of inactivity.
    """
  end

  schema do
    field(:command, :string, description: "The shell command to execute")

    field(:working_dir, :string, description: "Working directory for the command")

    field(:async, :boolean,
      description:
        "Wait only `wait_ms` then reply with a running process_id (default true). false = block until `timeout_ms`."
    )

    field(:wait_ms, :integer,
      description: "How long to wait before replying with a running process_id (default 5000)",
      default: 5_000
    )

    field(:timeout_ms, :integer,
      description:
        "Blocking-mode budget; the command is killed at it (default 30000). Ignored unless `async: false`."
    )

    field(:interactive, :boolean,
      default: false,
      description: "Keep stdin open for interact_with_process"
    )

    field(:pty, :boolean,
      description: "PTY backend for ssh/TTY programs (requires interactive: true)"
    )
  end

  # Cap on the output attached to a timeout / still-running reply, to keep a
  # chatty command from dumping megabytes into the LLM context. Keeps the tail,
  # which is where the hang is most likely to become visible.
  @output_limit 8_000

  @impl true
  def execute(params, frame) do
    command = Map.get(params, :command)
    # A client that knows its repository root (AiderDesk sends `x-project-dir`)
    # gets it as the default instead of paying a round trip to be told off for
    # an omitted `working_dir`. Without the header nothing changes.
    working_dir = Map.get(params, :working_dir) |> Helpers.with_request_cwd(frame)
    interactive = Map.get(params, :interactive, false)
    # When interactive is true, default pty to true unless explicitly set
    pty = if is_nil(Map.get(params, :pty)), do: interactive, else: Map.get(params, :pty)

    cond do
      is_nil(command) ->
        reply_error(frame, "Missing required parameter: command")

      is_nil(working_dir) and WorkingDir.needs_working_dir?(command) ->
        reply_error(
          frame,
          "Missing required parameter: working_dir. It must be provided unless the command specifies its own location (an absolute or ~ path, or a cd into one) or does not depend on the working directory."
        )

      pty == true and interactive != true ->
        reply_error(
          frame,
          "pty: true requires interactive: true. Set both parameters to enable PTY mode."
        )

      true ->
        with {:ok, working_dir} <- Helpers.validate_absolute_path(working_dir) do
          run(mode(params, interactive), command, working_dir, interactive, pty, params, frame)
        else
          {:error, reason} -> reply_error(frame, reason)
        end
    end
  end

  # `interactive: true` always answers immediately; only an explicit
  # `async: false` blocks. `timeout_ms` deliberately does NOT select blocking
  # mode — it is merely the blocking-mode budget — so the async default (and its
  # `wait_ms` window) survives callers that pass a timeout, e.g. Lua code_mode
  # scripts whose own wall clock is the outer bound.
  defp mode(_params, true), do: :interactive

  defp mode(params, _interactive) do
    if Map.get(params, :async) == false, do: :blocking, else: :async
  end

  defp run(:interactive, command, working_dir, _interactive, pty, _params, frame) do
    process_id = ProcessLauncher.new_process_id()

    case ProcessLauncher.start(process_id, command, working_dir, interactive: true, pty: pty) do
      {:ok, entry} ->
        payload =
          Helpers.put_cwd(
            %{
              "process_id" => process_id,
              "pid" => entry.pid,
              "status" => "running",
              "interactive" => entry.interactive,
              "pty" => entry.backend == :erlexec,
              "backend" => to_string(entry.backend),
              "hint" =>
                "Interactive process started. Send input with interact_with_process(process_id: ...) and read output with read_process_output."
            },
            working_dir
          )

        {:reply, Helpers.toon_response(Response.tool(), payload), frame}

      {:error, reason} ->
        reply_error(frame, "Failed to start process: #{reason}")
    end
  end

  defp run(:async, command, working_dir, _interactive, _pty, params, frame) do
    run_async(command, working_dir, wait_ms(params), frame)
  end

  defp run(:blocking, command, working_dir, _interactive, _pty, params, frame) do
    run_blocking(command, working_dir, timeout_ms(params), frame)
  end

  defp run_async(command, working_dir, wait_ms, frame) do
    process_id = ProcessLauncher.new_process_id()
    ref = make_ref()
    deadline = now_ms() + wait_ms

    case ProcessLauncher.start(process_id, command, working_dir, notify: self(), ref: ref) do
      {:ok, entry} ->
        case collect(ref, new_acc(), deadline) do
          {:done, acc} ->
            # The full result is in hand; do not leave a finished entry behind
            # (only still-running processes need to be readable by id).
            ProcessStore.remove(process_id)

            payload = acc |> result_payload() |> Helpers.put_cwd(working_dir)
            {:reply, Helpers.toon_response(Response.tool(), payload), frame}

          {:error, reason} ->
            ProcessStore.remove(process_id)
            reply_error(frame, "Command execution failed: #{reason}")

          {:running, acc} ->
            # The consumer fills in the pid after the launcher's handshake, so
            # prefer the current entry over the one captured at start.
            entry = ProcessStore.get(process_id) || entry

            payload =
              %{
                "process_id" => process_id,
                "pid" => entry.pid,
                "status" => "running",
                "waited_ms" => wait_ms,
                "hint" =>
                  "Command still running after #{wait_ms}ms. Read further output with read_process_output(process_id: ...); send input with interact_with_process; stop it with kill_process."
              }
              |> put_stream("stdout", acc.stdout)
              |> put_stream("stderr", acc.stderr)
              |> cap_output()
              |> Helpers.put_cwd(working_dir)

            {:reply, Helpers.toon_response(Response.tool(), payload), frame}
        end

      {:error, reason} ->
        reply_error(frame, "Failed to start process: #{reason}")
    end
  end

  defp run_blocking(command, working_dir, timeout_ms, frame) do
    case run_with_proxy_fallback(command, working_dir, timeout_ms) do
      {:ok, result} ->
        payload = result |> Helpers.put_cwd(working_dir)
        {:reply, Helpers.toon_response(Response.tool(), payload), frame}

      {:timeout, result} ->
        reply_error(frame, timeout_message(result, timeout_ms))

      {:error, reason} ->
        reply_error(frame, "Command execution failed: #{reason}")
    end
  end

  # ============================================================================
  # Waiting for a managed process
  # ============================================================================

  defp new_acc, do: %{stdout: "", stderr: "", exit_code: nil}

  # Collects the events forwarded by the launcher's stream consumer until the
  # child exits or `deadline` passes. `receive ... after 0` flushes whatever is
  # already in the mailbox, so a command that just finished is never reported as
  # still running.
  defp collect(ref, acc, deadline) do
    remaining = max(deadline - now_ms(), 0)

    receive do
      {:exec_event, ^ref, {:stdout, data}} ->
        collect(ref, %{acc | stdout: acc.stdout <> data}, deadline)

      {:exec_event, ^ref, {:stderr, data}} ->
        collect(ref, %{acc | stderr: acc.stderr <> data}, deadline)

      {:exec_event, ^ref, {:exit, code}} ->
        {:done, %{acc | exit_code: code}}

      {:exec_event, ^ref, {:error, reason}} ->
        {:error, reason}
    after
      remaining -> {:running, acc}
    end
  end

  defp result_payload(acc) do
    %{"exit_code" => acc.exit_code}
    |> put_stream("stdout", acc.stdout)
    |> put_stream("stderr", acc.stderr)
  end

  defp put_stream(payload, _key, ""), do: payload
  defp put_stream(payload, key, data), do: Map.put(payload, key, data)

  defp now_ms, do: System.monotonic_time(:millisecond)

  defp wait_ms(params), do: Map.get(params, :wait_ms) || 5_000

  defp timeout_ms(params), do: Map.get(params, :timeout_ms) || 30_000

  # ============================================================================
  # Blocking mode
  # ============================================================================

  # Runs the command to completion (or the timeout), killing it on timeout. The
  # process is removed from the store either way: the caller gets the output
  # inline, and only still-running processes need to stay addressable.
  defp run_managed(command, working_dir, timeout_ms, env) do
    process_id = ProcessLauncher.new_process_id()
    ref = make_ref()
    deadline = now_ms() + timeout_ms

    case ProcessLauncher.start(process_id, command, working_dir,
           notify: self(),
           ref: ref,
           env: env
         ) do
      {:ok, _entry} ->
        case collect(ref, new_acc(), deadline) do
          {:done, acc} ->
            ProcessStore.remove(process_id)
            {:ok, result_payload(acc)}

          {:error, reason} ->
            ProcessStore.remove(process_id)
            {:error, reason}

          {:running, acc} ->
            # Shutdown waits for the child to go DOWN before the store entry is
            # dropped, so no output is lost past this point.
            _ = ProcessStore.kill_process(process_id)
            ProcessStore.remove(process_id)
            {:timeout, result_payload(acc)}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  # `:on_fail` proxy fallback: run the command untouched, and only if it fails
  # the way a blocked outbound request fails, ask `ProxyEnv` whether a proxy is
  # the better fix and retry once with the variables injected into *this
  # command's* environment. Nothing is persisted, the command string is never
  # rewritten, and a command that reported success is never re-run.
  #
  # The streaming (default) mode cannot do this — it answers before a failure is
  # observable — so it reuses a verdict recorded here for the same command.
  defp run_with_proxy_fallback(command, working_dir, timeout_ms) do
    first = run_managed(command, working_dir, timeout_ms, nil)

    if ProxyEnv.retry_candidate?(command, first) do
      case ProxyEnv.judge(command, first) do
        {:inject, additions, detail} ->
          env = ProxyEnv.apply_to_env(Helpers.clean_env(), additions)

          case run_managed(command, working_dir, timeout_ms, env) do
            {:ok, retried} -> {:ok, annotate_proxy(retried, additions, detail, nil)}
            {:timeout, retried} -> {:timeout, annotate_proxy(retried, additions, detail, nil)}
            # The retry could not run: report the original failure, annotated.
            {:error, reason} -> tag_with_note(first, additions, detail, inspect(reason))
          end

        {:noop, _reason, _detail} ->
          first
      end
    else
      first
    end
  end

  defp tag_with_note({tag, payload}, additions, detail, retry_error) do
    {tag, annotate_proxy(payload, additions, detail, retry_error)}
  end

  defp tag_with_note(other, _additions, _detail, _retry_error), do: other

  # Surfaces what was injected and why, so the caller can see the retry instead
  # of receiving an unexplained second run of the same command.
  defp annotate_proxy(payload, additions, detail, retry_error) when is_map(payload) do
    note = %{
      "proxy_url" => Map.get(detail, "proxy_url"),
      "injected" => additions |> Enum.map(fn {key, _} -> key end) |> Enum.uniq(),
      "needs_proxy" => Map.get(detail, "needs_proxy"),
      "mechanism" => Map.get(detail, "mechanism"),
      "leak_risk" => Map.get(detail, "leak_risk"),
      "attempt" => 2
    }

    note = if retry_error, do: Map.put(note, "retry_error", retry_error), else: note
    Map.put(payload, "proxy_env", note)
  end

  defp annotate_proxy(payload, _additions, _detail, _retry_error), do: payload

  # ============================================================================
  # Responses
  # ============================================================================

  defp reply_error(frame, message) do
    {:reply, Response.tool() |> Response.error(message), frame}
  end

  defp timeout_message(result, timeout_ms) do
    payload =
      result
      |> Map.put("timed_out", true)
      |> Map.put("timeout_ms", timeout_ms)
      |> Map.put(
        "hint",
        "Command was killed at the timeout; stdout/stderr show output captured before termination."
      )
      |> cap_output()

    "Command timed out after #{timeout_ms}ms. Partial output captured before termination:\n" <>
      Helpers.toon_encode(payload)
  end

  defp cap_output(result) do
    result
    |> cap_stream("stdout")
    |> cap_stream("stderr")
  end

  defp cap_stream(result, key) do
    case Map.get(result, key) do
      value when is_binary(value) and byte_size(value) > @output_limit ->
        omitted = byte_size(value) - @output_limit
        tail = binary_part(value, byte_size(value) - @output_limit, @output_limit)
        Map.put(result, key, "[... #{omitted} bytes omitted ...]\n" <> tail)

      _ ->
        result
    end
  end
end
