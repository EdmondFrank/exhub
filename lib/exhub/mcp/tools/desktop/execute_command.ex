defmodule Exhub.MCP.Tools.Desktop.ExecuteCommand do
  @moduledoc """
  MCP Tool: execute_command

  Execute a shell command and return its output.

  Uses Exile for robust process execution with streaming stdout/stderr capture.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers
  alias Exhub.MCP.Desktop.ProxyEnv
  alias Exhub.MCP.Desktop.WorkingDir

  use Anubis.Server.Component, type: :tool

  def name, do: "execute_command"

  @impl true
  def description do
    """
    Execute a shell command and return its output.

    Runs the command in a login shell (sh -l -c) and waits up to timeout_ms
    milliseconds for it to complete. Returns stdout, stderr, and the exit code.

    Uses Exile for robust process execution with separate stdout/stderr capture.

    Parameters:
    - command: The shell command to execute
    - timeout_ms: Maximum time to wait in milliseconds (default 30000)
    - working_dir: Working directory for the command. Required unless the command
      specifies its own location (an absolute or ~ path, or a cd into one) or does
      not depend on the working directory.
      (Current server pwd: #{Helpers.current_pwd()})
    """
  end

  schema do
    field(:command, :string, description: "The shell command to execute")

    field(:timeout_ms, :integer,
      description: "Maximum time to wait in milliseconds (default 30000)",
      default: 30_000
    )

    field(:working_dir, :string, description: "Working directory for the command")
  end

  @impl true
  def execute(params, frame) do
    command = Map.get(params, :command)
    timeout_ms = Map.get(params, :timeout_ms, 30_000)
    working_dir = Map.get(params, :working_dir)

    cond do
      is_nil(command) ->
        resp = Response.tool() |> Response.error("Missing required parameter: command")
        {:reply, resp, frame}

      is_nil(working_dir) and WorkingDir.needs_working_dir?(command) ->
        resp =
          Response.tool()
          |> Response.error(
            "Missing required parameter: working_dir. It must be provided unless the command specifies its own location (an absolute or ~ path, or a cd into one) or does not depend on the working directory."
          )

        {:reply, resp, frame}

      true ->
        with {:ok, working_dir} <- Helpers.validate_absolute_path(working_dir) do
          case run_with_proxy_fallback(command, timeout_ms, working_dir) do
            {:ok, result} ->
              resp =
                Response.tool()
                |> Helpers.toon_response(result)

              {:reply, resp, frame}

            {:timeout, result} ->
              resp = Response.tool() |> Response.error(timeout_message(result, timeout_ms))
              {:reply, resp, frame}

            {:error, reason} ->
              resp = Response.tool() |> Response.error("Command execution failed: #{reason}")
              {:reply, resp, frame}
          end
        else
          {:error, reason} ->
            resp = Response.tool() |> Response.error(reason)
            {:reply, resp, frame}
        end
    end
  end

  # Cap on the partial output attached to a timeout error, to keep a chatty
  # command from dumping megabytes into the LLM context. Keeps the tail, which
  # is where the hang is most likely to become visible.
  @timeout_output_limit 8_000

  defp run_command(command, timeout_ms, working_dir, env \\ nil) do
    argv = Helpers.shell_command_args(command)
    opts = build_opts(working_dir, env)
    parent = self()

    task =
      Task.async(fn ->
        Exile.stream(argv, opts)
        |> Enum.reduce({"", "", nil}, fn event, acc ->
          # Forward every raw event so the caller can reconstruct the partial
          # output if this task is killed on timeout. Sending deltas (not the
          # running accumulator) keeps mailbox growth linear in the output size.
          send(parent, {:execute_command_output, event})
          accumulate(event, acc)
        end)
      end)

    case Task.yield(task, timeout_ms) do
      {:ok, {stdout, stderr, exit_code}} ->
        flush_pending_output()
        {:ok, build_result(stdout, stderr, exit_code || 0)}

      nil ->
        # Shutdown waits for the task to go DOWN, so no further events are sent
        # past this point and the drained output is the complete captured prefix.
        Task.shutdown(task, :brutal_kill)
        {stdout, stderr, _exit_code} = flush_pending_output()
        {:timeout, build_result(stdout, stderr, nil)}

      {:exit, reason} ->
        flush_pending_output()
        {:error, "Command failed: #{inspect(reason)}"}
    end
  rescue
    e ->
      {:error, Exception.message(e)}
  end

  defp accumulate({:stdout, data}, {out, err, code}), do: {out <> data, err, code}
  defp accumulate({:stderr, data}, {out, err, code}), do: {out, err <> data, code}
  defp accumulate({:exit, {:status, code}}, {out, err, _}), do: {out, err, code}
  defp accumulate({:exit, :epipe}, {out, err, _}), do: {out, err, 0}
  defp accumulate(_event, acc), do: acc

  # Fold any events forwarded by the stream task into the accumulated output.
  # Also empties the mailbox on the success path so no messages leak.
  defp flush_pending_output, do: flush_pending_output({"", "", nil})

  defp flush_pending_output(acc) do
    receive do
      {:execute_command_output, event} -> flush_pending_output(accumulate(event, acc))
    after
      0 -> acc
    end
  end

  defp build_result(stdout, stderr, exit_code) do
    result = %{"exit_code" => exit_code}
    result = if stdout != "", do: Map.put(result, "stdout", stdout), else: result
    if stderr != "", do: Map.put(result, "stderr", stderr), else: result
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
      value when is_binary(value) and byte_size(value) > @timeout_output_limit ->
        omitted = byte_size(value) - @timeout_output_limit
        tail = binary_part(value, byte_size(value) - @timeout_output_limit, @timeout_output_limit)
        Map.put(result, key, "[... #{omitted} bytes omitted ...]\n" <> tail)

      _ ->
        result
    end
  end

  # `:on_fail` proxy fallback: run the command untouched, and only if it fails
  # the way a blocked outbound request fails, ask `ProxyEnv` whether a proxy is
  # the better fix and retry once with the variables injected into *this
  # command's* environment. Nothing is persisted, the command string is never
  # rewritten, and a command that reported success is never re-run.
  defp run_with_proxy_fallback(command, timeout_ms, working_dir) do
    first = run_command(command, timeout_ms, working_dir)

    if ProxyEnv.retry_candidate?(command, first) do
      case ProxyEnv.judge(command, first) do
        {:inject, additions, detail} ->
          env = ProxyEnv.apply_to_env(Helpers.clean_env(), additions)

          case run_command(command, timeout_ms, working_dir, env) do
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

  defp build_opts(working_dir, env) do
    base_opts = [stderr: :consume, env: env]

    if working_dir do
      Keyword.put(base_opts, :cd, working_dir)
    else
      base_opts
    end
  end
end
