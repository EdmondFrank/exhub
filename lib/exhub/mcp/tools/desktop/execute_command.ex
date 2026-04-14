defmodule Exhub.MCP.Tools.Desktop.ExecuteCommand do
  @moduledoc """
  MCP Tool: execute_command

  Execute a shell command and return its output.

  Uses Exile for robust process execution with streaming stdout/stderr capture.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers

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
    - working_dir: Working directory for the command (optional)
    """
  end

  schema do
    field(:command, {:required, :string}, description: "The shell command to execute")
    field(:timeout_ms, :integer, description: "Maximum time to wait in milliseconds (default 30000)", default: 30_000)
    field(:working_dir, :string, description: "Working directory for the command (optional)")
  end

  @impl true
  def execute(params, frame) do
    command = Map.get(params, :command)
    timeout_ms = Map.get(params, :timeout_ms, 30_000)
    working_dir = Map.get(params, :working_dir) |> Helpers.expand_path()

    case run_command(command, timeout_ms, working_dir) do
      {:ok, result} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(result)

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Command execution failed: #{reason}")
        {:reply, resp, frame}
    end
  end

  defp run_command(command, timeout_ms, working_dir) do
    argv = Helpers.shell_command_args(command)
    opts = build_opts(working_dir)

    try do
      task =
        Task.async(fn ->
          Exile.stream(argv, opts)
          |> Enum.reduce({"", "", nil}, fn
            {:stdout, data}, {out, err, code} -> {out <> data, err, code}
            {:stderr, data}, {out, err, code} -> {out, err <> data, code}
            {:exit, {:status, code}}, {out, err, _} -> {out, err, code}
            {:exit, :epipe}, {out, err, _} -> {out, err, 0}
            _, acc -> acc
          end)
        end)

      case Task.yield(task, timeout_ms) do
        {:ok, {stdout, stderr, exit_code}} ->
          # Default exit_code to 0 if stream ended without explicit exit
          exit_code = exit_code || 0

          result = %{"exit_code" => exit_code}
          result = if stdout != "", do: Map.put(result, "stdout", stdout), else: result
          result = if stderr != "", do: Map.put(result, "stderr", stderr), else: result

          {:ok, result}

        nil ->
          Task.shutdown(task, :brutal_kill)
          {:error, "Command timed out after #{timeout_ms}ms"}

        {:exit, reason} ->
          {:error, "Command failed: #{inspect(reason)}"}
      end
    rescue
      e ->
        {:error, Exception.message(e)}
    end
  end

  defp build_opts(working_dir) do
    base_opts = [stderr: :consume, env: Helpers.clean_env()]

    if working_dir do
      Keyword.put(base_opts, :cd, working_dir)
    else
      base_opts
    end
  end
end
