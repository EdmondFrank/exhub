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

    Runs the command in a shell (sh on Unix/macOS) and waits up to timeout_ms
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
    argv = ["sh", "-c", command]
    opts = build_opts(working_dir)

    try do
      task =
        Task.async(fn ->
          Exile.stream(argv, opts)
          |> Enum.reduce({"", "", 0}, fn
            {:stdout, data}, {out, err, code} -> {out <> data, err, code}
            {:stderr, data}, {out, err, code} -> {out, err <> data, code}
            {:exit, {:status, code}}, {out, err, _} -> {out, err, code}
            {:exit, :epipe}, {out, err, _} -> {out, err, 0}
            _, acc -> acc
          end)
        end)

      case Task.yield(task, timeout_ms) do
        {:ok, {stdout, stderr, exit_code}} ->
          {:ok,
           %{
             "success" => exit_code == 0,
             "command" => command,
             "stdout" => stdout,
             "stderr" => stderr,
             "exit_code" => exit_code
           }}

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

  defp build_opts(nil), do: [stderr: :consume, exit_timeout: 5000]

  defp build_opts(working_dir) do
    [stderr: :consume, exit_timeout: 5000, cd: working_dir]
  end
end
