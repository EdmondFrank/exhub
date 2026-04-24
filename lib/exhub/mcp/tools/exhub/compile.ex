defmodule Exhub.MCP.Tools.Exhub.Compile do
  @moduledoc """
  MCP Tool: exhub_compile

  Runs `mix compile` in the Exhub project directory.

  Only available in development or when Mix is loaded.  Returns an error
  when running from a release build where Mix is not present.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "exhub_compile"

  @impl true
  def description do
    """
    Compile the Exhub project using `mix compile`.

    Only works when the server is running from the source tree (Mix is
    available).  Returns an error in release builds.

    Parameters:
    - env: Build environment to use, e.g. "dev" or "prod" (default "dev")
    - working_dir: Project root directory (defaults to current project)
    """
  end

  schema do
    field(:env, :string, description: "Build environment: dev, test, or prod", default: "dev")
    field(:working_dir, :string, description: "Project root directory (defaults to current project)")
    field(:timeout_ms, :integer, description: "Compilation timeout in milliseconds (default 120_000)", default: 120_000)
  end

  @impl true
  def execute(params, frame) do
    env = Map.get(params, :env, "dev")
    working_dir = Map.get(params, :working_dir)
    timeout_ms = Map.get(params, :timeout_ms, 120_000)

    unless mix_available?() do
      resp =
        Response.tool()
        |> Response.error(
          "Mix is not available. Compilation is only supported when running from the source tree, not from a release build."
        )

      {:reply, resp, frame}
    else
      working_dir =
        if working_dir do
          Helpers.expand_path(working_dir)
        else
          # Try to locate the project root by looking for mix.exs
          find_project_root()
        end

      case validate_project_dir(working_dir) do
        {:error, reason} ->
          resp = Response.tool() |> Response.error(reason)
          {:reply, resp, frame}

        :ok ->
          command = "MIX_ENV=#{env} mix compile"
          argv = Helpers.shell_command_args(command, login: true)
          opts = [stderr: :consume, cd: working_dir]

          task =
            Task.async(fn ->
              try do
                Exile.stream(argv, opts)
                |> Enum.reduce({"", "", nil}, fn
                  {:stdout, data}, {out, err, code} -> {out <> data, err, code}
                  {:stderr, data}, {out, err, code} -> {out, err <> data, code}
                  {:exit, {:status, code}}, {out, err, _} -> {out, err, code}
                  {:exit, :epipe}, {out, err, _} -> {out, err, 0}
                  _, acc -> acc
                end)
              rescue
                e -> {:error, Exception.message(e)}
              end
            end)

          case Task.yield(task, timeout_ms) || Task.shutdown(task, :brutal_kill) do
            {:ok, {:error, message}} ->
              resp = Response.tool() |> Response.error("Compilation failed: #{message}")
              {:reply, resp, frame}

            {:ok, {stdout, stderr, exit_code}} ->
              result = %{
                "exit_code" => exit_code || 0,
                "stdout" => String.trim(stdout),
                "stderr" => String.trim(stderr)
              }

              resp = Response.tool() |> Helpers.toon_response(result)
              {:reply, resp, frame}

            nil ->
              resp =
                Response.tool()
                |> Response.error("Compilation timed out after #{timeout_ms}ms.")

              {:reply, resp, frame}
          end
      end
    end
  end

  defp mix_available? do
    Code.ensure_loaded?(Mix)
  end

  defp validate_project_dir(nil) do
    {:error, "Could not locate a project root (no mix.exs found)."}
  end

  defp validate_project_dir(dir) do
    cond do
      not File.dir?(dir) ->
        {:error, "Directory does not exist: #{dir}"}

      not File.exists?(Path.join(dir, "mix.exs")) ->
        {:error, "Not a Mix project (mix.exs not found in #{dir})."}

      true ->
        :ok
    end
  end

  defp find_project_root do
    # Walk up from the current working directory looking for mix.exs
    cwd = File.cwd!()

    find_up(cwd, "mix.exs") || cwd
  end

  defp find_up(dir, filename) do
    candidate = Path.join(dir, filename)

    if File.exists?(candidate) do
      dir
    else
      parent = Path.dirname(dir)

      if parent == dir do
        nil
      else
        find_up(parent, filename)
      end
    end
  end
end
