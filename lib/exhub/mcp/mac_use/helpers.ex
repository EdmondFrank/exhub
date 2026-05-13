defmodule Exhub.MCP.MacUse.Helpers do
  @moduledoc """
  Shared helper functions for the Mac-use MCP server.

  Wraps the `axcli` CLI tool for macOS Accessibility API automation.
  """

  alias Anubis.Server.Response

  @axcli_bin "axcli"

  # ── axcli Execution ────────────────────────────────────────────────────

  @doc """
  Run an axcli command with the given arguments.

  Returns `{:ok, output}` or `{:error, reason}`.
  """
  def run_axcli(args, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 30_000)
    full_argv = [@axcli_bin | args]

    task =
      Task.async(fn ->
        {stdout, stderr, exit_status} =
          Exile.stream(full_argv, stderr: :consume, env: clean_env())
          |> Enum.reduce({"", "", 0}, fn
            {:stdout, data}, {out, err, code} -> {out <> data, err, code}
            {:stderr, data}, {out, err, code} -> {out, err <> data, code}
            {:exit, {:status, code}}, {out, err, _} -> {out, err, code}
            {:exit, :epipe}, {out, err, _} -> {out, err, 0}
            _, acc -> acc
          end)

        stdout = String.trim(stdout)
        stderr = String.trim(stderr)

        case exit_status do
          0 -> {:ok, stdout}
          _ ->
            detail = if stderr != "", do: stderr, else: stdout
            {:error, "axcli exited with code #{exit_status}: #{detail}"}
        end
      end)

    case Task.yield(task, timeout) do
      {:ok, result} -> result
      nil ->
        Task.shutdown(task, :brutal_kill)
        {:error, "axcli command timed out after #{timeout}ms"}
    end
  rescue
    e ->
      {:error, "Failed to run axcli: #{Exception.message(e)}"}
  end

  @doc """
  Build app targeting arguments (--app or --pid).
  """
  def app_args(%{"app" => app}), do: ["--app", app]
  def app_args(%{app: app}), do: ["--app", app]
  def app_args(%{"pid" => pid}), do: ["--pid", to_string(pid)]
  def app_args(%{pid: pid}), do: ["--pid", to_string(pid)]
  def app_args(_), do: []

  @doc """
  Build optional strategy arguments.
  """
  def strategy_args(%{"strategy" => strategy}), do: ["--strategy", strategy]
  def strategy_args(%{strategy: strategy}), do: ["--strategy", strategy]
  def strategy_args(_), do: []

  @doc """
  Build optional --no-visual-cursor flag.
  """
  def cursor_args(%{"no_visual_cursor" => true}), do: ["--no-visual-cursor"]
  def cursor_args(%{no_visual_cursor: true}), do: ["--no-visual-cursor"]
  def cursor_args(_), do: []

  # ── Response Helpers ───────────────────────────────────────────────────

  def toon_response(%Response{} = resp, data) when is_map(data) do
    encoded =
      try do
        Toon.encode!(data)
      rescue
        _ -> Jason.encode!(data)
      end

    Response.text(resp, encoded)
  end

  def toon_response(%Response{} = resp, data) when is_binary(data) do
    Response.text(resp, data)
  end

  def toon_response(%Response{} = resp, data) do
    Response.text(resp, inspect(data))
  end

  # ── Internal ───────────────────────────────────────────────────────────

  defp clean_env do
    System.get_env()
    |> Enum.reject(fn {k, _} ->
      String.starts_with?(k, "RELEASE") or
        k in ["PROGNAME", "ROOTDIR", "BINDIR"]
    end)
    |> Enum.to_list()
  end
end
