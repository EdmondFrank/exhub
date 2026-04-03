defmodule Exhub.MCP.Tools.BrowserUse.Helper do
  @moduledoc """
  Shared helpers for all BrowserUse tool modules.

  Provides `run/2` which executes a kuri-agent argv list via Exile,
  collects stdout/stderr, and returns an `Anubis.Server.Response`.

  Uses `Exile.stream/2` (not `stream!/2`) so that a non-zero exit status
  from kuri-agent does not raise — the exit status is captured as the last
  stream element `{:exit, {:status, n}}` and included in the response.
  """

  alias Anubis.Server.Response

  @doc """
  Run `kuri-agent` with the given argv list (must NOT include "kuri-agent" itself).
  Returns `{:reply, response, frame}`.
  """
  def run(argv, frame) when is_list(argv) do
    full_argv = ["kuri-agent" | argv]

    try do
      {stdout, stderr, exit_status} =
        Exile.stream(full_argv, stderr: :consume)
        |> Enum.reduce({"", "", 0}, fn
          {:stdout, data}, {out, err, code} -> {out <> data, err, code}
          {:stderr, data}, {out, err, code} -> {out, err <> data, code}
          {:exit, {:status, code}}, {out, err, _} -> {out, err, code}
          {:exit, :epipe}, {out, err, _} -> {out, err, 0}
          _, acc -> acc
        end)

      result =
        Jason.encode!(%{
          "command" => Enum.join(full_argv, " "),
          "stdout" => stdout,
          "stderr" => stderr,
          "exit_status" => exit_status
        })

      resp = Response.tool() |> Response.text(result)
      {:reply, resp, frame}
    rescue
      e ->
        resp =
          Response.tool()
          |> Response.error("Failed to run kuri-agent: #{Exception.message(e)}")

        {:reply, resp, frame}
    end
  end
end
