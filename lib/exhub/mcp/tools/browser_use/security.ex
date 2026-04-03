defmodule Exhub.MCP.Tools.BrowserUse.Security do
  @moduledoc """
  MCP Tool for browser security testing.

  Wraps kuri-agent commands: cookies, headers, audit, storage, jwt, fetch, probe.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.BrowserUse.Helper

  use Anubis.Server.Component, type: :tool

  def name, do: "browser_security"

  @impl true
  def description do
    """
    Run security tests and authenticated requests against the current browser session.

    Commands:
    - `cookies` — List all cookies with security flags ([Secure], [HttpOnly], [SameSite]).
    - `headers` — Check security response headers (CSP, HSTS, X-Frame-Options, etc.).
    - `audit`   — Full security audit: HTTPS + headers + JS-visible cookies. Returns a score and list of issues.
    - `storage` — Dump localStorage, sessionStorage, or both. Scope: local | session | all (default all).
    - `jwt`     — Scan storage and cookies for JWTs, decode and print their payloads.
    - `fetch`   — Make an authenticated HTTP request using the current session cookies and stored headers.
    - `probe`   — IDOR probe: replace `{id}` in a URL template with a range of integers and report status codes.

    All commands output JSON. For `audit` and `headers`, extract the inner value with:
      jq '.result.result.value | fromjson'
    """
  end

  schema do
    field(:command, {:required, :string},
      description: "One of: cookies, headers, audit, storage, jwt, fetch, probe"
    )

    field(:scope, :string,
      description:
        "(`storage` only) Which storage to dump: local, session, or all. Default: all."
    )

    field(:method, :string,
      description: "(`fetch` only) HTTP method: GET, POST, HEAD, etc. Default: GET."
    )

    field(:url, :string,
      description:
        "(`fetch` only) URL to request. (`probe` only) URL template with `{id}` placeholder. Example: https://api.example.com/v2/courses/{id}/assessments"
    )

    field(:data, :string,
      description:
        "(`fetch` only) JSON body for POST requests. Example: \"{\\\"score\\\":100}\""
    )

    field(:probe_start, :integer,
      description: "(`probe` only) Start of the ID range to probe (inclusive)."
    )

    field(:probe_end, :integer,
      description: "(`probe` only) End of the ID range to probe (inclusive)."
    )
  end

  @impl true
  def execute(params, frame) do
    command = Map.get(params, :command)

    case command do
      cmd when cmd in ["cookies", "headers", "audit", "jwt"] ->
        Helper.run([cmd], frame)

      "storage" ->
        argv =
          case Map.get(params, :scope) do
            nil -> ["storage"]
            scope -> ["storage", scope]
          end

        Helper.run(argv, frame)

      "fetch" ->
        method = Map.get(params, :method, "GET") |> String.upcase()
        url = Map.get(params, :url)
        data = Map.get(params, :data)

        cond do
          is_nil(url) or url == "" ->
            resp = Response.tool() |> Response.error("`url` is required for the `fetch` command")
            {:reply, resp, frame}

          true ->
            argv =
              ["fetch", method, url]
              |> then(fn a ->
                if data, do: a ++ ["--data", data], else: a
              end)

            Helper.run(argv, frame)
        end

      "probe" ->
        url = Map.get(params, :url)
        probe_start = Map.get(params, :probe_start)
        probe_end = Map.get(params, :probe_end)

        cond do
          is_nil(url) or url == "" ->
            resp = Response.tool() |> Response.error("`url` is required for the `probe` command")
            {:reply, resp, frame}

          is_nil(probe_start) ->
            resp =
              Response.tool()
              |> Response.error("`probe_start` is required for the `probe` command")

            {:reply, resp, frame}

          is_nil(probe_end) ->
            resp =
              Response.tool()
              |> Response.error("`probe_end` is required for the `probe` command")

            {:reply, resp, frame}

          true ->
            Helper.run(["probe", url, to_string(probe_start), to_string(probe_end)], frame)
        end

      _ ->
        resp =
          Response.tool()
          |> Response.error(
            "Unknown command: #{inspect(command)}. Valid: cookies, headers, audit, storage, jwt, fetch, probe"
          )

        {:reply, resp, frame}
    end
  end
end
