defmodule Exhub.Gitee.Dashboard do
  @moduledoc """
  Gitee Enterprise dashboard payloads for `exhub-gitee-dashboard.el`.

  The dashboard used to shell out to `envless exec` + curl + jq and issue the
  nine enterprise calls one after another (~6 s of Emacs-blocking latency).
  This module performs the same calls through `GiteeCat` — with the
  `mcp-gitee-ent` user agent and the Enterprise MCP access token — and fans
  them out concurrently, so the whole payload costs about as much as its
  slowest section.

  `dashboard/1` and `detail/4` return maps whose keys and value shapes match
  what the elisp previously produced with jq, so the rendering code is
  unchanged:

    * dashboard — `enterprise_id`, `generated_at`, `enterprise`, `overview`,
      `mine`, `committers`, `projects`, `programs`, `members`, `pulls`,
      `pulls_total`, `issues`, `issues_total`
    * detail — `kind`, `ref`, `generated_at`, `detail`, `timeline`,
      `timeline_total`

  A section that fails degrades to an empty value and is reported in the
  payload's `errors` map; only a failure of the enterprise profile (dashboard)
  or of the item itself (detail) aborts the request.
  """

  alias GiteeCat.Client

  @base_url "https://api.gitee.com"
  @user_agent "mcp-gitee-ent"
  @token_key "gitee_ent_mcp_access_token"
  @timezone "Asia/Shanghai"
  @connect_timeout 5_000
  @receive_timeout 15_000
  @request_timeout 20_000
  @max_concurrency 9

  @type opts :: [
          enterprise_id: pos_integer(),
          top: pos_integer(),
          limit: pos_integer(),
          notes: pos_integer()
        ]

  @doc """
  Fetches every dashboard section concurrently.

  Options: `:enterprise_id` (default 1), `:top` (commit ranking size, default
  10) and `:limit` (pull request / issue page size, default 10).
  """
  @spec dashboard(opts()) :: {:ok, map()} | {:error, term()}
  def dashboard(opts \\ []) do
    ent_id = Keyword.get(opts, :enterprise_id, 1)
    top = Keyword.get(opts, :top, 10)
    limit = Keyword.get(opts, :limit, 10)

    with {:ok, client} <- client() do
      results =
        run_all(
          enterprise: fn -> GiteeCat.Dashboard.enterprise(client, ent_id) end,
          overview: fn -> GiteeCat.Dashboard.overview(client, ent_id) end,
          mine: fn -> GiteeCat.Dashboard.user_dashboard(client, ent_id) end,
          committers: fn -> GiteeCat.Dashboard.top_committer(client, ent_id, top) end,
          projects: fn -> GiteeCat.Dashboard.count(client, ent_id, :projects) end,
          programs: fn -> GiteeCat.Dashboard.count(client, ent_id, :programs) end,
          members: fn -> GiteeCat.Dashboard.count(client, ent_id, :members) end,
          pulls: fn ->
            GiteeCat.Dashboard.pulls(client, ent_id, scope: "related_to_me", per_page: limit)
          end,
          issues: fn ->
            GiteeCat.Dashboard.issues(client, ent_id,
              only_related_me: 1,
              state: "open",
              per_page: limit
            )
          end
        )

      case fatal(results, [:enterprise]) do
        nil -> {:ok, shape_dashboard(ent_id, results)}
        failure -> {:error, failure}
      end
    end
  end

  @doc """
  Fetches one pull request or issue together with its timeline.

  `ref` is the pull request iid or the issue ident; `subject` is the numeric
  project id of a pull request and is ignored for an issue.  Options:
  `:enterprise_id` (default 1) and `:notes` (timeline page size, default 20).
  """
  @spec detail(String.t(), String.t(), String.t() | nil, opts()) ::
          {:ok, map()} | {:error, term()}
  def detail(kind, ref, subject \\ nil, opts \\ [])

  def detail(kind, ref, subject, opts) when kind in ["pull", "issue"] do
    ent_id = Keyword.get(opts, :enterprise_id, 1)
    notes_limit = Keyword.get(opts, :notes, 20)

    with {:ok, client} <- client() do
      requests =
        case kind do
          "pull" ->
            [
              detail: fn -> GiteeCat.Dashboard.pull(client, ent_id, subject, ref) end,
              notes: fn -> GiteeCat.Dashboard.pull_timeline(client, ent_id, subject, ref) end
            ]

          "issue" ->
            [
              detail: fn -> GiteeCat.Dashboard.issue(client, ent_id, ref) end,
              notes: fn -> GiteeCat.Dashboard.issue_notes(client, ent_id, ref, notes_limit) end,
              logs: fn -> GiteeCat.Dashboard.issue_logs(client, ent_id, ref, notes_limit) end
            ]
        end

      results = run_all(requests)

      case fatal(results, [:detail]) do
        nil -> {:ok, shape_detail(kind, ref, results)}
        failure -> {:error, failure}
      end
    end
  end

  @doc """
  Builds the dashboard payload from already-fetched section results.

  `results` maps section keys to decoded bodies (or `{:error, reason}`);
  exposed for tests.
  """
  @spec shape_dashboard(pos_integer(), map()) :: map()
  def shape_dashboard(ent_id, results) do
    pulls = objects(results[:pulls])
    issues = objects(results[:issues])

    %{
      "enterprise_id" => ent_id,
      "generated_at" => timestamp(),
      "enterprise" => objects(results[:enterprise]),
      "overview" => objects(results[:overview]),
      "mine" => objects(results[:mine]),
      "committers" => list_of(results[:committers]),
      "projects" => total_count(results[:projects]),
      "programs" => total_count(results[:programs]),
      "members" => total_count(results[:members]),
      "pulls" => list_of(Map.get(pulls, "data")),
      "pulls_total" => Map.get(pulls, "total_count"),
      "issues" => list_of(Map.get(issues, "data")),
      "issues_total" => Map.get(issues, "total_count"),
      "errors" => errors(results)
    }
  end

  @doc """
  Builds a detail payload from already-fetched results (`:detail`, `:notes`,
  optionally `:logs`); exposed for tests.
  """
  @spec shape_detail(String.t(), String.t(), map()) :: map()
  def shape_detail("pull", ref, results) do
    notes = objects(results[:notes])
    timeline = list_of(Map.get(notes, "list"))

    %{
      "kind" => "pull",
      "ref" => to_string(ref),
      "generated_at" => timestamp(),
      "detail" => objects(results[:detail]),
      "timeline" => timeline,
      "timeline_total" => count_value(Map.get(notes, "count")) + event_count(timeline),
      "errors" => errors(results)
    }
  end

  def shape_detail("issue", ref, results) do
    notes = objects(results[:notes])
    logs = objects(results[:logs])
    timeline = list_of(Map.get(notes, "data")) ++ list_of(Map.get(logs, "data"))

    %{
      "kind" => "issue",
      "ref" => to_string(ref),
      "generated_at" => timestamp(),
      "detail" => objects(results[:detail]),
      "timeline" => timeline,
      "timeline_total" =>
        count_value(Map.get(notes, "total_count")) +
          count_value(Map.get(logs, "total_count")),
      "errors" => errors(results)
    }
  end

  @doc "Human-readable description of a `dashboard/1` or `detail/4` error."
  @spec error_message(term()) :: String.t()
  def error_message(:missing_token) do
    "GITEE_ENT_MCP_ACCESS_TOKEN is not configured: store `#{@token_key}` in " <>
      "ExHub's SecretVault and run exhub_reload_keys"
  end

  def error_message({:section, key, reason}), do: "#{key}: #{error_message(reason)}"
  def error_message({:http, status, message}), do: "Gitee API returned HTTP #{status}: #{message}"
  def error_message({:transport, message}), do: "cannot reach the Gitee API: #{message}"
  def error_message({:task, reason}), do: "request crashed: #{inspect(reason)}"
  def error_message(other), do: inspect(other)

  # ── Request plumbing ──────────────────────────────────────────────────────

  defp client do
    case token() do
      nil ->
        {:error, :missing_token}

      token ->
        base = base_url()

        {:ok,
         Client.new(%{access_token: token}, base,
           user_agent: @user_agent,
           request_options:
             Exhub.TLSCompat.httpoison_opts(base) ++
               [timeout: @connect_timeout, recv_timeout: @receive_timeout]
         )}
    end
  end

  # `Application.load_secrets/0` copies the whole vault into `:persistent_term`
  # at boot and `exhub_reload_keys` refreshes it, so a rotated token is picked
  # up without a restart. The app-env fallback covers a config-file setup.
  defp token do
    [
      :persistent_term.get(@token_key, nil),
      Application.get_env(:exhub, :gitee_ent_mcp_access_token)
    ]
    |> Enum.find_value(fn
      value when is_binary(value) ->
        case String.trim(value) do
          "" -> nil
          trimmed -> trimmed
        end

      _ ->
        nil
    end)
  end

  defp base_url, do: Application.get_env(:exhub, :gitee_dashboard_base_url, @base_url)

  defp run_all(requests) do
    requests
    |> Task.async_stream(
      fn {_key, fun} -> request(fun) end,
      max_concurrency: @max_concurrency,
      ordered: true,
      timeout: @request_timeout,
      on_timeout: :kill_task
    )
    |> Enum.zip(requests)
    |> Map.new(fn
      {{:ok, {:ok, body}}, {key, _fun}} -> {key, body}
      {{:ok, {:error, reason}}, {key, _fun}} -> {key, {:error, reason}}
      {{:exit, reason}, {key, _fun}} -> {key, {:error, {:task, reason}}}
    end)
  end

  defp request(fun) do
    case fun.() do
      {status, body, _resp} when status in 200..299 -> {:ok, body}
      {status, body, _resp} -> {:error, {:http, status, message(body)}}
      {:error, reason} -> {:error, reason}
    end
  rescue
    error in [HTTPoison.Error] -> {:error, {:transport, Exception.message(error)}}
  end

  defp message(body) when is_map(body) do
    Enum.find_value(["message", "error_description", "error"], fn key ->
      case Map.get(body, key) do
        text when is_binary(text) -> text
        _ -> nil
      end
    end) || inspect(body, limit: 5, printable_limit: 200)
  end

  defp message(body) when is_binary(body), do: body |> String.trim() |> String.slice(0, 200)
  defp message(_body), do: "unexpected response"

  defp fatal(results, keys) do
    Enum.find_value(keys, fn key ->
      case results[key] do
        {:error, reason} -> {:section, key, reason}
        _ -> nil
      end
    end)
  end

  # ── Shaping helpers ───────────────────────────────────────────────────────

  defp objects(value) when is_map(value), do: value
  defp objects(_value), do: %{}

  defp list_of(value) when is_list(value), do: value
  defp list_of(_value), do: []

  defp total_count(value) do
    case objects(value) do
      %{"total_count" => count} -> count_value(count)
      _ -> nil
    end
  end

  defp count_value(value) when is_integer(value), do: value
  defp count_value(value) when is_float(value), do: trunc(value)
  defp count_value(_value), do: 0

  defp event_count(timeline) do
    Enum.count(timeline, fn entry ->
      is_map(entry) and Map.get(entry, "type") == "operate_log"
    end)
  end

  defp errors(results) do
    for {key, {:error, reason}} <- results, into: %{} do
      {to_string(key), error_message(reason)}
    end
  end

  defp timestamp do
    case DateTime.now(@timezone) do
      {:ok, datetime} -> Calendar.strftime(datetime, "%Y-%m-%d %H:%M:%S")
      _ -> Calendar.strftime(DateTime.utc_now(), "%Y-%m-%d %H:%M:%S")
    end
  end
end
