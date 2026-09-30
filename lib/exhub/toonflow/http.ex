defmodule Exhub.Toonflow.HTTP do
  @moduledoc """
  Shared HTTP helpers for the Toonflow media clients.

  Media generation on MoArk is asynchronous: a task is submitted to an
  `.../async/...` endpoint, which returns a `task_id`, and the result is polled
  from `GET /v1/task/{task_id}`. `submit/4` and `poll/4` wrap that flow; the
  caller supplies the endpoint and the pure `interpret_poll/1` classifier
  exposed by the relevant tool module (`Exhub.MCP.Tools.VideoGen` /
  `Exhub.MCP.Tools.Speak`), so no generation logic is duplicated here.

  All requests go through `Exhub.TLSCompat` so they work behind the Exhub
  proxy/TLS shim.
  """

  alias Exhub.TLSCompat

  @api_base "https://api.moark.com/v1"
  @task_url @api_base <> "/task"
  @submit_timeout_ms 60_000
  @poll_timeout_ms 15_000
  @download_timeout_ms 120_000
  @default_interval_ms 10_000
  @default_max_attempts 55

  @doc "The MoArk API base URL."
  @spec api_base() :: String.t()
  def api_base, do: @api_base

  @doc "The async task endpoint (append `/<task_id>`)."
  @spec task_url() :: String.t()
  def task_url, do: @task_url

  @doc "The configured Gitee AI / MoArk API key."
  @spec api_key() :: {:ok, String.t()} | {:error, :missing_api_key}
  def api_key do
    case Application.get_env(:exhub, :giteeai_api_key, "") do
      "" -> {:error, :missing_api_key}
      key -> {:ok, key}
    end
  end

  @doc "POST a JSON body and decode the JSON response."
  @spec post_json(String.t(), map(), String.t(), keyword()) :: {:ok, term()} | {:error, term()}
  def post_json(url, body, key, opts \\ []) do
    request_opts =
      [
        json: body,
        headers: [
          {"Content-Type", "application/json"},
          {"Authorization", "Bearer #{key}"},
          {"X-Failover-Enabled", "true"}
        ],
        receive_timeout: Keyword.get(opts, :timeout, @submit_timeout_ms)
      ] ++ TLSCompat.req_opts()

    case Req.post(url, request_opts) do
      {:ok, %Req.Response{status: status, body: body}} when status in 200..299 -> {:ok, body}
      {:ok, %Req.Response{status: status, body: body}} -> {:error, {:http, status, body}}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc "Submit an async task and return its `task_id`."
  @spec submit(String.t(), map(), String.t(), keyword()) :: {:ok, String.t()} | {:error, term()}
  def submit(url, body, key, opts \\ []) do
    case post_json(url, body, key, opts) do
      {:ok, %{"task_id" => task_id}} when is_binary(task_id) -> {:ok, task_id}
      {:ok, decoded} -> {:error, {:no_task_id, decoded}}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Poll a task until `interpret` returns `{:ok, result}` or `{:error, reason}`.

  `interpret` receives the decoded `/v1/task/{id}` payload and returns
  `{:ok, result}`, `{:error, message}` or `:pending`. Returns
  `{:error, {:poll_timeout, task_id, attempts}}` when the attempt budget runs
  out.
  """
  @spec poll(
          String.t(),
          String.t(),
          (map() -> {:ok, map()} | {:error, term()} | :pending),
          keyword()
        ) ::
          {:ok, map()} | {:error, term()}
  def poll(task_id, key, interpret, opts \\ []) when is_function(interpret, 1) do
    do_poll(
      task_id,
      key,
      interpret,
      Keyword.get(opts, :interval_ms, @default_interval_ms),
      Keyword.get(opts, :max_attempts, @default_max_attempts),
      0
    )
  end

  @doc "GET and decode a JSON payload."
  @spec get_json(String.t(), String.t()) :: {:ok, map()} | {:error, term()}
  def get_json(url, key) do
    request_opts =
      [headers: [{"Authorization", "Bearer #{key}"}], receive_timeout: @poll_timeout_ms] ++
        TLSCompat.req_opts()

    case Req.get(url, request_opts) do
      {:ok, %Req.Response{status: 200, body: body}} -> {:ok, body}
      {:ok, %Req.Response{status: status, body: body}} -> {:error, {:http, status, body}}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc "Download a binary asset (audio/video) from a URL."
  @spec download(String.t()) :: {:ok, binary()} | {:error, term()}
  def download(url) do
    request_opts = [receive_timeout: @download_timeout_ms] ++ TLSCompat.req_opts()

    case Req.get(url, request_opts) do
      {:ok, %Req.Response{status: 200, body: body}} when is_binary(body) -> {:ok, body}
      {:ok, %Req.Response{status: status}} -> {:error, {:download, status}}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc "Write `data` to `path`, creating parent directories."
  @spec save(binary(), String.t()) :: :ok | {:error, term()}
  def save(data, path),
    do: with(:ok <- File.mkdir_p(Path.dirname(path)), do: File.write(path, data))

  # --- internals ---

  defp do_poll(task_id, _key, _interpret, _interval, max, attempt) when attempt >= max,
    do: {:error, {:poll_timeout, task_id, max}}

  defp do_poll(task_id, key, interpret, interval, max, attempt) do
    :timer.sleep(interval)

    case get_json(@task_url <> "/" <> task_id, key) do
      {:ok, result} ->
        case interpret.(result) do
          {:ok, _} = ok -> ok
          {:error, _} = error -> error
          :pending -> do_poll(task_id, key, interpret, interval, max, attempt + 1)
        end

      {:error, reason} ->
        {:error, reason}
    end
  end
end
