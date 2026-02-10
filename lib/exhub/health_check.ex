defmodule Exhub.HealthCheck do
  @moduledoc """
  Health Check module using Quantum scheduler to monitor target URLs
  and send webhook notifications on status changes.

  Supports multiple webhook providers:
  - :default - Generic JSON webhook
  - :feishu - Feishu/Lark bot webhook format
  """

  use Quantum, otp_app: :exhub

  require Logger

  @type webhook_provider :: :default | :feishu

  @doc """
  Performs a health check on a target URL.
  Returns {:ok, status_code} on success, {:error, reason} on failure.
  """
  def check_target_url(url, opts \\ []) do
    method = Keyword.get(opts, :method, :get)
    headers = Keyword.get(opts, :headers, [])
    timeout = Keyword.get(opts, :timeout, 30_000)
    expected_status = Keyword.get(opts, :expected_status, 200)

    Logger.info("Checking health for: #{url}")

    case make_request(method, url, headers, timeout) do
      {:ok, %{status: status}} when status == expected_status ->
        Logger.info("Health check passed for #{url} (status: #{status})")
        {:ok, status}

      {:ok, %{status: status}} ->
        Logger.warning("Health check returned unexpected status for #{url}: #{status}")
        {:error, {:unexpected_status, status}}

      {:error, reason} ->
        Logger.error("Health check failed for #{url}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp make_request(:get, url, headers, timeout) do
    case HTTPoison.get(url, headers, recv_timeout: timeout) do
      {:ok, %HTTPoison.Response{status_code: status, body: body, headers: resp_headers}} ->
        {:ok, %{status: status, body: body, headers: resp_headers}}

      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, reason}
    end
  end

  defp make_request(:head, url, headers, timeout) do
    case HTTPoison.head(url, headers, recv_timeout: timeout) do
      {:ok, %HTTPoison.Response{status_code: status, body: body, headers: resp_headers}} ->
        {:ok, %{status: status, body: body, headers: resp_headers}}

      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, reason}
    end
  end

  defp make_request(method, url, headers, timeout) do
    Logger.warning("Unsupported HTTP method: #{method}, falling back to GET")
    make_request(:get, url, headers, timeout)
  end

  @doc """
  Sends a webhook notification.
  Returns :ok on success, {:error, reason} on failure.

  ## Options
    - :headers - Custom HTTP headers (default: [{"Content-Type", "application/json"}])
    - :timeout - Request timeout in ms (default: 30_000)
    - :provider - Webhook provider (:default or :feishu) (default: :default)
  """
  def send_webhook(webhook_url, payload, opts \\ []) do
    provider = Keyword.get(opts, :provider, :default)
    headers = Keyword.get(opts, :headers, [{"Content-Type", "application/json"}])
    timeout = Keyword.get(opts, :timeout, 30_000)

    formatted_payload = format_payload(payload, provider)
    json_payload = Jason.encode!(formatted_payload)

    Logger.info("Sending webhook to: #{webhook_url} (provider: #{provider})")

    case HTTPoison.post(webhook_url, json_payload, headers, recv_timeout: timeout) do
      {:ok, %HTTPoison.Response{status_code: status}} when status in 200..299 ->
        Logger.info("Webhook sent successfully to #{webhook_url} (status: #{status})")
        :ok

      {:ok, %HTTPoison.Response{status_code: status}} ->
        Logger.warning("Webhook returned non-success status: #{status}")
        {:error, {:http_error, status}}

      {:error, %HTTPoison.Error{reason: reason}} ->
        Logger.error("Failed to send webhook to #{webhook_url}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @doc """
  Formats payload for the specified webhook provider.

  ## Feishu Format
  Feishu (Lark) bot webhooks require a specific format:
  ```json
  {
    "msg_type": "text",
    "content": {
      "text": "message content"
    }
  }
  ```
  """
  def format_payload(payload, :feishu) do
    text = build_feishu_text(payload)

    %{
      "msg_type" => "text",
      "content" => %{
        "text" => text
      }
    }
  end

  def format_payload(payload, :default), do: payload
  def format_payload(payload, _), do: format_payload(payload, :default)

  defp build_feishu_text(%{event: "health_check_failure", failures: failures} = payload) do
    timestamp = Map.get(payload, :timestamp, DateTime.utc_now() |> DateTime.to_iso8601())

    failure_details =
      Enum.map_join(failures, "\n", fn failure ->
        "• #{failure.name}: #{format_failure_reason(failure.reason)}"
      end)

    """
    ⚠️ Health Check Alert

    Timestamp: #{timestamp}
    Failed Targets: #{length(failures)}

    #{failure_details}
    """
  end

  defp build_feishu_text(payload) when is_map(payload) do
    payload
    |> Jason.encode!()
    |> Jason.Formatter.pretty_print()
  end

  defp build_feishu_text(payload) when is_binary(payload), do: payload
  defp build_feishu_text(payload), do: inspect(payload)

  defp format_failure_reason({:unexpected_status, status}), do: "Unexpected status code: #{status}"
  defp format_failure_reason(:timeout), do: "Request timeout"
  defp format_failure_reason(:econnrefused), do: "Connection refused"
  defp format_failure_reason(reason) when is_atom(reason), do: to_string(reason)
  defp format_failure_reason(reason), do: inspect(reason)

  @doc """
  Runs health checks for all configured targets.
  Sends webhook notifications on failures.
  This function is intended to be called by Quantum scheduler.
  """
  def run_health_checks do
    config = Application.get_env(:exhub, __MODULE__, [])
    targets = Keyword.get(config, :targets, [])
    webhook_url = Keyword.get(config, :webhook_url)
    webhook_provider = Keyword.get(config, :webhook_provider, :default)

    if Enum.empty?(targets) do
      Logger.warning("No targets configured for HealthCheck")
      {:error, :no_targets_configured}
    else
      results = Enum.map(targets, &check_target/1)
      failed = Enum.filter(results, fn {status, _} -> status == :error end)

      unless Enum.empty?(failed) do
        notify_failures(webhook_url, failed, webhook_provider)
      end

      {:ok, results}
    end
  end

  defp check_target(target_config) do
    url = Keyword.fetch!(target_config, :url)
    name = Keyword.get(target_config, :name, url)
    opts = Keyword.drop(target_config, [:url, :name])

    case check_target_url(url, opts) do
      {:ok, status} -> {:ok, %{name: name, url: url, status: status}}
      {:error, reason} -> {:error, %{name: name, url: url, reason: reason}}
    end
  end

  defp notify_failures(nil, failed, _provider) do
    Logger.warning("No webhook configured, skipping notifications for #{length(failed)} failures")
  end

  defp notify_failures(webhook_url, failed, provider) do
    payload = %{
      event: "health_check_failure",
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
      failures: Enum.map(failed, fn {:error, info} -> info end)
    }

    case send_webhook(webhook_url, payload, provider: provider) do
      :ok ->
        Logger.info("Webhook notification sent for #{length(failed)} failures")

      {:error, reason} ->
        Logger.error("Failed to send webhook notification: #{inspect(reason)}")
    end
  end

  @doc """
  Checks a single target and optionally sends webhook on failure.
  """
  def check_single_target(target_config, webhook_url \\ nil, opts \\ []) do
    provider = Keyword.get(opts, :webhook_provider, :default)

    case check_target(target_config) do
      {:ok, result} ->
        {:ok, result}

      {:error, _} = error ->
        if webhook_url do
          notify_failures(webhook_url, [error], provider)
        end
        error
    end
  end
end
