defmodule Exhub.MCP.Tools.DocExtract.Client do
  @moduledoc """
  Shared client for document extraction using Gitee AI Async Document Parse API.

  This module provides the core extraction functionality that can be used by
  multiple tools (doc_extract, read_file, etc.).
  """

  @submit_url "https://ai.gitee.com/v1/async/documents/parse"
  @task_url "https://ai.gitee.com/v1/task"
  @default_model "PaddleOCR-VL-1.5"
  @poll_interval_ms 5_000
  @max_poll_attempts 60
  @http_timeout_ms 30_000
  @poll_http_timeout_ms 15_000
  @boundary_random_max 999_999_999

  @doc_extensions ~w(.pdf .docx .doc .png .jpg .jpeg .tiff .tif .bmp .gif .webp)

  @doc """
  Returns the list of supported document extensions.
  """
  def supported_extensions, do: @doc_extensions

  @doc """
  Checks if a file is a document type based on its extension.
  """
  def document_type?(path) when is_binary(path) do
    ext = Path.extname(path) |> String.downcase()
    ext in @doc_extensions
  end

  def document_type?(_), do: false

  @doc """
  Extracts text from a document file (local path or URL).

  Returns `{:ok, text}` on success or `{:error, reason}` on failure.
  """
  def extract(file, opts \\ []) do
    include_image = Keyword.get(opts, :include_image, true)
    output_format = Keyword.get(opts, :output_format, "md")

    api_key = Application.get_env(:exhub, :giteeai_api_key, "")

    if api_key == "" do
      {:error, "Gitee AI API key not configured. Run: mix scr.insert dev giteeai_api_key \"your-key\""}
    else
      do_extract(file, include_image, output_format, api_key)
    end
  end

  defp do_extract(file, include_image, output_format, api_key) do
    with {:ok, task_id} <- submit_task(file, include_image, output_format, api_key),
         {:ok, result} <- poll_task(task_id, api_key) do
      {:ok, extract_text(result)}
    end
  end

  defp submit_task(file, include_image, output_format, api_key) do
    headers = [
      {"Authorization", "Bearer #{api_key}"}
    ]

    base_fields = [
      {"model", @default_model},
      {"include_image", to_string(include_image)},
      {"include_image_base64", "false"},
      {"output_format", output_format}
    ]

    case build_file_field(file) do
      {:ok, file_field} ->
        fields = base_fields ++ [file_field]
        boundary = "----ExhubDocExtract#{:rand.uniform(@boundary_random_max)}"

        {body, content_type} = build_multipart(fields, boundary)

        full_headers = [{"Content-Type", content_type} | headers]

        case HTTPoison.post(@submit_url, body, full_headers,
               recv_timeout: @http_timeout_ms,
               timeout: @http_timeout_ms
             ) do
          {:ok, %HTTPoison.Response{status_code: 200, body: resp_body}} ->
            case Jason.decode(resp_body) do
              {:ok, %{"task_id" => task_id}} when is_binary(task_id) ->
                {:ok, task_id}

              {:ok, decoded} ->
                {:error, "No task_id in response: #{inspect(decoded)}"}

              {:error, reason} ->
                {:error, "Failed to decode submit response: #{inspect(reason)}"}
            end

          {:ok, %HTTPoison.Response{status_code: status, body: resp_body}} ->
            {:error, "HTTP #{status}: #{resp_body}"}

          {:error, %HTTPoison.Error{reason: reason}} ->
            {:error, "HTTP request failed: #{inspect(reason)}"}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp build_file_field(file) do
    cond do
      String.starts_with?(file, ["http://", "https://"]) ->
        case HTTPoison.get(file, [], recv_timeout: @http_timeout_ms, timeout: @http_timeout_ms) do
          {:ok, %HTTPoison.Response{status_code: 200, body: content, headers: resp_headers}} ->
            filename = file |> URI.parse() |> Map.get(:path, "document") |> Path.basename()
            filename = if filename == "" or filename == "/", do: "document", else: filename
            content_type = get_content_type_from_headers(resp_headers) || "application/octet-stream"
            {:ok, {"file", {filename, content, content_type}}}

          {:ok, %HTTPoison.Response{status_code: status}} ->
            {:error, "Failed to download file, HTTP #{status}"}

          {:error, %HTTPoison.Error{reason: reason}} ->
            {:error, "Failed to download file: #{inspect(reason)}"}
        end

      File.exists?(file) ->
        filename = Path.basename(file)
        mime = guess_mime(filename)

        case File.read(file) do
          {:ok, content} ->
            {:ok, {"file", {filename, content, mime}}}

          {:error, reason} ->
            {:error, "Cannot read file #{file}: #{inspect(reason)}"}
        end

      true ->
        {:error, "File not found: #{file}"}
    end
  end

  defp poll_task(task_id, api_key, attempt \\ 0)

  defp poll_task(_task_id, _api_key, attempt) when attempt >= @max_poll_attempts do
    {:error, "Timed out after #{@max_poll_attempts} attempts (#{@max_poll_attempts * @poll_interval_ms / 1000}s)"}
  end

  defp poll_task(task_id, api_key, attempt) do
    :timer.sleep(@poll_interval_ms)

    url = "#{@task_url}/#{task_id}"
    headers = [{"Authorization", "Bearer #{api_key}"}]

    case HTTPoison.get(url, headers, recv_timeout: @poll_http_timeout_ms, timeout: @poll_http_timeout_ms) do
      {:ok, %HTTPoison.Response{status_code: 200, body: resp_body}} ->
        case Jason.decode(resp_body) do
          {:ok, result} ->
            handle_poll_result(result, task_id, api_key, attempt)

          {:error, reason} ->
            {:error, "Failed to decode poll response: #{inspect(reason)}"}
        end

      {:ok, %HTTPoison.Response{status_code: status, body: body}} ->
        {:error, "Poll HTTP #{status}: #{body}"}

      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, "Poll request failed: #{inspect(reason)}"}
    end
  end

  defp handle_poll_result(result, task_id, api_key, attempt) do
    cond do
      Map.get(result, "error") ->
        msg = Map.get(result, "message", "Unknown error")
        {:error, "#{result["error"]}: #{msg}"}

      Map.get(result, "status") == "success" ->
        {:ok, result}

      Map.get(result, "status") in ["failed", "cancelled"] ->
        {:error, "Task ended with status: #{result["status"]}"}

      true ->
        poll_task(task_id, api_key, attempt + 1)
    end
  end

  defp extract_text(result) do
    output = Map.get(result, "output", %{})

    cond do
      Map.has_key?(output, "segments") ->
        segments = Map.get(output, "segments", [])

        segments
        |> Enum.sort_by(&Map.get(&1, "index", 0))
        |> Enum.map_join("\n\n", &Map.get(&1, "content", ""))
        |> String.trim()

      Map.has_key?(output, "text_result") ->
        Map.get(output, "text_result", "") |> String.trim()

      Map.has_key?(output, "file_url") ->
        file_url = Map.get(output, "file_url")

        case HTTPoison.get(file_url, [], recv_timeout: @http_timeout_ms, timeout: @http_timeout_ms) do
          {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
            String.trim(body)

          _ ->
            "Result file available at: #{file_url}"
        end

      true ->
        "No text extracted or unknown output format.\n\nRaw output: #{inspect(output)}"
    end
  end

  defp build_multipart(fields, boundary) do
    parts =
      Enum.map(fields, fn
        {name, {filename, content, content_type}} ->
          [
            "--#{boundary}\r\n",
            "Content-Disposition: form-data; name=\"#{name}\"; filename=\"#{filename}\"\r\n",
            "Content-Type: #{content_type}\r\n",
            "\r\n",
            content,
            "\r\n"
          ]

        {name, value} ->
          [
            "--#{boundary}\r\n",
            "Content-Disposition: form-data; name=\"#{name}\"\r\n",
            "\r\n",
            value,
            "\r\n"
          ]
      end)

    body = IO.iodata_to_binary([parts, "--#{boundary}--\r\n"])
    content_type = "multipart/form-data; boundary=#{boundary}"
    {body, content_type}
  end

  @doc """
  Guesses the MIME type from a filename based on its extension.
  """
  def guess_mime(filename) do
    case Path.extname(filename) |> String.downcase() do
      ".pdf"  -> "application/pdf"
      ".docx" -> "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
      ".doc"  -> "application/msword"
      ".png"  -> "image/png"
      ".jpg"  -> "image/jpeg"
      ".jpeg" -> "image/jpeg"
      ".tiff" -> "image/tiff"
      ".tif"  -> "image/tiff"
      ".bmp"  -> "image/bmp"
      ".gif"  -> "image/gif"
      ".webp" -> "image/webp"
      _       -> "application/octet-stream"
    end
  end

  defp get_content_type_from_headers(headers) do
    headers
    |> Enum.find_value(fn {k, v} ->
      if String.downcase(k) == "content-type", do: v
    end)
  end
end
