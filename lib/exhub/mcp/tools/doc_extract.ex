defmodule Exhub.MCP.Tools.DocExtract do
  @moduledoc """
  MCP Tool for extracting and recognizing text from documents (PDF, DOCX, etc.)
  using the Gitee AI Async Document Parse API (PaddleOCR-VL-1.5).

  Supports both local file paths and remote URLs as input.
  The extraction is asynchronous: the tool submits a task, polls until
  completion, then returns the full markdown-formatted text.
  """

  alias Anubis.Server.Response

  use Anubis.Server.Component, type: :tool

  @submit_url "https://ai.gitee.com/v1/async/documents/parse"
  @task_url "https://ai.gitee.com/v1/task"
  @default_model "PaddleOCR-VL-1.5"
  @poll_interval_ms 5_000
  @max_poll_attempts 60  # 5 min max (60 * 5s)
  @http_timeout_ms 30_000
  @poll_http_timeout_ms 15_000
  @boundary_random_max 999_999_999

  def name, do: "doc_extract"

  @impl true
  def description do
    """
    Extract and recognize text from documents (PDF, DOCX, images, etc.) using Gitee AI.

    Powered by PaddleOCR-VL-1.5 via the Gitee AI Async Document Parse API.
    Supports both local file paths and remote URLs as input.

    The tool submits an async parsing task, waits for completion, then returns
    the extracted content in Markdown format.

    **Supported input formats:** PDF, DOCX, DOC, PNG, JPG, JPEG, TIFF, BMP, etc.

    **Returns:** Extracted text in Markdown format, with layout and structure preserved.

    Display the result as: 📖[Extracted Document Content]
    """
  end

  schema do
    field(:file, {:required, :string},
      description: "Path to the document file (e.g. /path/to/doc.pdf) or a remote URL (http/https)"
    )

    field(:include_image, :boolean,
      description: "Whether to include image references in the output (default: true)"
    )

    field(:output_format, :string,
      description: "Output format: 'md' for Markdown (default), 'text' for plain text"
    )
  end

  @impl true
  def execute(params, frame) do
    file = Map.get(params, :file)
    include_image = Map.get(params, :include_image, true)
    output_format = Map.get(params, :output_format, "md")

    cond do
      is_nil(file) or file == "" ->
        resp = Response.tool() |> Response.error("`file` is required — provide a local path or URL")
        {:reply, resp, frame}

      true ->
        api_key = Application.get_env(:exhub, :giteeai_api_key, "")

        if api_key == "" do
          resp =
            Response.tool()
            |> Response.error(
              "Gitee AI API key not configured. Run: mix scr.insert dev giteeai_api_key \"your-key\""
            )

          {:reply, resp, frame}
        else
          do_extract(file, include_image, output_format, api_key, frame)
        end
    end
  end

  # ---------------------------------------------------------------------------
  # Private helpers
  # ---------------------------------------------------------------------------

  defp do_extract(file, include_image, output_format, api_key, frame) do
    case submit_task(file, include_image, output_format, api_key) do
      {:ok, task_id} ->
        case poll_task(task_id, api_key) do
          {:ok, result} ->
            text = extract_text(result)
            resp = Response.tool() |> Response.text(text)
            {:reply, resp, frame}

          {:error, reason} ->
            resp = Response.tool() |> Response.error("Extraction failed: #{reason}")
            {:reply, resp, frame}
        end

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to submit task: #{reason}")
        {:reply, resp, frame}
    end
  end

  defp submit_task(file, include_image, output_format, api_key) do
    headers = [
      {"Authorization", "Bearer #{api_key}"}
    ]

    # Build multipart form fields
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
        # Download the remote file content first
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
        # Still pending/processing — keep polling
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

  # ---------------------------------------------------------------------------
  # Multipart builder (no external dependency needed)
  # ---------------------------------------------------------------------------

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

  defp guess_mime(filename) do
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
