defmodule Exhub.MCP.Desktop.Nanoxml do
  @moduledoc """
  Wrapper for the `nanoxml` CLI tool — fast local Office Open XML text extraction.

  Handles .docx, .xlsx, and .pptx files without calling an external API,
  using the nanoxml binary for near-instant text/CSV extraction.
  """

  @nanoxml_bin "nanoxml"

  @office_extensions ~w(.docx .xlsx .pptx)

  @doc """
  Returns the list of office file extensions handled by nanoxml.
  """
  def supported_extensions, do: @office_extensions

  @doc """
  Checks if a file is an Office Open XML type (.docx, .xlsx, .pptx).
  """
  def office_type?(path) when is_binary(path) do
    ext = path |> Path.extname() |> String.downcase()
    ext in @office_extensions
  end

  def office_type?(_), do: false

  @doc """
  Extracts text from an Office Open XML file using nanoxml.

  - `.docx` / `.pptx` → plain text via `nanoxml text`
  - `.xlsx` → all sheets as CSV, joined with sheet-name headers

  Returns `{:ok, text}` or `{:error, reason}`.
  """
  def extract_text(path) do
    ext = path |> Path.extname() |> String.downcase()

    cond do
      ext in [".docx", ".pptx"] ->
        run_nanoxml(["text", path])

      ext == ".xlsx" ->
        extract_xlsx(path)

      true ->
        {:error, "Unsupported office file type: #{ext}"}
    end
  end

  # ── XLSX: list sheets, extract each as CSV ─────────────────────────────

  defp extract_xlsx(path) do
    with {:ok, sheets_json} <- run_nanoxml(["sheets", path, "--json"]),
         {:ok, sheets} <- Jason.decode(sheets_json),
         {:ok, parts} <- extract_all_sheets(path, sheets) do
      {:ok, Enum.join(parts, "\n\n")}
    end
  end

  defp extract_all_sheets(path, sheets) when is_list(sheets) do
    results =
      sheets
      |> Enum.with_index()
      |> Enum.map(fn {sheet, idx} ->
        name = sheet["name"] || "Sheet#{idx + 1}"

        case run_nanoxml(["csv", path, to_string(idx)]) do
          {:ok, csv} ->
            "## #{name}\n\n#{csv}"

          {:error, reason} ->
            "## #{name}\n\n[Error extracting sheet: #{reason}]"
        end
      end)

    {:ok, results}
  end

  defp extract_all_sheets(_path, _sheets), do: {:ok, []}

  # ── CLI runner (follows run_axcli pattern from mac_use/helpers) ────────

  defp run_nanoxml(args, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 30_000)
    full_argv = [@nanoxml_bin | args]

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
          0 ->
            {:ok, stdout}

          _ ->
            detail = if stderr != "", do: stderr, else: stdout
            {:error, "nanoxml exited with code #{exit_status}: #{detail}"}
        end
      end)

    case Task.yield(task, timeout) do
      {:ok, result} ->
        result

      nil ->
        Task.shutdown(task, :brutal_kill)
        {:error, "nanoxml command timed out after #{timeout}ms"}
    end
  rescue
    e ->
      {:error, "Failed to run nanoxml: #{Exception.message(e)}"}
  end

  defp clean_env do
    System.get_env()
    |> Enum.reject(fn {k, _} ->
      String.starts_with?(k, "RELEASE") or
        k in ["PROGNAME", "ROOTDIR", "BINDIR"]
    end)
    |> Enum.to_list()
  end
end
