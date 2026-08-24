defmodule Exhub.BlinkSearch.Backends.CurrentBuffer do
  @moduledoc """
  Current Buffer backend — searches the current buffer content using `ripgrep`.

  Emacs pushes the buffer content (base64-encoded) via `init_buffer/3`.
  The content is written to a temp file and searched with `rg --json`.
  """

  use Exhub.BlinkSearch.Backend

  alias Exhub.BlinkSearch.Backend

  @impl true
  def search_match(prefix, state) do
    temp_path = Map.get(state, :buffer_temp_path)
    clean_prefix = String.replace(prefix, "*", "")
    words = String.split(clean_prefix, ~r/\s+/, trim: true)

    if words != [] and temp_path and File.exists?(temp_path) and clean_prefix != "" do
      rg_bin = System.find_executable("rg")

      if rg_bin do
        pattern = Enum.join(words, ".*")

        command = [
          rg_bin,
          "-S",
          "--json",
          "--max-columns",
          "300",
          "-g",
          "!node_modules",
          "-g",
          "!__pycache__",
          "-g",
          "!dist",
          pattern,
          temp_path
        ]

        Backend.get_process_result(command)
        |> Enum.map(fn line -> Backend.parse_rg_line(line) end)
        |> Enum.reject(&is_nil/1)
        |> Enum.map(fn result ->
          if String.length(result.text) < 1000 do
            result
          else
            %{text: String.slice(result.text, 0, 1000), matches: [hd(result.matches)]}
          end
        end)
      else
        []
      end
    else
      []
    end
  end

  @impl true
  def do_action(candidate, state) do
    buffer_name = Map.get(state, :buffer_name, "")
    text = Backend.candidate_text(candidate)

    case String.split(text, ":", parts: 3) do
      [line, col, _rest] ->
        Exhub.send_message(
          ~s|(blink-search-current-buffer-do #{Backend.elisp_quote(buffer_name)} #{line} #{col})|
        )

        # Clean up temp file
        cleanup_temp(state)

      _ ->
        :ok
    end

    :ok
  end

  @impl true
  def select(candidate, state) do
    buffer_name = Map.get(state, :buffer_name, "")
    text = Backend.candidate_text(candidate)

    case String.split(text, ":", parts: 3) do
      [line, col, _rest] ->
        Exhub.send_message(
          ~s|(blink-search-current-buffer-preview #{Backend.elisp_quote(buffer_name)} #{line} #{col})|
        )

      _ ->
        :ok
    end

    :ok
  end

  @impl true
  def copy(candidate, _state) do
    text = Backend.candidate_text(candidate)
    copy_text = text |> String.split(":") |> List.last() |> String.trim()
    Exhub.send_message(~s|(kill-new #{Backend.elisp_quote(copy_text)})|)
    Exhub.send_message(~s|(message "[Blink-Search] Copy: #{Backend.escape_message(copy_text)}")|)
    :ok
  end

  @impl true
  def clean(state) do
    cleanup_temp(state)
    state
  end

  @doc """
  Initialize the current buffer content.

  Decodes base64 content and writes it to a temp file for rg to search.
  """
  @spec init_buffer(String.t(), String.t(), map()) :: map()
  def init_buffer(buffer_name, buffer_content_base64, state) do
    # Clean up previous temp file
    cleanup_temp(state)

    content =
      case Base.decode64(buffer_content_base64) do
        {:ok, decoded} -> decoded
        :error -> ""
      end

    # Create temp file with md5-based name (matching Python behavior)
    md5 = :erlang.md5(buffer_name) |> Base.encode16(case: :lower)
    temp_path = Path.join(System.tmp_dir!(), "blink-search-temp-buffer-#{md5}")

    File.write!(temp_path, content)

    state
    |> Map.put(:buffer_name, buffer_name)
    |> Map.put(:buffer_temp_path, temp_path)
  end

  # Private helpers

  defp cleanup_temp(state) do
    case Map.get(state, :buffer_temp_path) do
      nil -> :ok
      path -> File.rm(path)
    end
  rescue
    _ -> :ok
  end
end
