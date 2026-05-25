defmodule Exhub.MCP.Tools.Emacs.ListBuffers do
  @moduledoc """
  MCP Tool: emacs_list_buffers

  List open buffers in Emacs with filtering and pagination.

  Returns buffer names with optional details, supporting:
  - Limiting results (default: 20 most recent buffers)
  - Keyword filtering (case-insensitive match on buffer names)
  - Total buffer count display
  - Detailed buffer information (size, major mode)
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Emacs.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "emacs_list_buffers"

  @impl true
  def description do
    """
    List open buffers in Emacs with filtering and pagination.

    Returns a list of buffer names currently open in the Emacs editor.
    By default, returns the 20 most recently active buffers with total count.
    Supports keyword filtering and custom limits.

    Examples:
    - List recent buffers: {}
    - List buffers with details: {"include_details": true}
    - List specific buffers: {"keyword": "el", "limit": 5}
    - List all buffers: {"limit": 0}
    """
  end

  schema do
    field(:include_details, :boolean,
      description: "Whether to include buffer details like size and mode (default: false)",
      default: false
    )

    field(:limit, :integer,
      description: "Maximum number of buffers to return (default: 20)",
      default: 20
    )

    field(:keyword, :string,
      description: "Filter buffers by keyword (case-insensitive match on buffer name)",
      default: nil
    )

    field(:include_total, :boolean,
      description: "Whether to include total buffer count (default: true)",
      default: true
    )
  end

  @impl true
  def execute(params, frame) do
    include_details = Map.get(params, :include_details, false)
    limit = Map.get(params, :limit, 20)
    keyword = Map.get(params, :keyword)
    include_total = Map.get(params, :include_total, true)

    command = build_elisp_command(include_details, keyword, limit, include_total)

    case Helpers.send_command(command) do
      {:ok, response} ->
        output = parse_response(response, include_details, include_total)
        resp = Response.tool() |> Response.text(output)
        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to list buffers: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end

  defp build_elisp_command(include_details, keyword, limit, include_total) do
    # Build the base buffer list expression
    base_expr = "(buffer-list)"

    # Add keyword filtering if provided (case-insensitive)
    filtered_expr =
      if keyword do
        escaped_keyword = Helpers.escape_elisp(keyword)
        "(let ((case-fold-search t)) (cl-remove-if-not (lambda (buf) (string-match-p (regexp-quote \"#{escaped_keyword}\") (buffer-name buf))) #{base_expr}))"
      else
        base_expr
      end

    # Add limit if specified (0 means no limit)
    limited_expr =
      if limit > 0 do
        "(seq-take #{filtered_expr} #{limit})"
      else
        filtered_expr
      end

    # Build the mapping expression based on include_details
    mapping_expr =
      if include_details do
        "(mapcar (lambda (buf) (list (buffer-name buf) (buffer-size buf) (with-current-buffer buf major-mode))) #{limited_expr})"
      else
        "(mapcar 'buffer-name #{limited_expr})"
      end

    # Wrap with total count if requested
    if include_total do
      "(let ((all-buffers (buffer-list)) (filtered-buffers #{mapping_expr})) (list (length all-buffers) filtered-buffers))"
    else
      mapping_expr
    end
  end

  defp parse_response(response, include_details, include_total) do
    if include_total do
      # Response is (total-count (buffer-list))
      case parse_total_and_list(response) do
        {total, list_response} ->
          buffers = parse_buffer_data(list_response, include_details)
          format_with_total(buffers, total, include_details)

        :error ->
          # Fallback: treat as simple list
          buffers = parse_buffer_data(response, include_details)
          format_buffers(buffers, include_details)
      end
    else
      buffers = parse_buffer_data(response, include_details)
      format_buffers(buffers, include_details)
    end
  end

  defp parse_total_and_list(response) do
    cleaned = response |> String.trim() |> String.trim("\"")

    # Parse structured response: (total (buffer-list))
    # We need to handle nested parentheses properly
    case extract_total_and_list(cleaned) do
      {total_str, list_str} ->
        total = String.to_integer(total_str)
        {total, list_str}

      :error ->
        :error
    end
  end

  defp extract_total_and_list(str) do
    # Find the first opening parenthesis
    if String.starts_with?(str, "(") do
      # Find the total number (first element)
      case Regex.run(~r/^\((\d+)\s+/, str, capture: :all_but_first) do
        [total_str] ->
          # Find the matching closing parenthesis for the list
          # We need to find the start of the inner list
          inner_start = String.length("(#{total_str} ")
          inner = String.slice(str, inner_start..-1//1)

          # Find the matching closing parenthesis
          case find_matching_paren(inner, 0, 0) do
            {:ok, list_end} ->
              # Include the parentheses so parse_buffer_list can detect the format
              list_str = String.slice(inner, 0..list_end//1)
              {total_str, list_str}

            :error ->
              :error
          end

        _ ->
          :error
      end
    else
      :error
    end
  end

  defp find_matching_paren(str, pos, depth) do
    if pos >= String.length(str) do
      if depth == 0, do: {:ok, pos}, else: :error
    else
      char = String.at(str, pos)

      case char do
        "(" ->
          find_matching_paren(str, pos + 1, depth + 1)

        ")" ->
          if depth == 1 do
            {:ok, pos}
          else
            find_matching_paren(str, pos + 1, depth - 1)
          end

        _ ->
          find_matching_paren(str, pos + 1, depth)
      end
    end
  end

  defp parse_buffer_data(response, include_details) do
    if include_details do
      Helpers.parse_detailed_buffer_list(response)
    else
      Helpers.parse_buffer_list(response)
    end
  end

  defp format_with_total(buffers, total, include_details) do
    filtered_count = length(buffers)
    header = "#{filtered_count} of #{total} buffer(s):\n\n"

    body =
      if include_details do
        format_detailed_buffer_list_body(buffers)
      else
        format_buffer_list_body(buffers)
      end

    header <> body
  end

  defp format_buffers(buffers, include_details) do
    if include_details do
      format_detailed_buffer_list(buffers)
    else
      Helpers.format_buffer_list(buffers)
    end
  end

  defp format_buffer_list_body(buffers) do
    if buffers == [] do
      "No buffers found."
    else
      Enum.map_join(buffers, "\n", fn buffer -> "  - #{buffer}" end)
    end
  end

  defp format_detailed_buffer_list_body(buffers) do
    if buffers == [] do
      "No buffers found."
    else
      Enum.map_join(buffers, "\n", fn
        {name, size, mode} ->
          "  - #{name} (#{size} bytes, #{mode})"
        name when is_binary(name) ->
          "  - #{name}"
      end)
    end
  end

  defp format_detailed_buffer_list(buffers) do
    if buffers == [] do
      "No buffers found."
    else
      header = "#{length(buffers)} buffer(s):\n\n"
      body = format_detailed_buffer_list_body(buffers)
      header <> body
    end
  end
end