defmodule Exhub.MCP.Emacs.Helpers do
  @moduledoc """
  Helper functions for Emacs MCP server communication.

  Provides functions to send Elisp commands to Emacs and receive responses
  through the existing WebSocket connection.
  """

  require Logger

  @doc """
  Send an Elisp command to Emacs and wait for a response.

  This function sends a command to Emacs via the WebSocket connection and
  waits for a response using a request-response mechanism.

  ## Parameters
  - `command` - The Elisp command string to execute in Emacs
  - `timeout` - Timeout in milliseconds (default: 5000)

  ## Returns
  - `{:ok, response}` - The response from Emacs
  - `{:error, reason}` - If the command fails or times out
  """
  def send_command(command, timeout \\ 30_000) do
    request_id = generate_request_id()
    wrapped_command = wrap_command_with_response(command, request_id)

    # Register to receive the response
    Registry.register(Exhub.Registry, "emacs_response_#{request_id}", self())

    # Send the command to Emacs
    Exhub.send_message(wrapped_command)

    # Wait for the response
    receive do
      {:emacs_response, ^request_id, response} ->
        Registry.unregister(Exhub.Registry, "emacs_response_#{request_id}")
        {:ok, response}
    after
      timeout ->
        Registry.unregister(Exhub.Registry, "emacs_response_#{request_id}")
        {:error, :timeout}
    end
  end

  @doc """
  Generate a unique request ID for command-response matching.
  """
  def generate_request_id do
    :crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower)
  end

  @doc """
  Wrap an Elisp command with response handling.

  This wraps the command to send the response back through the WebSocket
  with the request ID for matching.
  """
  def wrap_command_with_response(command, request_id) do
    """
    (let ((result (condition-case err
                    (eval (read "#{escape_elisp(command)}"))
                    (error (format "Error: %s" (error-message-string err))))))
      (exhub-send-response "#{request_id}" (prin1-to-string result)))
    """
  end

  @doc """
  Escape a string for inclusion in Elisp code.
  """
  def escape_elisp(string) do
    string
    |> String.replace("\\", "\\\\")
    |> String.replace("\"", "\\\"")
    |> String.replace("\n", "\\n")
    |> String.replace("\t", "\\t")
  end

  @doc """
  Parse a buffer list response from Emacs.

  Emacs returns buffer list as a string representation of a list.
  This function parses it into a list of buffer names.
  """
  def parse_buffer_list(response) do
    # Remove surrounding quotes and parse as Elisp list
    cleaned =
      response
      |> String.trim()
      |> String.trim("\"")

    # Simple parsing for ("buffer1" "buffer2" ...) format
    if String.starts_with?(cleaned, "(") and String.ends_with?(cleaned, ")") do
      cleaned
      |> String.slice(1..-2//1)
      |> String.split("\" \"")
      |> Enum.map(fn s ->
        s
        |> String.trim("\"")
        |> String.trim()
      end)
      |> Enum.reject(&(&1 == ""))
    else
      # Fallback: split by newlines or spaces
      cleaned
      |> String.split(~r/[\n\s]+/, trim: true)
      |> Enum.reject(&(&1 == ""))
    end
  end

  @doc """
  Format a buffer list for display.
  """
  def format_buffer_list(buffers) do
    if buffers == [] do
      "No buffers found."
    else
      header = "#{length(buffers)} buffer(s):\n\n"
      body = Enum.map_join(buffers, "\n", fn buffer -> "  - #{buffer}" end)
      header <> body
    end
  end

  @doc """
  Parse a detailed buffer list response from Emacs.

  Emacs returns detailed buffer list as nested list format:
  (("buffer1" 1234 emacs-lisp-mode) ("buffer2" 5678 fundamental-mode))

  This function parses it into a list of {name, size, mode} tuples.
  """
  def parse_detailed_buffer_list(response) do
    cleaned =
      response
      |> String.trim()
      |> String.trim("\"")

    # Parse nested list format
    if String.starts_with?(cleaned, "((") and String.ends_with?(cleaned, "))") do
      # Remove outer parens
      inner = cleaned |> String.slice(1..-2//1)

      # Split by ") (" to get individual entries
      inner
      |> String.split(~r/\)\s*\(/, trim: true)
      |> Enum.map(fn entry ->
        entry
        |> String.trim_leading("(")
        |> String.trim_trailing(")")
        |> String.trim()
      end)
      |> Enum.map(fn entry ->
        case Regex.run(~r/^"([^"]+)"\s+(\d+)\s+(\S+)$/, entry) do
          [_, name, size_str, mode] ->
            {name, String.to_integer(size_str), mode}

          _ ->
            {entry, 0, "unknown"}
        end
      end)
    else
      # Fallback: parse as simple list
      parse_buffer_list(response)
      |> Enum.map(fn name -> {name, 0, "unknown"} end)
    end
  end
end
