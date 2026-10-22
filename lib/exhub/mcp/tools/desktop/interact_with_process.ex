defmodule Exhub.MCP.Tools.Desktop.InteractWithProcess do
  @moduledoc """
  MCP Tool: interact_with_process

  Send input to an interactive process's stdin with optional template expansion.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.CostGuard
  alias Exhub.MCP.Desktop.Helpers
  alias Exhub.MCP.Desktop.ProcessStore
  alias Exhub.MCP.Desktop.SecretGuard

  use Anubis.Server.Component, type: :tool

  require Logger

  def name, do: "interact_with_process"

  # Matches ${VAR_NAME} or `command` for single-pass template expansion.
  # Env var names: start with letter or underscore, followed by letters/digits/underscores.
  # Command substitution: any non-backtick characters enclosed in backticks.
  @template_regex ~r/\$\{([A-Za-z_][A-Za-z0-9_]*)\}|`([^`]+)`/

  @impl true
  def description do
    """
    Send input to an interactive process's stdin.

    Use this to interact with processes started with start_process(interactive: true).
    The input is sent directly to the process's stdin. Use this for interactive
    commands, REPLs, or any process that accepts user input.

    Input Modes:
    - raw (default): Input is sent as-is, no expansion is performed.
    - template: Input is processed for shell-style template expansion before sending:
      - Environment variables: ${VAR_NAME} is replaced with the value of the
        environment variable. If the variable is not set, it is replaced with
        an empty string. Example: ${SSH_PASS}
      - Command substitution: `command` is replaced with the stdout of the
        executed command (trailing newlines are trimmed). Example: `cat pass.txt`
        or `openssl rand -hex 16`

    Templates are expanded left-to-right in a single pass. The output of command
    substitution is not re-scanned for further expansion.

    Security: a `command` substitution is an `execute_command` by another name, so
    it passes through the same cost and secret gates — a substitution that would
    scan the whole disk or expose a secret is refused and nothing is sent. When
    any expansion occurs the expanded value is treated as sensitive: it is not
    echoed back in `input_sent`, and it is masked out of the returned `output` so
    a child that echoes its stdin (cat, a REPL, ssh) cannot leak it.

    Parameters:
    - process_id: The ID returned by start_process
    - input: The text to send to the process's stdin
    - mode: Input processing mode: 'raw' (default, send as-is) or 'template'
      (expand ${VAR_NAME} and `command` before sending)
    - wait_seconds: Number of seconds to wait after sending input before
      returning (default: 2). Set to 0 to return immediately without waiting.
    - return_output: Whether to include the process output in the response
      (default: true). When true, the response includes output, status,
      exit_code, and total_lines.
    - auto_newline: Whether to append a newline (\n) to the input before
      sending (default: false). Automatically defaults to true for PTY
      processes (those started with pty: true), since PTY modes require
      newlines for command execution. Set to false explicitly to send
      input without a trailing newline.
    - offset: Line offset to start reading output from (0 = from beginning, default 0)
    - length: Maximum number of output lines to return (default 1000)
    """
  end

  schema do
    field(:process_id, {:required, :string}, description: "The ID returned by start_process")

    field(:input, {:required, :string}, description: "The text to send to the process's stdin")

    field(:mode, :string,
      default: "raw",
      description:
        "Input processing mode: 'raw' (default, send as-is) or 'template' (expand ${VAR_NAME} and `command` before sending)"
    )

    field(:wait_seconds, :integer,
      default: 2,
      description:
        "Number of seconds to wait after sending input before returning (default: 2). Set to 0 to return immediately."
    )

    field(:return_output, :boolean,
      default: true,
      description:
        "Whether to include the process output in the response (default: true). When true, the response includes output, status, exit_code, and total_lines."
    )

    field(:auto_newline, :boolean,
      description:
        "Whether to append a newline (\\n) to the input before sending (default: false). Automatically defaults to true for PTY processes. Set to false explicitly to send input without a trailing newline."
    )

    field(:offset, :integer,
      default: 0,
      description: "Line offset to start reading output from (0 = from beginning, default 0)"
    )

    field(:length, :integer,
      default: 1000,
      description: "Maximum number of output lines to return (default 1000)"
    )
  end

  @impl true
  def execute(params, frame) do
    process_id = Map.get(params, :process_id)
    input = Map.get(params, :input)
    mode = Map.get(params, :mode, "raw")
    wait_seconds = Map.get(params, :wait_seconds, 2)
    return_output = Map.get(params, :return_output, true)
    auto_newline = Map.get(params, :auto_newline)
    offset = Map.get(params, :offset, 0)
    max_lines = Map.get(params, :length, 1000)

    case prepare_input(input, mode) do
      {:ok, expanded_input, input_expanded, expanded_values} ->
        # Determine auto_newline: if not explicitly set, default to true for PTY
        # processes (backend: :erlexec)
        effective_auto_newline =
          if is_nil(auto_newline) do
            case ProcessStore.get(process_id) do
              nil -> false
              entry -> entry.backend == :erlexec
            end
          else
            auto_newline
          end

        # Append newline if auto_newline is enabled
        final_input =
          if effective_auto_newline do
            expanded_input <> "\n"
          else
            expanded_input
          end

        case ProcessStore.send_input(process_id, final_input) do
          :ok ->
            # Wait for the specified duration to allow the process to produce output
            if wait_seconds > 0, do: Process.sleep(wait_seconds * 1000)

            # When expansion occurred, the expanded input may contain secrets
            # (e.g. ${SSH_PASS}, `cat pass.txt`) that should not be echoed back.
            # Only include input_sent when no expansion happened (raw mode or
            # no templates found), where it equals the original user input.
            response_data = %{
              "success" => true,
              "process_id" => process_id,
              "input_expanded" => input_expanded,
              "auto_newline" => effective_auto_newline
            }

            response_data =
              unless input_expanded do
                Map.put(response_data, "input_sent", expanded_input)
              else
                response_data
              end

            # Optionally include process output in the response
            response_data =
              if return_output do
                case ProcessStore.get_output(process_id, offset) do
                  {:ok, result} ->
                    # Touch the process to reset its TTL
                    ProcessStore.touch(process_id)

                    lines =
                      if result.output == "", do: [], else: String.split(result.output, "\n")

                    sliced_lines = Enum.take(lines, max_lines)
                    sliced = Enum.join(sliced_lines, "\n")
                    lines_returned = length(sliced_lines)

                    # A child that echoes its stdin (cat/REPL/ssh) would otherwise
                    # hand the expanded secret straight back in `output`.
                    output = if input_expanded, do: redact(sliced, expanded_values), else: sliced

                    # `result.output` is already offset-sliced, so `lines` holds
                    # only the remaining lines.
                    has_more =
                      length(lines) > lines_returned or
                        (result.status == :running and lines_returned == max_lines)

                    Map.merge(response_data, %{
                      "output" => output,
                      "offset" => offset,
                      "lines_returned" => lines_returned,
                      "total_lines" => result.total_lines,
                      "status" => to_string(result.status),
                      "exit_code" => result.exit_code,
                      "has_more" => has_more
                    })

                  {:error, :not_found} ->
                    Map.put(
                      response_data,
                      "output_error",
                      "Process not found when reading output"
                    )
                end
              else
                response_data
              end

            resp =
              Response.tool()
              |> Helpers.toon_response(response_data)

            {:reply, resp, frame}

          {:error, :not_found} ->
            resp =
              Response.tool()
              |> Helpers.toon_response(%{
                "success" => false,
                "error" => "Process not found: #{process_id}"
              })

            {:reply, resp, frame}

          {:error, :not_interactive} ->
            resp =
              Response.tool()
              |> Helpers.toon_response(%{
                "success" => false,
                "error" =>
                  "Process #{process_id} is not interactive. Start with interactive: true."
              })

            {:reply, resp, frame}

          {:error, :no_port} ->
            resp =
              Response.tool()
              |> Helpers.toon_response(%{
                "success" => false,
                "error" => "Process #{process_id} has no port reference"
              })

            {:reply, resp, frame}

          {:error, :process_not_running} ->
            resp =
              Response.tool()
              |> Helpers.toon_response(%{
                "success" => false,
                "error" => "Process #{process_id} is not running"
              })

            {:reply, resp, frame}

          {:error, reason} ->
            resp =
              Response.tool()
              |> Helpers.toon_response(%{
                "success" => false,
                "error" => "Failed to send input: #{reason}"
              })

            {:reply, resp, frame}
        end

      {:error, reason} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{
            "success" => false,
            "error" => "Input expansion failed: #{reason}"
          })

        {:reply, resp, frame}
    end
  end

  # ============================================================================
  # Input Preparation
  # ============================================================================

  # Dispatches input processing based on the selected mode.
  # - "template": expand ${VAR_NAME} and `command` templates
  # - any other value (including "raw"): pass input through unchanged
  defp prepare_input(input, "template") do
    expand_input(input)
  end

  defp prepare_input(input, _mode) do
    {:ok, input, false, []}
  end

  # Masks every expanded value out of the returned output. The value is what a
  # child would echo back when it reads its stdin, so replacing it is enough to
  # keep the expansion out of the conversation.
  defp redact(text, values) when is_list(values) do
    values
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
    |> Enum.reduce(text, fn value, acc -> String.replace(acc, value, "[redacted]") end)
  end

  # ============================================================================
  # Template Expansion
  # ============================================================================

  # Expands ${VAR_NAME} and `command` templates in the input string.
  #
  # Returns `{:ok, expanded_input, expanded?, values}` where `expanded?` indicates
  # whether any template was found and expanded, and `values` are the expanded
  # values (used to redact them from any echoed output). Returns `{:error, reason}`
  # if a command substitution fails.
  #
  # Expansion is single-pass: the output of command substitution is not
  # re-scanned for further expansion, matching shell semantics.
  defp expand_input(input) do
    if has_template?(input) do
      try do
        {expanded, values} = do_expand(input)
        {:ok, expanded, true, values}
      rescue
        e -> {:error, Exception.message(e)}
      end
    else
      {:ok, input, false, []}
    end
  end

  defp has_template?(input) do
    String.contains?(input, "${") or String.contains?(input, "`")
  end

  # Walks the matches left-to-right, building the expanded string while
  # collecting each expanded value. `Regex.replace/4` cannot do this because its
  # callback has no accumulator.
  defp do_expand(input) do
    matches = Regex.scan(@template_regex, input, return: :index)

    {chunks, last_end, values} =
      Enum.reduce(matches, {[], 0, []}, fn [whole | _captures], {chunks, last_end, values} ->
        {start, len} = whole
        literal = binary_part(input, last_end, start - last_end)
        matched = binary_part(input, start, len)
        value = expand_template(matched)
        {[[literal, value] | chunks], start + len, [value | values]}
      end)

    tail = binary_part(input, last_end, byte_size(input) - last_end)
    {IO.iodata_to_binary([Enum.reverse(chunks), tail]), Enum.reverse(values)}
  end

  # Expands a single template match.
  defp expand_template(match) do
    cond do
      String.starts_with?(match, "${") ->
        # ${VAR_NAME} → env var value (empty string if unset)
        var_name = String.slice(match, 2..-2//1)
        System.get_env(var_name) || ""

      String.starts_with?(match, "`") ->
        # `command` → command stdout (trailing newlines trimmed)
        cmd = String.slice(match, 1..-2//1)

        case run_command_substitution(cmd) do
          {:ok, output} -> output
          {:error, reason} -> raise "Command substitution failed for `#{cmd}`: #{reason}"
        end

      true ->
        match
    end
  end

  # Executes a command in the configured shell and returns trimmed stdout.
  #
  # A backtick substitution runs an arbitrary shell command, so it goes through
  # the same gates as `execute_command` before anything is executed.
  defp run_command_substitution(cmd) do
    with :ok <- guard_substitution(cmd) do
      [shell | args] = Helpers.shell_command_args(cmd, login: false)

      try do
        case System.cmd(shell, args, stderr_to_stdout: true) do
          {output, 0} ->
            # Shell command substitution removes all trailing newlines
            {:ok, String.trim_trailing(output, "\n")}

          {output, exit_code} ->
            {:error, "exit code #{exit_code}: #{String.trim(output)}"}
        end
      rescue
        e -> {:error, Exception.message(e)}
      end
    end
  end

  # Judges the substitution as if it were an `execute_command` call, reusing that
  # tool's secret and cost gates (and their `always_allow` configuration). Both
  # verdicts are normalised to `:ok | {:error, message}` so the caller's `with`
  # sees one shape. The secret gate runs first because its deterministic fast
  # path usually settles a secret-shaped command without a model call.
  defp guard_substitution(cmd) do
    arguments = %{"command" => cmd}

    case SecretGuard.check("execute_command", arguments) do
      :ok ->
        case CostGuard.check("execute_command", arguments) do
          :ok -> :ok
          {:refuse, message} -> {:error, message}
        end

      {:refuse, message} ->
        {:error, message}
    end
  end
end
