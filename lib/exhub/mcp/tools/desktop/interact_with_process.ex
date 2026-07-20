defmodule Exhub.MCP.Tools.Desktop.InteractWithProcess do
  @moduledoc """
  MCP Tool: interact_with_process

  Send input to an interactive process's stdin with optional template expansion.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers
  alias Exhub.MCP.Desktop.ProcessStore

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

    Parameters:
    - process_id: The ID returned by start_process
    - input: The text to send to the process's stdin
    - mode: Input processing mode: 'raw' (default, send as-is) or 'template'
      (expand ${VAR_NAME} and `command` before sending)
    """
  end

  schema do
    field(:process_id, {:required, :string}, description: "The ID returned by start_process")

    field(:input, {:required, :string},
      description: "The text to send to the process's stdin"
    )

    field(:mode, :string,
      default: "raw",
      description:
        "Input processing mode: 'raw' (default, send as-is) or 'template' (expand ${VAR_NAME} and `command` before sending)"
    )
  end

  @impl true
  def execute(params, frame) do
    process_id = Map.get(params, :process_id)
    input = Map.get(params, :input)
    mode = Map.get(params, :mode, "raw")

    case prepare_input(input, mode) do
      {:ok, expanded_input, input_expanded} ->
        case ProcessStore.send_input(process_id, expanded_input) do
          :ok ->
            # When expansion occurred, the expanded input may contain secrets
            # (e.g. ${SSH_PASS}, `cat pass.txt`) that should not be echoed back.
            # Only include input_sent when no expansion happened (raw mode or
            # no templates found), where it equals the original user input.
            response_data = %{
              "success" => true,
              "process_id" => process_id,
              "input_expanded" => input_expanded
            }

            response_data =
              unless input_expanded do
                Map.put(response_data, "input_sent", expanded_input)
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
                "error" => "Process #{process_id} is not interactive. Start with interactive: true."
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
    {:ok, input, false}
  end

  # ============================================================================
  # Template Expansion
  # ============================================================================

  # Expands ${VAR_NAME} and `command` templates in the input string.
  #
  # Returns `{:ok, expanded_input, expanded?}` where `expanded?` indicates
  # whether any template was found and expanded. Returns `{:error, reason}` if
  # a command substitution fails.
  #
  # Expansion is single-pass: the output of command substitution is not
  # re-scanned for further expansion, matching shell semantics.
  defp expand_input(input) do
    if has_template?(input) do
      try do
        expanded = String.replace(input, @template_regex, &replace_template/1)
        {:ok, expanded, true}
      rescue
        e -> {:error, Exception.message(e)}
      end
    else
      {:ok, input, false}
    end
  end

  defp has_template?(input) do
    String.contains?(input, "${") or String.contains?(input, "`")
  end

  # Replaces a single template match with its expanded value.
  defp replace_template(match) do
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
  defp run_command_substitution(cmd) do
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
