defmodule Exhub.Utils do
  @moduledoc """
  Shared utilities for Exhub.

  Includes path helpers and a TRAMP-aware `git_cmd/2` that transparently
  executes git commands on remote hosts when given an Emacs TRAMP path.
  """

  @tramp_regex ~r|^/([^:]+):(?:(?:([^@]+)@)?([^#:]+)(?:#(\d+))?:)?(/.*)$|

  @doc """
  Parse a potential TRAMP path into its components.

  Returns `{:tramp, method, user, host, port, path}` for remote paths,
  or `{:local, path}` for regular local paths.

  ## Examples

      iex> Exhub.Utils.parse_path("/ssh:user@host:/home/repo")
      {:tramp, "ssh", "user", "host", nil, "/home/repo"}

      iex> Exhub.Utils.parse_path("/ssh:user@host#2222:/home/repo")
      {:tramp, "ssh", "user", "host", "2222", "/home/repo"}

      iex> Exhub.Utils.parse_path("/home/repo")
      {:local, "/home/repo"}
  """
  def parse_path(path) when is_binary(path) do
    case Regex.run(@tramp_regex, path) do
      [_, method, user, host, port, remote_path] ->
        user = if user == "", do: nil, else: user
        port = if port == "", do: nil, else: port
        {:tramp, method, user, host, port, remote_path}

      _ ->
        {:local, path}
    end
  end

  @doc """
  Execute a git command, transparently handling TRAMP paths.

  For local paths, runs `git` directly via Exile in the given directory.
  For SSH-based TRAMP paths (`ssh`, `scp`, `rsync`, `sshfs`), runs the
  command remotely over SSH.
  For `sudo`/`su` TRAMP paths, strips the prefix and runs locally.

  Returns `{:ok, stdout}` on success or `{:error, {exit_code, output}}`
  on failure.
  """
  def git_cmd(args, path) when is_list(args) and is_binary(path) do
    case parse_path(path) do
      {:local, dir} ->
        run_exile(["git" | args], cd: dir)

      {:tramp, method, _user, _host, _port, remote_path}
      when method in ["sudo", "su"] ->
        run_exile(["git" | args], cd: remote_path)

      {:tramp, method, user, host, port, remote_path}
      when method in ["ssh", "scp", "rsync", "sshfs"] ->
        ssh_target = if user, do: "#{user}@#{host}", else: host

        ssh_args =
          if port do
            ["-p", port, ssh_target]
          else
            [ssh_target]
          end

        remote_command = "cd #{escape_shell(remote_path)} && git #{escape_args(args)}"
        run_exile(["ssh" | ssh_args ++ [remote_command]])

      {:tramp, method, _, _, _, _} ->
        {:error, {1, "Unsupported TRAMP method: #{method}"}}
    end
  end

  @doc """
  Get the repository root path for a file.

  Supports both local and TRAMP paths.
  """
  def get_repo_path(file_path) do
    file_dir = Path.dirname(to_string(file_path))

    case git_cmd(["rev-parse", "--show-toplevel"], file_dir) do
      {:ok, result} -> {:ok, String.trim(result)}
      {:error, _} -> Exhub.send_message(~s[(message "not a valid git repo")])
    end
  end

  # ==========================================================================
  # Private helpers
  # ==========================================================================

  defp run_exile(argv, opts \\ []) do
    try do
      {stdout, stderr, exit_code} =
        Exile.stream(argv, Keyword.merge([stderr: :consume], opts))
        |> Enum.reduce({"", "", nil}, fn
          {:stdout, data}, {out, err, code} -> {out <> data, err, code}
          {:stderr, data}, {out, err, code} -> {out, err <> data, code}
          {:exit, {:status, code}}, {out, err, _} -> {out, err, code}
          {:exit, :epipe}, {out, err, _} -> {out, err, 0}
          _, acc -> acc
        end)

      exit_code = exit_code || 0

      if exit_code == 0 do
        {:ok, stdout}
      else
        output =
          if stderr != "" do
            "#{stdout}\n#{stderr}" |> String.trim()
          else
            stdout
          end

        {:error, {exit_code, output}}
      end
    rescue
      e -> {:error, {nil, Exception.message(e)}}
    end
  end

  defp escape_shell(path) do
    # Simple shell escaping for the remote cd path
    "'" <> String.replace(path, "'", "'\\''") <> "'"
  end

  defp escape_args(args) do
    Enum.map_join(args, " ", &escape_shell/1)
  end
end
