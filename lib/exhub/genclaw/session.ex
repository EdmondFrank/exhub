defmodule Exhub.Genclaw.Session do
  @moduledoc """
  Session directory management and artifact recording for GenClaw.

  Each agent run gets a unique session directory under the configured runs root.
  Tools write their outputs into per-tool subdirectories within the session.
  """

  require Logger

  @doc """
  Create a new session directory and return its path.
  """
  def create_session_dir do
    root = runs_root()
    session_id = make_session_id()
    dir = Path.join(root, session_id)
    File.mkdir_p!(dir)
    dir
  end

  @doc """
  Get or create the current session directory.
  Uses the process dictionary to store the session dir per-process.
  """
  def get_session_dir do
    case Process.get(:genclaw_session_dir) do
      nil ->
        dir = create_session_dir()
        Process.put(:genclaw_session_dir, dir)
        dir

      dir ->
        File.mkdir_p!(dir)
        dir
    end
  end

  @doc """
  Reset the session directory (for new runs).
  """
  def reset_session_dir do
    Process.delete(:genclaw_session_dir)
    :ok
  end

  @doc """
  Get the output directory for a specific tool within the current session.
  Creates it if it doesn't exist.
  """
  def tool_output_dir(tool_name) do
    session_dir = get_session_dir()
    dir = Path.join(session_dir, tool_name)
    File.mkdir_p!(dir)
    dir
  end

  @doc """
  Generate a unique output file path within a tool's output directory.
  """
  def output_path(tool_name, filename) do
    dir = tool_output_dir(tool_name)
    Path.join(dir, filename)
  end

  @doc """
  Generate a unique PNG output path within a tool's output directory.
  """
  def gen_png_path(tool_name) do
    dir = tool_output_dir(tool_name)
    name = "gen_#{:crypto.strong_rand_bytes(4) |> Base.encode16(case: :lower)}.png"
    Path.join(dir, name)
  end

  @doc """
  Record an artifact in the session's artifacts.jsonl manifest.
  Best-effort — never crashes the caller.
  """
  def record_artifact(path, opts \\ []) do
    tool_name = Keyword.get(opts, :tool_name)
    role = Keyword.get(opts, :role) || infer_role(path)
    kind = Keyword.get(opts, :kind) || infer_kind(path)
    label = Keyword.get(opts, :label) || Path.basename(path)

    entry = %{
      schema_version: 1,
      ts: System.system_time(:millisecond),
      tool_name: tool_name,
      artifact: %{
        path: Path.expand(path),
        kind: kind,
        role: role,
        label: label,
        size_bytes: file_size(path),
        metadata: Keyword.get(opts, :metadata, %{})
      }
    }

    write_artifact_entry(entry)
  rescue
    _ -> :ok
  end

  @doc """
  Record all regular files in a directory as artifacts.
  """
  def record_artifacts_in_dir(dir, opts \\ []) do
    skip = MapSet.new(Keyword.get(opts, :skip_names, ["manifest.json"]))

    case File.ls(dir) do
      {:ok, files} ->
        Enum.each(files, fn name ->
          full = Path.join(dir, name)
          case File.stat(full) do
            {:ok, %{type: :regular}} ->
              unless MapSet.member?(skip, name) or String.starts_with?(name, "._") do
                record_artifact(full, Keyword.put(opts, :out_dir, dir))
              end

            _ ->
              :ok
          end
        end)

      _ ->
        :ok
    end
  end

  defp write_artifact_entry(entry) do
    session_dir = get_session_dir()
    path = Path.join(session_dir, "artifacts.jsonl")

    line = Jason.encode!(entry) <> "\n"
    File.write(path, line, [:append])
  rescue
    _ -> :ok
  end

  defp make_session_id do
    ts = Calendar.strftime(DateTime.utc_now(), "%Y%m%d-%H%M%S")
    rand = :crypto.strong_rand_bytes(3) |> Base.encode16(case: :lower)
    "#{ts}-#{rand}"
  end

  defp runs_root do
    Application.get_env(:exhub, :genclaw_runs_dir) ||
      Path.join(System.user_home!(), ".config/exhub/genclaw_runs")
  end

  defp infer_kind(path) do
    ext = Path.extname(path) |> String.downcase()
    case ext do
      e when e in [".png", ".jpg", ".jpeg", ".webp", ".gif"] -> "image"
      ".svg" -> "svg"
      ".json" -> "json"
      ".html" -> "html"
      e when e in [".txt", ".md"] -> "text"
      _ -> "file"
    end
  end

  defp infer_role(path) do
    name = Path.basename(path) |> String.downcase()
    cond do
      name == "manifest.json" -> "manifest"
      name == "meta.json" -> "metadata"
      name == "final.png" or String.starts_with?(name, "gen_") -> "final"
      String.contains?(name, "draft") -> "draft"
      String.contains?(name, "attempt") or String.contains?(name, "revise") -> "attempt"
      String.contains?(name, "background") -> "background"
      true -> "artifact"
    end
  end

  defp file_size(path) do
    case File.stat(path) do
      {:ok, %{size: size}} -> size
      _ -> 0
    end
  end
end
