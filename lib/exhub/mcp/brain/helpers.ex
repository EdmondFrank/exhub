defmodule Exhub.MCP.Brain.Helpers do
  @moduledoc """
  Shared helpers for Brain MCP tools (Obsidian vault operations).

  Provides vault path resolution, path validation, tag parsing,
  and TOON-encoded responses for all Brain tools.
  """

  alias Anubis.Server.Response

  @doc """
  Returns the configured Obsidian vault path.
  Falls back to ~/Documents/Obsidian if not configured.
  """
  @spec vault_path() :: String.t()
  def vault_path do
    path =
      Application.get_env(:exhub, :obsidian_vault_path) ||
        Path.join(System.user_home!(), "Documents/Obsidian")

    expand_path(path)
  end

  @doc """
  Resolves `~` and `~/...` paths to the user home directory.
  """
  @spec expand_path(String.t() | nil) :: String.t() | nil
  def expand_path(nil), do: nil
  def expand_path("~"), do: System.user_home!()
  def expand_path("~/" <> rest), do: Path.join(System.user_home!(), rest)
  def expand_path(path), do: path

  @doc """
  Ensures a filename has the .md extension.
  """
  @spec ensure_md(String.t()) :: String.t()
  def ensure_md(filename) do
    if String.ends_with?(filename, ".md"), do: filename, else: filename <> ".md"
  end

  @doc """
  Validates that a resolved path is within the vault root.
  Returns :ok or {:error, reason}.
  """
  @spec validate_in_vault(String.t(), String.t()) :: :ok | {:error, String.t()}
  def validate_in_vault(vault_path, full_path) do
    normalized_vault = Path.expand(vault_path)
    normalized_full = Path.expand(full_path)

    if String.starts_with?(normalized_full, normalized_vault) do
      :ok
    else
      {:error, "Path is outside the vault: #{full_path}"}
    end
  end

  @doc """
  Builds a full path within the vault from an optional folder and filename.
  """
  @spec build_note_path(String.t(), String.t(), String.t() | nil) :: String.t()
  def build_note_path(vault, filename, nil), do: Path.join(vault, ensure_md(filename))
  def build_note_path(vault, filename, folder), do: Path.join([vault, folder, ensure_md(filename)])

  @doc """
  Encode a map as TOON and add it as text content to a tool response.
  Falls back to JSON encoding if TOON encoding fails.
  """
  @spec toon_response(Response.t(), map()) :: Response.t()
  def toon_response(%Response{} = resp, data) when is_map(data) do
    encoded =
      try do
        Toon.encode!(data)
      rescue
        _ -> Jason.encode!(data)
      end

    Response.text(resp, encoded)
  end

  @doc """
  Recursively lists all .md files under a directory.
  Returns a list of relative paths from the vault root.
  """
  @spec list_md_files(String.t(), String.t()) :: [String.t()]
  def list_md_files(vault_path, dir_path) do
    case File.ls(dir_path) do
      {:ok, entries} ->
        Enum.flat_map(entries, fn entry ->
          full = Path.join(dir_path, entry)

          cond do
            File.dir?(full) ->
              list_md_files(vault_path, full)

            String.ends_with?(entry, ".md") ->
              [Path.relative_to(full, vault_path)]

            true ->
              []
          end
        end)

      {:error, _} ->
        []
    end
  end

  @doc """
  Extracts tags from note content (both frontmatter and inline #tags).
  """
  @spec extract_tags(String.t()) :: [String.t()]
  def extract_tags(content) do
    frontmatter_tags = extract_frontmatter_tags(content)
    inline_tags = extract_inline_tags(content)
    Enum.uniq(frontmatter_tags ++ inline_tags)
  end

  defp extract_frontmatter_tags(content) do
    case Regex.run(~r/\A---\n(.*?)\n---/s, content) do
      [_, fm] ->
        case Regex.run(~r/^tags:\s*\[([^\]]*)\]/m, fm) do
          [_, tags_str] ->
            tags_str
            |> String.split(",")
            |> Enum.map(&String.trim/1)
            |> Enum.reject(&(&1 == ""))

          _ ->
            case Regex.scan(~r/^tags:\n((?:\s+-\s+\S+\n?)+)/m, fm) do
              [[_, block]] ->
                Regex.scan(~r/-\s+(\S+)/, block)
                |> Enum.map(fn [_, tag] -> tag end)

              _ ->
                []
            end
        end

      _ ->
        []
    end
  end

  defp extract_inline_tags(content) do
    # Strip frontmatter first
    body =
      case Regex.run(~r/\A---\n.*?\n---\n(.*)/s, content) do
        [_, rest] -> rest
        _ -> content
      end

    Regex.scan(~r/(?:^|\s)#([\w\/]+)/, body)
    |> Enum.map(fn [_, tag] -> tag end)
  end
end
