defmodule Exhub.MCP.Tools.Lsp.Diagnostics do
  @moduledoc """
  MCP Tool: `lsp_diagnostics` — language-server diagnostics for any file.

  Thin wrapper over `Exhub.LspBridge.Headless.diagnostics/2`; see that module
  for root resolution and document syncing.
  """

  alias Exhub.LspBridge.Headless
  alias Exhub.MCP.Tools.Lsp.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "lsp_diagnostics"

  @impl true
  def description do
    """
    Language-server diagnostics for files in **any** project on this machine:
    each path is resolved to its own project root (git toplevel, then the
    server's build marker), so this is not limited to the current working
    directory.

    Files are re-read from disk first, so the result describes what is
    actually written rather than a stale editor buffer. A file an editor has
    open and modified reports `sync: editor` — its diagnostics reflect that
    buffer and the on-disk copy is not overwritten.

    Use it after edits as a compile/type check, or with `severity: "all"` for
    a full review. Pass `file_path` for one file or `paths` for a batch. The
    first call on a project starts its language server (a second or two for
    large ones, already warm afterwards — the same process an editor would
    use).

    Returns one block per file: `<path> — <n> error(s), <n> diagnostic(s)
    [root: …, servers, sync]` followed by `line:character severity message`
    lines.
    """
  end

  schema do
    field(:file_path, :string,
      description: "Path to one file (absolute, or relative to the server's cwd)."
    )

    field(:paths, {:list, :string},
      description: "Several files to diagnose in one call.",
      default: []
    )

    field(:severity, :string,
      description:
        "Highest severity to include: `error` (default), `warning`, `info`, `hint`, or `all`.",
      default: "error"
    )

    field(:max_per_file, :integer,
      description: "Maximum diagnostics reported per file (default 50).",
      default: 50
    )

    field(:wait_ms, :integer,
      description: "How long to wait for the server's verdict (default 10000).",
      default: 10_000
    )

    field(:server, :string,
      description:
        "Pin a language server by name (e.g. `zls`, `gopls`) instead of the default for the file type."
    )

    field(:project_path, :string,
      description: "Authoritative project root, when the file's git root is not the right one."
    )

    field(:refresh, :boolean,
      description: "Re-read the file from disk before diagnosing (default true).",
      default: true
    )
  end

  @impl true
  def execute(params, frame) do
    case collect_paths(params) do
      [] ->
        Helpers.error(frame, "Provide `file_path` or `paths`.")

      paths ->
        opts = Helpers.opts(params)
        severity = Helpers.severity(Map.get(params, :severity))
        max = Map.get(params, :max_per_file) || 50

        {:ok, entries} = Headless.diagnostics(paths, opts)

        text =
          entries
          |> Enum.map(&render(&1, severity, max))
          |> Enum.join("\n\n")

        Helpers.ok(frame, text)
    end
  end

  defp collect_paths(params) do
    [Map.get(params, :file_path) | List.wrap(Map.get(params, :paths, []))]
    |> Enum.reject(&(is_nil(&1) or &1 == ""))
    |> Enum.uniq()
  end

  defp render(%{error: reason} = entry, _severity, _max) do
    "✗ #{Path.expand(entry.path)}: unavailable (#{reason})"
  end

  defp render(entry, severity, max) do
    diagnostics = entry.diagnostics
    errors = Enum.count(diagnostics, &(Map.get(&1, "severity", 1) == 1))
    filtered = Helpers.filter_diagnostics(diagnostics, severity, max)

    header =
      "#{shorten(entry.path)} — #{errors} error(s), #{length(diagnostics)} diagnostic(s) " <>
        "[root: #{shorten(entry.root)}, #{Enum.join(entry.servers, ",")}, #{entry.sync}]"

    body =
      cond do
        diagnostics == [] -> "  (clean)"
        filtered == [] -> "  (nothing at or above this severity)"
        true -> Enum.map_join(filtered, "\n", &format_diagnostic/1)
      end

    header <> "\n" <> body <> pending_note(entry)
  end

  defp pending_note(entry) do
    if Map.get(entry, :published, true) do
      ""
    else
      "\n  (no verdict from the server within `wait_ms` — it may still be building/analysing; re-run with a larger `wait_ms`)"
    end
  end

  defp format_diagnostic(diagnostic) do
    start = get_in(diagnostic, ["range", "start"]) || %{}
    source = if diagnostic["source"], do: " [#{diagnostic["source"]}]", else: ""

    "  #{line(start["line"])}:#{line(start["character"])} " <>
      "#{severity_name(Map.get(diagnostic, "severity", 1))}#{source} #{diagnostic["message"]}"
  end

  defp severity_name(1), do: "error"
  defp severity_name(2), do: "warning"
  defp severity_name(3), do: "info"
  defp severity_name(_), do: "hint"

  defp line(nil), do: "?"
  defp line(position), do: position + 1

  defp shorten(nil), do: ""
  defp shorten(path), do: String.replace_prefix(Path.expand(path), System.user_home!(), "~")
end
