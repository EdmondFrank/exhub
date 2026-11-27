defmodule Exhub.MCP.Tools.Lsp.Ledger do
  @moduledoc """
  MCP Tool: `lsp_ledger` — diagnostics recorded out of band, across sessions.

  Answers "what is broken right now on this machine that I have not looked
  at" from `Exhub.LspBridge.Ledger`, which every session feeds (editor-owned
  ones included). This is the pull side of the proactive LSP surface: nothing
  has to be open, and nothing is lost when a buffer closes.
  """

  alias Exhub.LspBridge.Ledger, as: Store
  alias Exhub.MCP.Tools.Lsp.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "lsp_ledger"

  @impl true
  def description do
    """
    Diagnostics that language servers have reported since ExHub started,
    grouped by file — including files no editor has open any more.

    Every `lsp_*` call and every editor session updates this ledger, so it is
    the cheap way to find out what is currently broken across projects without
    re-diagnosing everything: "what did I break and not fix".

    `root` limits the report to one project (path prefix). `clear` forgets the
    recorded entries (for that root, or everything) — use it after a fix has
    been verified with `lsp_diagnostics`, so the ledger stays meaningful.
    """
  end

  schema do
    field(:root, :string, description: "Only report files under this directory.")

    field(:limit, :integer, description: "Maximum files to report (default 50).", default: 50)

    field(:clear, :boolean,
      description: "Forget recorded entries (for `root`, or all when omitted).",
      default: false
    )

    field(:verbose, :boolean,
      description: "Include the individual error messages, not just counts.",
      default: true
    )
  end

  @impl true
  def execute(params, frame) do
    root = Map.get(params, :root)

    if Map.get(params, :clear, false) do
      m = clear(root)

      Helpers.ok(
        frame,
        "Cleared #{m} ledger entr(ies)" <> if(root, do: " under #{root}.", else: ".")
      )
    else
      report(frame, params)
    end
  end

  defp report(frame, params) do
    root = params |> Map.get(:root) |> normalize_root()
    limit = Map.get(params, :limit) || 50
    verbose = Map.get(params, :verbose, true)

    entries =
      Store.all()
      |> Enum.filter(fn entry -> is_nil(root) or String.starts_with?(entry.path, root) end)
      |> Enum.take(limit)

    summary = Store.summary()

    case entries do
      [] ->
        Helpers.ok(
          frame,
          "Ledger is empty#{if root, do: " for #{root}", else: ""}. " <>
            "Run `lsp_diagnostics` on a file to record its current state."
        )

      entries ->
        header =
          "#{summary.files} file(s) recorded (#{summary.errors} error(s), " <>
            "#{summary.warnings} warning(s)). Showing #{length(entries)}:"

        body = Enum.map_join(entries, "\n\n", &format_entry(&1, verbose))
        Helpers.ok(frame, header <> "\n\n" <> body)
    end
  end

  defp clear(nil) do
    count = length(Store.all())
    Store.reset()
    count
  end

  defp clear(root) do
    prefix = normalize_root(root)

    Store.all()
    |> Enum.filter(fn entry -> String.starts_with?(entry.path, prefix) end)
    |> Enum.map(fn entry ->
      Store.clear(entry.path)
      entry
    end)
    |> length()
  end

  defp normalize_root(nil), do: nil
  defp normalize_root(""), do: nil
  defp normalize_root(root), do: Path.expand(root)

  defp format_entry(entry, verbose) do
    header =
      "#{shorten(entry.path)} — #{entry.errors} error(s), #{entry.warnings} warning(s) " <>
        "[#{age(entry.updated_at)}]"

    if verbose do
      header <> "\n" <> Enum.map_join(entry.diagnostics, "\n", &format_diagnostic/1)
    else
      header
    end
  end

  defp format_diagnostic(diagnostic) do
    start = get_in(diagnostic, ["range", "start"]) || %{}
    source = if diagnostic["source"], do: " [#{diagnostic["source"]}]", else: ""

    "  #{line(start["line"])}:#{line(start["character"])} error#{source} " <>
      "#{String.slice(diagnostic["message"] || "", 0, 400)}"
  end

  defp line(nil), do: "?"
  defp line(position), do: position + 1

  defp age(nil), do: "unknown"

  defp age(timestamp) do
    seconds = System.system_time(:millisecond) - timestamp

    cond do
      seconds < 60_000 -> "#{div(seconds, 1000)}s ago"
      seconds < 3_600_000 -> "#{div(seconds, 60_000)}m ago"
      true -> "#{div(seconds, 3_600_000)}h ago"
    end
  end

  defp shorten(path), do: String.replace_prefix(Path.expand(path), System.user_home!(), "~")
end
