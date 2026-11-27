defmodule Exhub.MCP.Tools.Lsp.Status do
  @moduledoc "MCP Tool: `lsp_status` — running language-server sessions."

  alias Exhub.LspBridge.{Config, Headless}
  alias Exhub.MCP.Tools.Lsp.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "lsp_status"

  @impl true
  def description do
    """
    What the language-server manager currently has running, machine-wide.

    Lists each session (project root, server profile, open documents, the
    capabilities the server advertised) plus the diagnostics ledger summary
    and how many servers are configured. Sessions are shared with editors:
    stopping one here also stops it for any editor using the same project.

    `shutdown` takes a file or project path and stops the session(s) that
    serve it — useful when a server wedges on a broken build or before a big
    dependency change. It restarts lazily on the next request.
    """
  end

  schema do
    field(:shutdown, :string,
      description: "Path to a file or project root whose session should be stopped."
    )

    field(:include_configured, :boolean,
      description: "Also list a few configured server names for reference.",
      default: false
    )
  end

  @impl true
  def execute(params, frame) do
    case Map.get(params, :shutdown) do
      path when is_binary(path) and path != "" ->
        case Headless.shutdown(path) do
          {:ok, count} -> Helpers.ok(frame, "Stopped #{count} session(s) for #{path}.")
          {:error, :no_session} -> Helpers.ok(frame, "No session serving #{path}.")
          {:error, reason} -> Helpers.error(frame, "Shutdown failed: #{inspect(reason)}")
        end

      _ ->
        Helpers.ok(frame, render(Headless.status(), Map.get(params, :include_configured, false)))
    end
  end

  defp render(status, include_configured) do
    sessions = status.sessions
    ledger = status.ledger

    header =
      "#{length(sessions)} session(s), #{summary_line(ledger)}, " <>
        "#{length(Config.all())} server config(s)"

    body =
      case sessions do
        [] ->
          "No language server is running. The first `lsp_*` call on a file starts one."

        sessions ->
          Enum.map_join(sessions, "\n\n", &render_session/1)
      end

    configured =
      if include_configured do
        names = Config.all() |> Enum.map(& &1.name) |> Enum.sort() |> Enum.take(40)
        "\n\nConfigured servers (first 40): " <> Enum.join(names, ", ")
      else
        ""
      end

    header <> "\n\n" <> body <> configured
  end

  defp render_session(session) do
    info = session.info || %{}
    documents = Map.get(info, :documents, [])
    servers = Map.get(info, :servers, [])

    capabilities =
      info
      |> Map.get(:capabilities, %{})
      |> Enum.map_join(", ", fn {server, providers} ->
        "#{server}: #{Enum.join(Enum.take(providers, 8), "/")}"
      end)

    lines = [
      "• #{shorten(session.root)} [#{session.profile}]",
      "  servers: #{if servers == [], do: "(none started)", else: Enum.join(servers, ", ")}",
      "  documents: #{Map.get(info, :document_count, length(documents))}" <>
        if(documents == [],
          do: "",
          else: " — " <> Enum.map_join(Enum.take(documents, 3), ", ", &shorten/1)
        ),
      if(capabilities == "", do: nil, else: "  capabilities: #{capabilities}")
    ]

    lines |> Enum.reject(&is_nil/1) |> Enum.join("\n")
  end

  defp summary_line(ledger) do
    "ledger: #{ledger.files} file(s), #{ledger.errors} error(s), #{ledger.warnings} warning(s)"
  end

  defp shorten(path), do: String.replace_prefix(Path.expand(path), System.user_home!(), "~")
end
