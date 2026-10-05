defmodule Exhub.LspBridge.Elisp do
  @moduledoc """
  Helpers for rendering LSP results as elisp forms pushed to Emacs.

  ExHub pushes results to Emacs as elisp expressions that the client `eval`s
  (`exhub-eval`). Values that are not trivially representable as elisp (maps,
  lists, arbitrary JSON) are JSON-encoded and passed as **escaped string
  literals** — the elisp side parses them with `json-parse-string` — rather
  than interpolated raw, which would not be valid elisp at all.

  Used by both `Exhub.LspBridge.Session` (diagnostics) and
  `Exhub.LspBridge.ClientManager` (readiness, responses, errors).
  """

  @doc "A double-quoted, escaped elisp string literal."
  @spec string(String.t()) :: String.t()
  def string(s) when is_binary(s) do
    escaped =
      s
      |> String.replace("\\", "\\\\")
      |> String.replace("\"", "\\\"")

    "\"" <> escaped <> "\""
  end

  @doc "A quoted elisp string holding the JSON encoding of `term`."
  @spec json(term()) :: String.t()
  def json(term), do: term |> Jason.encode!() |> string()

  @doc "Build a `(name arg ...)` elisp form from already-rendered arguments."
  @spec form(String.t(), [String.t()]) :: String.t()
  def form(name, args \\ []) do
    "(" <> Enum.join([name | args], " ") <> ")"
  end
end
