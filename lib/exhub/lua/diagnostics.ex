defmodule Exhub.Lua.Diagnostics do
  @moduledoc """
  Classifies a Lua sandbox failure into a stable, model-friendly category so the
  caller can react (fix syntax, discover a tool, retry) instead of parsing a raw
  Lua trace. Mirrors AiderDesk's PTC diagnostics.

  Categories: `SyntaxError` (the snippet did not compile), `UnknownTool` (a call
  on a missing tool or global, with a `retrieve_tools` hint), `ToolFailure`
  (`server.tool failed: …`) and `TimeoutExceeded`.
  """

  @doc """
  Turns a sandbox exception into a categorised message.

  `Lua.CompilerException` → `SyntaxError`; any other (runtime) error →
  `UnknownTool` when it is a call on a nil value, else the raw message.
  """
  @spec classify(Exception.t()) :: String.t()
  def classify(%Lua.CompilerException{} = error) do
    error
    |> Exception.message()
    |> String.replace_prefix("Failed to compile Lua!", "")
    |> String.trim()
    |> case do
      "" -> "SyntaxError: code could not be parsed"
      body -> "SyntaxError: " <> body
    end
  end

  def classify(error) do
    error
    |> Exception.message()
    |> String.replace_prefix("Lua runtime error: ", "")
    |> classify_runtime()
  end

  defp classify_runtime(message) do
    case Regex.run(~r/attempt to (?:call|index) a nil value \([^']*'([^']+)'/, message) do
      [_, name] ->
        "UnknownTool: '#{name}' is not available in this sandbox. " <>
          "Use retrieve_tools({query = '...'}) inside the code to find available tools."

      _ ->
        message
    end
  end

  @doc "Normalises a process exit / tool-call failure reason to a message string."
  @spec format_reason(term()) :: String.t()
  def format_reason(reason) when is_binary(reason), do: reason
  def format_reason(%{__exception__: true} = exception), do: Exception.message(exception)
  def format_reason(%{message: message}) when is_binary(message), do: message
  def format_reason(reason), do: inspect(reason)
end
