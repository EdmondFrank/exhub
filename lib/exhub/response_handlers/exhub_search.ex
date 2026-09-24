defmodule Exhub.ResponseHandlers.ExhubSearch do
  @moduledoc """
  WebSocket response handler for `exhub-probe` search commands from Emacs.

  Dispatches `["func", ["exhub-search", action, req_id, params]]` messages to
  `Exhub.MCP.Hub.BuiltInRegistry.call_tool("desktop", "search_files", params)`,
  which runs the built-in `desktop` server in the same BEAM VM (no HTTP or
  session handshake). The result is pushed back to Emacs as evaluable elisp:

      (exhub-probe--receive REQ-ID IS-ERROR JSON)

  `IS-ERROR` is `t` or `nil`; `JSON` is a single-line JSON object carrying the
  raw tool text (and, for glob/content, the TOON-decoded structured results).

  ## Actions

  - `"search"` — semantic (probe) search, Smart Decide filtering on by default
  - `"glob"` — match file/directory paths against a glob pattern
  - `"content"` — literal/regex content search

  The registry call happens inside the handler `Task` started by
  `Exhub.SocketHandler`, so a multi-second probe run never blocks the Cowboy
  process. `Exhub.MCP.Hub.ClientManager` is deliberately not used: it is a
  GenServer that would serialize the whole search behind one call.
  """

  require Logger

  alias Exhub.BlinkSearch.Backend
  alias Exhub.MCP.Hub.BuiltInRegistry

  @actions %{"search" => "semantic", "glob" => "glob", "content" => "content"}

  @doc """
  Handles `["exhub-search", action, req_id, params]` messages.

  Never raises: every path replies to Emacs (success or error).
  """
  def call(["exhub-search", action, req_id, params]) do
    result =
      with {:ok, search_type} <- normalize_action(action) do
        run(search_type, params)
      end

    reply(req_id, result)
    nil
  end

  def call(["exhub-search", action | _rest]) do
    Logger.warning("[ExhubSearch] Malformed message for action: #{inspect(action)}")
    nil
  end

  def call(args) do
    Logger.warning("[ExhubSearch] Unknown message: #{inspect(args)}")
    nil
  end

  # --- private ---

  defp normalize_action(action) when is_binary(action) do
    case Map.fetch(@actions, action) do
      {:ok, search_type} ->
        {:ok, search_type}

      :error ->
        {:error, "Unknown search action: #{action}. Use \"search\", \"glob\" or \"content\"."}
    end
  end

  defp normalize_action(action), do: {:error, "Invalid search action: #{inspect(action)}"}

  # `search_files` validates string keys via its Peri schema (matching the other
  # `BuiltInRegistry` callers), so the decoded JSON map is forwarded as-is with
  # the search type and filter default applied.
  defp run(search_type, params) when is_map(params) do
    params =
      params
      |> Map.put("search_type", search_type)
      |> default_filter(search_type)

    case BuiltInRegistry.call_tool("desktop", "search_files", params) do
      {:ok, %{"isError" => true} = response} ->
        {:error, error_text(response)}

      {:ok, %{} = response} ->
        {:ok, %{"text" => content_text(response), "data" => structured(search_type, response)}}

      {:error, reason} ->
        {:error, reason_text(reason)}

      other ->
        {:error, "Unexpected search_files response: #{inspect(other)}"}
    end
  rescue
    error -> {:error, "search_files raised: #{Exception.message(error)}"}
  catch
    kind, value -> {:error, "search_files failed: #{inspect({kind, value})}"}
  end

  defp run(_search_type, params), do: {:error, "Invalid search parameters: #{inspect(params)}"}

  # Always send `filter` for semantic searches: the frontend must not depend on
  # the server-side default (it is off in `config/test.exs`).
  defp default_filter(params, "semantic"), do: Map.put_new(params, "filter", true)
  defp default_filter(params, _search_type), do: params

  defp content_text(%{"content" => content}) when is_list(content) do
    content
    |> Enum.map(fn
      %{"text" => text} -> text
      %{text: text} -> text
      text when is_binary(text) -> text
      _ -> ""
    end)
    |> Enum.join("\n")
  end

  defp content_text(other), do: inspect(other)

  # glob/content results are returned as TOON by the tool (`Helpers.toon_response`).
  # Decoding them here lets Emacs render structured results instead of parsing
  # TOON itself. Semantic results are plain text (the `Pattern:`/`---`/`File:`
  # format), which the frontend parses.
  defp structured(search_type, response) when search_type in ["glob", "content"] do
    text = content_text(response)

    try do
      Toon.decode!(text)
    rescue
      _ -> nil
    end
  end

  defp structured(_search_type, _response), do: nil

  defp error_text(response) do
    case content_text(response) do
      "" -> "search_files returned an error"
      text -> text
    end
  end

  defp reason_text(reason) when is_binary(reason), do: reason

  defp reason_text(%{message: message} = reason) do
    base = if is_binary(message), do: message, else: inspect(message)

    case Map.get(reason, :errors) do
      nil -> base
      errors -> base <> ": " <> inspect(errors)
    end
  end

  defp reason_text(%{__struct__: _} = error) do
    Exception.message(error)
  rescue
    _ -> inspect(error)
  end

  defp reason_text(reason), do: inspect(reason)

  defp reply(req_id, result) do
    {is_error, payload} =
      case result do
        {:ok, %{"text" => text, "data" => data}} ->
          {false,
           %{"ok" => true, "text" => text, "data" => data, "error" => nil, "reqId" => req_id}}

        {:error, message} ->
          {true,
           %{
             "ok" => false,
             "text" => nil,
             "data" => nil,
             "error" => stringify(message),
             "reqId" => req_id
           }}
      end

    json = Jason.encode!(payload)

    Exhub.send_message(
      "(exhub-probe--receive #{req_token(req_id)} #{bool(is_error)} #{Backend.elisp_quote(json)})"
    )
  rescue
    error ->
      Logger.error("[ExhubSearch] Failed to reply (#{inspect(req_id)}): #{inspect(error)}")
      :ok
  end

  defp req_token(req_id) when is_integer(req_id), do: Integer.to_string(req_id)
  defp req_token(req_id), do: Backend.elisp_quote(to_string(req_id))

  defp stringify(message) when is_binary(message), do: message
  defp stringify(message), do: inspect(message)

  defp bool(true), do: "t"
  defp bool(false), do: "nil"
end
