defmodule Exhub.ResponseHandlers.ExhubAgent do
  @moduledoc """
  Emacs-facing agent surface, backed by the Agent Hub (`Exhub.Sagents.Hub`).

  The Emacs client (`exhub-agent.el`) picks one of the registered Sagents agent
  profiles and chats with it. Conversations live in the Agent Hub store, so the
  same thread is also visible in the browser at `/agent-hub`.

  Protocol (elisp -> server, via `Exhub.DefaultResponseHandler`):

    * `(exhub-agent list-agents CALLBACK)` - reply with a JSON array of the
      registered profile names
    * `(exhub-agent chat-with-agent FILE [AGENT MESSAGE BUFFER])` - one turn,
      rendered into BUFFER through the `exhub-chat-response` callbacks
    * `(exhub-agent start-agent CALLBACK [AGENT])` - boot the profile's agent
    * `(exhub-agent kill-agent CALLBACK [AGENT])` - stop every running
      conversation of that profile
  """

  alias Exhub.Sagents.Hub

  @max_error_chars 300

  def call(["exhub-agent", "list-agents", callback, _args]) do
    Exhub.send_message(~s[(#{callback} #{inf_inspect(Jason.encode!(agent_names()))})])
  end

  def call(["exhub-agent", "chat-with-agent", _buffer_file, [agent_name, message, buffer_name]]) do
    case chat(agent_name, message) do
      {:ok, reply} ->
        Exhub.send_message(~s[(exhub-chat-response 1 #{inf_inspect(reply)} "#{buffer_name}")])
        Exhub.send_message(~s[(exhub-chat-finish-answer "#{buffer_name}")])

      {:error, :not_found} ->
        notify("No such agent '#{agent_name}'. Available: #{Enum.join(agent_names(), ", ")}")

      {:error, reason} ->
        notify("Agent '#{agent_name}' failed: " <> short_reason(reason))
    end
  end

  def call(["exhub-agent", "start-agent", callback, [agent_name]]) do
    result =
      case Hub.start_agent(agent_name) do
        {:ok, _pid} -> "Agent #{agent_name} started"
        {:error, :not_found} -> "No such agent '#{agent_name}'"
        {:error, reason} -> "Failed to start '#{agent_name}': " <> short_reason(reason)
      end

    Exhub.send_message(~s[(#{callback} #{inf_inspect(result)})])
  end

  def call(["exhub-agent", "kill-agent", callback, [agent_name]]) do
    Hub.stop_agent(agent_name)
    Exhub.send_message(~s[(#{callback} #{inf_inspect("Agent #{agent_name} stopped")})])
  end

  # `Hub.chat/2` holds the caller for the whole turn and can exit with the
  # agent's process; the socket handler runs response handlers in a Task, so a
  # failure here must surface as a message, never as a dead socket.
  defp chat(agent_name, message) do
    Hub.chat(agent_name, message)
  rescue
    error -> {:error, error}
  catch
    :exit, reason -> {:error, reason}
  end

  defp agent_names do
    Hub.list_agents() |> Enum.map(& &1.name)
  end

  defp short_reason(%{message: message}) when is_binary(message) and message != "" do
    String.slice(message, 0, @max_error_chars)
  end

  defp short_reason(reason) do
    reason
    |> inspect(printable_limit: @max_error_chars)
    |> String.slice(0, @max_error_chars)
  end

  defp inf_inspect(object) do
    inspect(object, printable_limit: :infinity)
  end

  defp notify(msg) do
    Exhub.send_message(~s[(message #{inf_inspect(msg)})])
  end
end
