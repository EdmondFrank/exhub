defmodule Exhub.SocketHandler do
  @behaviour :cowboy_websocket
  require Logger

  def init(req, state) do
    {:cowboy_websocket, req, state}
  end

  def websocket_init(state) do
    result = Registry.register(Exhub.Registry, "socket_handler", :socket_handler)
    Logger.debug("Subscribing socket handler (#{inspect result}")
    schedule_ping(state)
    {:ok, state}
  end

  def websocket_handle({:text, "exhub-pong"}, state) do
    schedule_ping(state)
    {:reply, {:text, "nil"}, state}
  end

  def websocket_handle({:text, "exhub-ping"}, state) do
    {:reply, {:text, "(exhub-pong)"}, state}
  end

  def websocket_handle({:text, "exhub-reload"}, state) do
    Task.start(fn ->
      summary = Exhub.HotReload.reload_and_summarize()
      escaped = String.replace(summary, "\"", "\\\"")
      Exhub.send_message("(message \"[Exhub] #{escaped}\")")
    end)

    {:reply, {:text, "(message \"[Exhub] Hot reload started...\")"}, state}
  end

  def websocket_handle({:text, "exhub-reload-keys"}, state) do
    Task.start(fn ->
      case Exhub.Router.Config.reload_from_scr() do
        :ok ->
          Exhub.send_message("(message \"[Exhub] API keys reloaded from SecretVault successfully.\")")

        {:error, reason} ->
          Exhub.send_message("(message \"[Exhub] Failed to reload API keys: #{inspect(reason)}\")")
      end
    end)

    {:reply, {:text, "(message \"[Exhub] API key reload from SecretVault started...\")"}, state}
  end

  def websocket_handle({:text, message}, state) do
    Logger.debug("Received message #{inspect message}")
    dispatch_message(message)
    # Check if this is an emacs_response message
    case parse_emacs_response(message) do
      {:ok, request_id, response} ->
        dispatch_emacs_response(request_id, response)
        {:reply, {:text, "nil"}, state}
      :not_response ->
        case response_handler().call(message) do
          response when is_binary(response) -> {:reply, {:text, response}, state}
          nil -> {:reply, {:text, "nil"}, state}
        end
    end
  end

  def websocket_info(:ping, state) do
    {:reply, {:text, "(exhub--pong)"}, state}
  end

  def websocket_info({:send_to_emacs, message}, state) do
    Logger.debug("[exhub] sending message to emacs \"#{message}\"")
    {:reply, {:text, message}, state}
  end

  def terminate(reason, _req, _state) do
    Logger.debug("Terminating websocket handler because of #{inspect reason}")
  end

  defp dispatch_message(message) do
    Registry.dispatch(Exhub.Registry, "consumer", fn entries ->
      for {pid, _mode} <- entries do
        send(pid, {:message, message})
      end
    end)
  end

  defp parse_emacs_response(message) do
    case Jason.decode(message) do
      {:ok, ["emacs_response", request_id, response]} ->
        {:ok, request_id, response}
      _ ->
        :not_response
    end
  rescue
    _ -> :not_response
  end

  defp dispatch_emacs_response(request_id, response) do
    Registry.dispatch(Exhub.Registry, "emacs_response_#{request_id}", fn entries ->
      for {pid, _mode} <- entries do
        send(pid, {:emacs_response, request_id, response})
      end
    end)
  end

  @ping_interval 30_000
  defp schedule_ping(_) do
    Process.send_after(self(), :ping, @ping_interval)
  end

  defp response_handler do
    Application.get_env(:exhub, :response_handler, Exhub.DefaultResponseHandler)
  end
end
