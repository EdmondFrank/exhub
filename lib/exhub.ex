defmodule Exhub do
  @moduledoc """
  Documentation for `Exhub`.
  """
  require Logger

  def subscribe do
    Registry.register(Exhub.Registry, "consumer", :consumer)
  end

  def get_message do
    receive do
      {:message, message} -> message
    end
  end

  def send_message(message) do
    Registry.dispatch(Exhub.Registry, "socket_handler", fn entries ->
      for {pid, _mode} <- entries do
        send(pid, {:send_to_emacs, message})
      end
    end)
  end
end
