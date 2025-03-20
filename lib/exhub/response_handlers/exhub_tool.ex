defmodule Exhub.ResponseHandlers.ExhubTool do
  alias Exhub.Utils
  alias Exhub.Llm.Chat
  alias Exhub.Llm.Mcp.ServerManager
  alias Exhub.Llm.Mcp.ClientManager

  def call(["exhub-tool", "start-server", callback, [unique_name, command | args]]) do
    with  server_name <- String.to_atom(unique_name),
         {:ok, _pid} <- ServerManager.new(server_name, command, args),
         {:ok, _pid} <- ClientManager.new(:"#{server_name}_client", server_name) do
      Exhub.send_message(~s[(#{callback} "#{unique_name}")])
    end
  end

  def call(["exhub-tool", "get-repo-path", callback, [file_path]]) do
    with {:ok, repo_dir} <- Utils.get_repo_path(file_path) do
      Exhub.send_message(~s[(#{callback} "#{repo_dir}")])
    end
  end

  def call(["exhub-tool", "chat-with-tool", buffer_file, [unique_name, system_message, user_message, buffer_name]]) do
    with {:ok,repo_path} <- Utils.get_repo_path(buffer_file),
         {:ok, reply} <- Chat.execute(system_message, user_message, unique_name, %{"repo_path" => repo_path}) do
      Exhub.send_message(~s[(exhub-chat-response 1 #{inf_inspect(reply)} "#{buffer_name}")])
      Exhub.send_message(~s[(exhub-chat-finish-answer "#{buffer_name}")])
    end
  end

  def call(["exhub-tool", "stop-server", callback, [unique_name]]) do
    with server_name <- String.to_atom(unique_name),
         :ok <- ServerManager.kill(server_name),
         :ok <- ClientManager.kill(:"#{server_name}_client") do
      Exhub.send_message(~s[(#{callback} "#{unique_name} stopped")])
    end
  end

  defp inf_inspect(object) do
    inspect(object, printable_limit: :infinity)
  end
end
