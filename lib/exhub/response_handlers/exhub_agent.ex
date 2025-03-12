defmodule Exhub.ResponseHandlers.ExhubAgent do
  alias Exhub.Llm.World
  alias Exhub.Llm.World.Agent
  require Logger

  def call(["exhub-agent", "list-agents", callback, _args]) do
    {:ok, agents} = World.list_agents
    Logger.info(inspect(agents))
    Exhub.send_message(~s[(#{callback} #{inf_inspect(Jason.encode!(agents))})])
  end

  def call(["exhub-agent", "chat-with-agent", _buffer_file, [agent_name, system_message, user_message, buffer_name]]) do
    {:ok, agents} = World.list_agents
    if !Enum.member?(agents, agent_name) do
      World.create_agent(agent_name, Agent, name: agent_name, system_message: system_message)
    end
    {:ok, reply} = World.send_message(agent_name, user_message)
    Exhub.send_message(~s[(exhub-chat-response 1 #{inf_inspect(reply)} "#{buffer_name}")])
    Exhub.send_message(~s[(exhub-chat-finish-answer "#{buffer_name}")])
  end

  def call(["exhub-agent", "kill-agent", callback, [agent_name]]) do
    World.stop_agent(agent_name)
    Exhub.send_message(~s[(#{callback} "Agent #{agent_name} killed successfully")])
  end

  defp inf_inspect(object) do
    inspect(object, printable_limit: :infinity)
  end
end
