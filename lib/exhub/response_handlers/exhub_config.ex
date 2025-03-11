defmodule Exhub.ResponseHandlers.ExhubConfig do
  alias Exhub.Llm.LlmConfigServer
  def call(["exhub-config", "switch-model", callback]) do
    llm_names = LlmConfigServer.list_llm_names()
    Exhub.send_message(~s[(#{callback} #{inf_inspect(Jason.encode!(llm_names))})])
  end

  def call(["exhub-config", "set-model", llm_name]) do
    LlmConfigServer.set_default_llm_name(llm_name)
    Exhub.send_message(~s[(message "Model has successfully changed to #{llm_name}")])
  end

  def call(["exhub-config", "current-model", callback]) do
    llm_name = LlmConfigServer.get_default_llm_name
    Exhub.send_message(~s[(#{callback} "#{llm_name}")])
  end

  defp inf_inspect(object) do
    inspect(object, printable_limit: :infinity)
  end
end
