defmodule Exhub.ResponseHandlers.ExhubGitee do
  def call(["exhub-gitee", module, func, callback, args]) do
    with {200, body, _response} <- apply(Module.concat(GiteeCat, module), String.to_existing_atom(func), [GiteeCat.Client.new() | args]) do
      Exhub.send_message(~s[(#{callback} #{inf_inspect(Jason.encode!(body))})])
    end
  end

  defp inf_inspect(object) do
    inspect(object, printable_limit: :infinity)
  end
end
