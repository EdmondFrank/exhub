defmodule GiteeCat.Users do
  import GiteeCat
  alias GiteeCat.Client
  @spec me(Client.t()) :: GiteeCat.response()
  def me(client) do
    get("enterprises/users", client)
  end
end
