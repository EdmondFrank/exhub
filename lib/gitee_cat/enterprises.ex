defmodule GiteeCat.Enterprises do
  import GiteeCat
  alias GiteeCat.Client
  @spec list(Client.t(), any, KeyWord.t()) :: GiteeCat.response()
  def list(client, params, options \\ []) do
    get("enterprises/list", client, params, options)
  end
end
