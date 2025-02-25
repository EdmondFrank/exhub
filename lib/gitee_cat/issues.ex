defmodule GiteeCat.Issues do
  import GiteeCat
  alias GiteeCat.Client
  @spec list(Client.t(), pos_integer(), any, KeyWord.t()) :: GiteeCat.response()
  def list(client, ent_id, params, options \\ []) do
    get("enterprises/#{ent_id}/issues", client, params, options)
  end

  @spec create(Client.t(), pos_integer(), map) :: GiteeCat.response()
  def create(client, ent_id, body) when is_map(body) do
    post("enterprises/#{ent_id}/issues", client, body)
  end

  @spec children(Client.t(), pos_integer(), pos_integer(), any, KeyWord.t()) :: GiteeCat.response()
  def children(client, ent_id, issue_id, params \\ %{}, options \\ []) do
    get("enterprises/#{ent_id}/issues/#{issue_id}/children", client, params, options)
  end
end
