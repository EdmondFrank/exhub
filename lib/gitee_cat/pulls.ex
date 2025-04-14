defmodule GiteeCat.Pulls do
  import GiteeCat
  alias GiteeCat.Client
  @spec list(Client.t(), pos_integer(), any, KeyWord.t()) :: GiteeCat.response()
  def list(client, ent_id, params, options \\ []) do
    get("enterprises/#{ent_id}/pull_requests", client, params, options)
  end

  @spec raw_diff(Client.t(), pos_integer(), pos_integer(), pos_integer(), KeyWord.t()) :: GiteeCat.response()
  def raw_diff(client, ent_id, project_id, pull_id, options \\ []) do
    get("enterprises/#{ent_id}/projects/#{project_id}/pull_requests/#{pull_id}/diff", client, options)
  end
end
