defmodule GiteeCat.IssueTypes do
  import GiteeCat
  alias GiteeCat.Client
  @spec list(Client.t(), pos_integer(), any, KeyWord.t()) :: GiteeCat.response()
  def list(client, ent_id, params \\ %{}, options \\ []) do
    get("enterprises/#{ent_id}/issue_types", client, params, options)
  end
end
