defmodule GiteeCat.AttachFiles do
  import GiteeCat
  alias GiteeCat.Client
  @spec upload_with_base64(Client.t(), pos_integer(), map) :: GiteeCat.response()
  def upload_with_base64(client, ent_id, body) when is_map(body) do
    post("enterprises/#{ent_id}/attach_files/upload_with_base_64", client, body)
  end
end
