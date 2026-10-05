defmodule Exhub.LspBridge.Handlers.Locations do
  @moduledoc """
  Normalisation of LSP `Location` / `LocationLink` results.

  Navigation methods may answer with a single `Location`, a list of
  `Location`, or a list of `LocationLink` (`targetUri`/`targetRange`/
  `targetSelectionRange` — volar and others). Elixir port of the shape
  handling in `core/handler/find_define_base.py`, minus the JDT/C#/Deno
  virtual-document resolvers.
  """

  @doc "Normalise any of the location shapes into a list of location maps."
  @spec normalize(term()) :: [map()]
  def normalize(nil), do: []

  def normalize(list) when is_list(list) do
    list |> Enum.map(&one/1) |> Enum.reject(&is_nil/1)
  end

  def normalize(map) when is_map(map), do: normalize([map])
  def normalize(_other), do: []

  defp one(%{"uri" => uri} = loc) do
    range = loc["range"] || %{}

    %{
      "uri" => uri,
      "path" => path(uri),
      "range" => range,
      "selectionRange" => loc["selectionRange"] || range
    }
  end

  defp one(%{"targetUri" => uri} = loc) do
    range = loc["targetRange"] || %{}
    selection = loc["targetSelectionRange"] || range

    %{"uri" => uri, "path" => path(uri), "range" => range, "selectionRange" => selection}
  end

  defp one(_other), do: nil

  # file:// URI -> filesystem path (percent-decoded).
  defp path("file://" <> p), do: URI.decode(p)
  defp path(uri) when is_binary(uri), do: uri
  defp path(_), do: ""
end
