defmodule Exhub.MCP.Desktop.Helpers do
  @moduledoc """
  Shared helpers for Desktop MCP tools.

  Provides TOON-encoded responses to reduce LLM token consumption by 30-60%
  compared to JSON, while maintaining full readability and semantic equivalence.

  ## Usage

      alias Exhub.MCP.Desktop.Helpers
      alias Anubis.Server.Response

      resp =
        Response.tool()
        |> Helpers.toon_response(%{"success" => true, "count" => 42})

      {:reply, resp, frame}
  """

  alias Anubis.Server.Response

  @doc """
  Encode a map as TOON and add it as text content to a tool response.

  Falls back to JSON encoding if TOON encoding fails for any reason.
  """
  @spec toon_response(Response.t(), map()) :: Response.t()
  def toon_response(%Response{} = resp, data) when is_map(data) do
    encoded =
      try do
        Toon.encode!(data)
      rescue
        _ -> Jason.encode!(data)
      end

    Response.text(resp, encoded)
  end

  @doc """
  Resolves `~` and `~/...` paths to the user home directory.

  Passes through absolute and relative paths unchanged. Returns `nil` for `nil`,
  useful for optional path parameters like `working_dir`.
  """
  @spec expand_path(String.t() | nil) :: String.t() | nil
  def expand_path(nil), do: nil
  def expand_path("~"), do: System.user_home!()
  def expand_path("~/" <> rest), do: Path.join(System.user_home!(), rest)
  def expand_path(path), do: path
end
