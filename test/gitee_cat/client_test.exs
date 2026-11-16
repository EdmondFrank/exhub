defmodule GiteeCat.ClientTest do
  use ExUnit.Case, async: true

  alias GiteeCat.Client

  test "keeps the historical defaults" do
    client = Client.new()

    assert client.user_agent == "gitee-cat"
    assert client.request_options == []
    assert client.endpoint == "https://api.gitee.com/"
  end

  test "builds an access-token client with a per-client user agent and options" do
    client =
      Client.new(%{access_token: "t"}, "https://api.gitee.com",
        user_agent: "mcp-gitee-ent",
        request_options: [recv_timeout: 15_000]
      )

    assert client.auth == %{access_token: "t"}
    assert client.endpoint == "https://api.gitee.com/"
    assert client.user_agent == "mcp-gitee-ent"
    assert client.request_options == [recv_timeout: 15_000]
  end

  test "new/1 with an endpoint leaves the auth unset" do
    client = Client.new("https://example.test")

    assert client.auth == nil
    assert client.endpoint == "https://example.test/"
  end
end
