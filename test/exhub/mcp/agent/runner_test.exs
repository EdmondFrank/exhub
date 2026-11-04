defmodule Exhub.MCP.Agent.RunnerTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Agent.Runner

  describe "build_mcp_servers/2" do
    test "the bus entry uses the ACP http shape, not the legacy uri key" do
      [bus] = Runner.build_mcp_servers("pi", expose_bus: true)

      assert bus["name"] == "exhub-bus"
      assert bus["type"] == "http"
      assert bus["url"] =~ "/agent-bus/mcp?as=pi"
      # pi dereferences headers unconditionally
      assert bus["headers"] == []
      refute Map.has_key?(bus, "uri")
    end

    test "no servers unless asked" do
      assert Runner.build_mcp_servers("pi") == []
    end

    test "uri is accepted as an alias for url" do
      [server] =
        Runner.build_mcp_servers("pi",
          mcp_servers: [%{"name" => "brain", "uri" => "http://127.0.0.1:9069/brain/mcp"}]
        )

      assert server["type"] == "http"
      assert server["url"] == "http://127.0.0.1:9069/brain/mcp"
      assert server["headers"] == []
    end

    test "maps and lists of headers/env become ACP name/value arrays" do
      [server] =
        Runner.build_mcp_servers("pi",
          mcp_servers: [
            %{
              "name" => "brain",
              "type" => "sse",
              "url" => "http://x/mcp",
              "headers" => %{"A" => "b"}
            }
          ]
        )

      assert server["type"] == "sse"
      assert server["headers"] == [%{"name" => "A", "value" => "b"}]
    end

    test "stdio servers keep command/args/env" do
      [server] =
        Runner.build_mcp_servers("pi",
          mcp_servers: [
            %{
              "name" => "local",
              "command" => "node",
              "args" => ["bridge.js"],
              "env" => [{"K", "V"}]
            }
          ]
        )

      assert server["command"] == "node"
      assert server["args"] == ["bridge.js"]
      assert server["env"] == [%{"name" => "K", "value" => "V"}]
      refute Map.has_key?(server, "type")
    end

    test "entries without a usable transport are ignored" do
      assert Runner.build_mcp_servers("pi", mcp_servers: [%{"name" => "broken"}]) == []
      assert Runner.build_mcp_servers("pi", mcp_servers: [%{"uri" => "http://x"}]) == []
    end
  end
end
