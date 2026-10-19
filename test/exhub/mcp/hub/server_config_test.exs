defmodule Exhub.MCP.Hub.ServerConfigTest do
  use ExUnit.Case, async: false

  alias Exhub.MCP.Hub.ServerConfig

  @token_var "EXHUB_SERVER_CONFIG_TEST_TOKEN"

  setup do
    System.delete_env(@token_var)
    on_exit(fn -> System.delete_env(@token_var) end)
    :ok
  end

  describe "to_anubis_client_opts/1 with ${VAR} templates" do
    test "expands stdio env values and args" do
      System.put_env(@token_var, "tok-123")

      config =
        ServerConfig.from_json(%{
          "name" => "github",
          "transport" => "stdio",
          "command" => "npx",
          "args" => ["-y", "srv", "${#{@token_var}}"],
          "env" => %{"TOKEN" => "${#{@token_var}}"}
        })

      {:stdio, stdio_opts} = ServerConfig.to_anubis_client_opts(config)[:transport]

      assert stdio_opts[:env] == %{"TOKEN" => "tok-123"}
      assert stdio_opts[:args] == ["-y", "srv", "tok-123"]
    end

    test "expands remote transport headers" do
      System.put_env(@token_var, "tok-123")

      config =
        ServerConfig.from_json(%{
          "name" => "remote",
          "transport" => "streamable_http",
          "url" => "https://example.com/mcp",
          "headers" => %{"Authorization" => "Bearer ${#{@token_var}}"}
        })

      {:streamable_http, http_opts} = ServerConfig.to_anubis_client_opts(config)[:transport]

      assert http_opts[:headers] == %{"Authorization" => "Bearer tok-123"}
    end

    test "does not mutate the stored config, so templates are what gets persisted" do
      System.put_env(@token_var, "tok-123")

      config =
        ServerConfig.from_json(%{
          "name" => "github",
          "transport" => "stdio",
          "command" => "npx",
          "env" => %{"TOKEN" => "${#{@token_var}}"}
        })

      _opts = ServerConfig.to_anubis_client_opts(config)

      assert config.env == %{"TOKEN" => "${#{@token_var}}"}
      assert ServerConfig.to_json(config)["env"] == %{"TOKEN" => "${#{@token_var}}"}
    end
  end
end
