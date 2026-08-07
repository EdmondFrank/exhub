defmodule Exhub.Router.ConfigTest do
  use ExUnit.Case, async: false

  alias Exhub.Router.Config
  alias Exhub.Router.Settings

  @deepseek_config ~s({"headers":[{"models":["deepseek-*"],"providers":["openai"],"headers":{"X-package-id":"8848"}}]})

  setup do
    original = System.get_env("EXHUB_ROUTER_CONFIG")

    path =
      Path.join(
        System.tmp_dir!(),
        "exhub_router_cfg_#{System.unique_integer([:positive])}.json"
      )

    File.write!(path, @deepseek_config)
    System.put_env("EXHUB_ROUTER_CONFIG", path)

    on_exit(fn ->
      Settings.reload()
      File.rm(path)

      case original do
        nil -> System.delete_env("EXHUB_ROUTER_CONFIG")
        value -> System.put_env("EXHUB_ROUTER_CONFIG", value)
      end
    end)

    %{config_path: path}
  end

  test "adds the package id header for DeepSeek OpenAI models" do
    headers = Config.get_auth_headers("deepseek-v3", :openai)

    assert {"X-package-id", "8848"} in headers
  end

  test "does not add the DeepSeek package id header for other models" do
    headers = Config.get_auth_headers("kimi-k2.5", :openai)

    refute {"X-package-id", "8848"} in headers
  end

  test "does not add the DeepSeek package id header for other providers" do
    headers = Config.get_auth_headers("deepseek-v3", :anthropic)

    refute {"X-package-id", "8848"} in headers
  end

  test "configured values replace duplicate non-sensitive base header names", %{config_path: path} do
    File.write!(
      path,
      ~s({"headers":[{"models":["deepseek-v3"],"headers":{"X-Failover-Enabled":"false"}}]})
    )
    Settings.reload()

    headers = Config.get_auth_headers("deepseek-v3", :openai)
    assert {"X-Failover-Enabled", "false"} in headers
    refute {"X-Failover-Enabled", "true"} in headers
  end

  test "configured credential headers cannot override Exhub authorization" do
    path =
      Path.join(
        System.tmp_dir!(),
        "exhub_router_cfg_#{System.unique_integer([:positive])}.json"
      )

    File.write!(
      path,
      ~s({"headers":[{"headers":{"authorization":"attacker","X-Api-Key":"attacker","X-package-id":"8848"}}]})
    )
    System.put_env("EXHUB_ROUTER_CONFIG", path)
    Settings.reload()

    headers = Config.get_auth_headers("deepseek-v3", :openai)

    refute {"authorization", "attacker"} in headers
    refute {"X-Api-Key", "attacker"} in headers
    assert {"X-package-id", "8848"} in headers

    # The real authorization header from Exhub is still present.
    assert Enum.any?(headers, fn
             {"authorization", value} -> String.starts_with?(value, "Bearer ")
             _ -> false
           end)
  end

  test "reload after editing the file makes the next lookup use the new value" do
    path =
      Path.join(
        System.tmp_dir!(),
        "exhub_router_cfg_#{System.unique_integer([:positive])}.json"
      )

    File.write!(path, ~s({"headers":[{"headers":{"X-version":"1"}}]}))
    System.put_env("EXHUB_ROUTER_CONFIG", path)
    Settings.reload()

    assert {"X-version", "1"} in Config.get_auth_headers("deepseek-v3", :openai)

    File.write!(path, ~s({"headers":[{"headers":{"X-version":"2"}}]}))
    Settings.reload()

    headers = Config.get_auth_headers("deepseek-v3", :openai)
    assert {"X-version", "2"} in headers
    refute {"X-version", "1"} in headers
  end

  test "malformed router settings fall back to base auth headers safely" do
    path =
      Path.join(
        System.tmp_dir!(),
        "exhub_router_cfg_#{System.unique_integer([:positive])}.json"
      )

    File.write!(path, "not json {{{")
    System.put_env("EXHUB_ROUTER_CONFIG", path)
    Settings.reload()

    headers = Config.get_auth_headers("deepseek-v3", :openai)
    refute {"X-package-id", "8848"} in headers
    assert Enum.any?(headers, fn
             {"authorization", value} -> String.starts_with?(value, "Bearer ")
             _ -> false
           end)
  end
end