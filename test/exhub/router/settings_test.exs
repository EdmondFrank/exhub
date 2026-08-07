defmodule Exhub.Router.SettingsTest do
  use ExUnit.Case, async: false

  alias Exhub.Router.Settings

  setup do
    original = System.get_env("EXHUB_ROUTER_CONFIG")

    on_exit(fn ->
      Settings.reload()

      case original do
        nil -> System.delete_env("EXHUB_ROUTER_CONFIG")
        value -> System.put_env("EXHUB_ROUTER_CONFIG", value)
      end
    end)

    :ok
  end

  # Writes `content` to a fresh temp file, points EXHUB_ROUTER_CONFIG at it,
  # and guarantees cleanup. Returns the path.
  defp with_config(content) do
    path =
      Path.join(
        System.tmp_dir!(),
        "exhub_router_cfg_#{System.unique_integer([:positive])}.json"
      )

    File.write!(path, content)
    System.put_env("EXHUB_ROUTER_CONFIG", path)
    on_exit(fn -> File.rm(path) end)
    path
  end

  test "deepseek-* glob matches deepseek-v3 and not kimi-k2.5" do
    with_config(~s({"headers":[{"models":["deepseek-*"],"providers":["openai"],"headers":{"X-package-id":"8848"}}]}))

    assert {"X-package-id", "8848"} in Settings.headers("deepseek-v3", :openai)
    refute {"X-package-id", "8848"} in Settings.headers("kimi-k2.5", :openai)
  end

  test "provider filtering matches openai only" do
    with_config(~s({"headers":[{"models":["deepseek-*"],"providers":["openai"],"headers":{"X-package-id":"8848"}}]}))

    assert {"X-package-id", "8848"} in Settings.headers("deepseek-v3", :openai)
    refute {"X-package-id", "8848"} in Settings.headers("deepseek-v3", :anthropic)
  end

  test "model matching is case-insensitive" do
    with_config(~s({"headers":[{"models":["DEEPSEEK-*"],"headers":{"X-package-id":"8848"}}]}))

    assert {"X-package-id", "8848"} in Settings.headers("DeepSeek-V3", :openai)
  end

  test "omitted model and provider lists are unrestricted" do
    with_config(~s({"headers":[{"headers":{"X-package-id":"8848"}}]}))

    assert {"X-package-id", "8848"} in Settings.headers("anything-model", :anthropic)
  end

  test "missing file returns an empty rule set without raising" do
    System.put_env(
      "EXHUB_ROUTER_CONFIG",
      Path.join(System.tmp_dir!(), "exhub_router_missing_#{System.unique_integer([:positive])}.json")
    )

    assert Settings.headers("deepseek-v3", :openai) == []
  end

  test "malformed JSON returns an empty rule set without raising" do
    with_config("this is not json {{{")

    assert Settings.headers("deepseek-v3", :openai) == []
  end

  test "invalid rule shapes are ignored" do
    with_config(~s({"headers":[{"headers":{"X-ok":"yes"}},{"headers":42},{"not":"a rule"}]}))

    assert {"X-ok", "yes"} in Settings.headers("deepseek-v3", :openai)
  end

  test "environment-selected path takes precedence over the home default" do
    path = with_config(~s({"headers":[{"headers":{"X-env":"1"}}]}))

    assert File.exists?(path)
    assert {"X-env", "1"} in Settings.headers("deepseek-v3", :openai)
  end

  test "credential headers from JSON are never emitted" do
    with_config(~s({"headers":[{"headers":{"authorization":"hacked","X-Api-Key":"hacked","Proxy-Authorization":"hacked","X-package-id":"8848"}}]}))

    headers = Settings.headers("deepseek-v3", :openai)

    refute {"authorization", "hacked"} in headers
    refute {"X-Api-Key", "hacked"} in headers
    refute {"Proxy-Authorization", "hacked"} in headers
    assert {"X-package-id", "8848"} in headers
  end

  test "reload invalidates the cache and the next lookup uses the new file" do
    path = with_config(~s({"headers":[{"headers":{"X-version":"1"}}]}))

    assert {"X-version", "1"} in Settings.headers("deepseek-v3", :openai)

    File.write!(path, ~s({"headers":[{"headers":{"X-version":"2"}}]}))
    Settings.reload()

    headers = Settings.headers("deepseek-v3", :openai)
    assert {"X-version", "2"} in headers
    refute {"X-version", "1"} in headers
  end

  test "file content change is auto-detected via file metadata" do
    path = with_config(~s({"headers":[{"headers":{"X-version":"1"}}]}))

    assert {"X-version", "1"} in Settings.headers("deepseek-v3", :openai)

    # Different size so the metadata cache notices without an explicit reload.
    File.write!(path, ~s({"headers":[{"headers":{"X-version":"22"}}]}))
    headers = Settings.headers("deepseek-v3", :openai)
    assert {"X-version", "22"} in headers
    refute {"X-version", "1"} in headers
  end

  test "duplicate custom header names merge case-insensitively, later rule wins" do
    with_config(~s({"headers":[{"headers":{"X-package-id":"1"}},{"headers":{"x-package-id":"2"}}]}))

    headers = Settings.headers("deepseek-v3", :openai)
    assert {"x-package-id", "2"} in headers
    refute {"X-package-id", "1"} in headers
    assert length(headers) == 1
  end
end