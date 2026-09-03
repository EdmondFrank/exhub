defmodule Exhub.Fim.ClientTest do
  use ExUnit.Case, async: false

  alias Exhub.Fim.Client

  # Captures the previous :llms / :deepseek_api_key app env so parallel test
  # modules never observe mutated state.
  setup do
    previous_llms = Application.get_env(:exhub, :llms)
    previous_deepseek = Application.get_env(:exhub, :deepseek_api_key)

    on_exit(fn ->
      restore_app_env(:llms, previous_llms)
      restore_app_env(:deepseek_api_key, previous_deepseek)
      System.delete_env("DEEPSEEK_API_KEY")
    end)

    :ok
  end

  defp restore_app_env(key, nil), do: Application.delete_env(:exhub, key)
  defp restore_app_env(key, value), do: Application.put_env(:exhub, key, value)

  describe "build_prompt/1" do
    test "concatenates language-and-tab and before-cursor" do
      context = %{"language-and-tab" => "# language: elixir\n", "before-cursor" => "def foo do"}
      assert Client.build_prompt(context) == "# language: elixir\ndef foo do"
    end

    test "defaults missing keys to empty strings" do
      assert Client.build_prompt(%{}) == "\n"
      assert Client.build_prompt(nil) == "\n"
    end
  end

  describe "build_suffix/1" do
    test "returns after-cursor" do
      assert Client.build_suffix(%{"after-cursor" => "end"}) == "end"
    end

    test "defaults missing key to empty string" do
      assert Client.build_suffix(%{}) == ""
    end
  end

  describe "parse_sse/1" do
    test "extracts choices[0].text from data lines" do
      body = """
      data: {"choices":[{"text":"def "}]}

      data: {"choices":[{"text":"foo"}]}

      data: [DONE]
      """

      assert Client.parse_sse(body) == "def foo"
    end

    test "skips comment/blank lines and malformed JSON" do
      body = "some comment\ndata: {not json}\ndata: {\"choices\":[{\"text\":\"ok\"}]}\n"
      assert Client.parse_sse(body) == "ok"
    end

    test "handles empty and non-binary input" do
      assert Client.parse_sse("") == ""
      assert Client.parse_sse(nil) == ""
      assert Client.parse_sse(%{}) == ""
    end

    test "substring choices text may contain escaped quotes" do
      body = ~s(data: {"choices":[{"text":"say \\"hi\\""}]}\n)
      assert Client.parse_sse(body) == ~s(say "hi")
    end
  end

  describe "provider_config/2" do
    test "codestral defaults" do
      config = Client.provider_config("codestral")
      assert config.endpoint == "https://codestral.mistral.ai/v1/fim/completions"
      assert config.model == "codestral-latest"
      assert config.timeout_ms == 60_000
      assert is_binary(config.api_key) or is_nil(config.api_key)
    end

    test "openai-fim-compatible defaults" do
      config = Client.provider_config("openai-fim-compatible")
      assert config.endpoint == "https://api.deepseek.com/beta/completions"
      assert config.model == "deepseek-chat"
    end

    test "opts override defaults" do
      config =
        Client.provider_config("codestral", %{
          "model" => "m",
          "endpoint" => "http://localhost:1234/v1",
          "api_key" => "explicit",
          "timeout_ms" => 5_000
        })

      assert config.model == "m"
      assert config.endpoint == "http://localhost:1234/v1"
      assert config.api_key == "explicit"
      assert config.timeout_ms == 5_000
    end

    test "ignores invalid timeout_ms and n" do
      config = Client.provider_config("codestral", %{"timeout_ms" => -1, "n" => 0})
      assert config.timeout_ms == 60_000
    end

    test "unknown provider yields empty config without crashing" do
      config = Client.provider_config("bogus")
      assert is_nil(config.endpoint)
      assert is_nil(config.model)
      assert is_nil(config.api_key)
      assert config.timeout_ms == 60_000
    end

    test "resolves llms entry api key for codestral" do
      Application.put_env(:exhub, :llms, %{
        "codestral/codestral-latest" => %{api_key: "llms-key"}
      })

      assert Client.provider_config("codestral").api_key == "llms-key"
    end

    test "falls back to OS env when app env unset" do
      Application.delete_env(:exhub, :llms)
      Application.delete_env(:exhub, :deepseek_api_key)
      System.put_env("DEEPSEEK_API_KEY", "env-key")
      assert Client.provider_config("openai-fim-compatible").api_key == "env-key"
    end

    test "explicit api_key wins over llms" do
      Application.put_env(:exhub, :llms, %{
        "codestral/codestral-latest" => %{api_key: "llms-key"}
      })

      assert Client.provider_config("codestral", %{"api_key" => "explicit"}).api_key == "explicit"
    end
  end

  describe "complete/3" do
    test "returns error without making a request when the API key is missing" do
      Application.delete_env(:exhub, :llms)
      System.delete_env("CODESTRAL_API_KEY")

      assert {:error, message} = Client.complete("codestral", %{"before-cursor" => "x"})
      assert message =~ "API key not configured"
    end
  end
end