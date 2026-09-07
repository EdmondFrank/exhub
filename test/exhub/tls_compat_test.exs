defmodule Exhub.TLSCompatTest do
  use ExUnit.Case, async: true

  # Mirrors the alert payload OTP generates for api.moark.com's cross-signed
  # TrustAsia root (KeyUsage=[keyCertSign, cRLSign], EKU=[serverAuth]).
  @mismatch {:bad_cert,
             {:key_usage_mismatch,
              {{:Extension, {2, 5, 29, 15}, true, [:keyCertSign, :cRLSign]},
               {:Extension, {2, 5, 29, 37}, false, [{1, 3, 6, 1, 5, 5, 7, 3, 1}]}}}}

  describe "verify_fun/0" do
    test "returns a fun and state tuple" do
      {fun, state} = Exhub.TLSCompat.verify_fun()
      assert is_function(fun, 3)
      assert state == nil
    end

    test "accepts the key_usage_mismatch path-validation error" do
      {fun, state} = Exhub.TLSCompat.verify_fun()
      assert fun.(:some_cert, @mismatch, :state) == {:valid, :state}
    end

    test "delegates extension events to default handling" do
      {fun, _} = Exhub.TLSCompat.verify_fun()
      ext = {:Extension, {2, 5, 29, 31}, false, :whatever}
      assert fun.(:some_cert, {:extension, ext}, :state) == {:unknown, :state}
    end

    test "accepts valid and valid_peer events" do
      {fun, _} = Exhub.TLSCompat.verify_fun()
      assert fun.(:some_cert, :valid, :state) == {:valid, :state}
      assert fun.(:some_cert, :valid_peer, :state) == {:valid, :state}
    end

    test "fails on every other bad_cert reason" do
      {fun, _} = Exhub.TLSCompat.verify_fun()

      assert fun.(:some_cert, {:bad_cert, :unknown_ca}, :state) == {:fail, :unknown_ca}
      assert fun.(:some_cert, {:bad_cert, :expired}, :state) == {:fail, :expired}

      assert fun.(:some_cert, {:bad_cert, :hostname_check_failed}, :state) ==
               {:fail, :hostname_check_failed}
    end
  end

  describe "httpoison_opts/1" do
    test "wraps verify_fun under the ssl key with SNI derived from the URL" do
      opts = Exhub.TLSCompat.httpoison_opts("https://api.moark.com/v1")

      assert [{:ssl, ssl}] = opts
      assert {fun, nil} = Keyword.fetch!(ssl, :verify_fun)
      assert is_function(fun, 3)
      assert ssl[:server_name_indication] == ~c"api.moark.com"
    end

    test "omits SNI when the URL has no host" do
      opts = Exhub.TLSCompat.httpoison_opts("not-a-url")

      assert [ssl: ssl] = opts
      refute Keyword.has_key?(ssl, :server_name_indication)
    end
  end

  describe "req_opts/0" do
    test "nests verify_fun under connect_options.transport_opts" do
      opts = Exhub.TLSCompat.req_opts()
      transport_opts = get_in(opts, [:connect_options, :transport_opts])

      assert {fun, nil} = Keyword.fetch!(transport_opts, :verify_fun)
      assert is_function(fun, 3)
    end
  end

  describe "langchain_req_config/0" do
    test "is a map whose connect_options Req can merge as a keyword list" do
      config = Exhub.TLSCompat.langchain_req_config()

      assert is_map(config)
      # LangChain does `Keyword.new(req_config)` and Req reads connect_options
      # with Keyword functions, so the nested values must be keyword lists.
      assert [transport_opts: transport_opts] = config.connect_options
      assert {fun, nil} = Keyword.fetch!(transport_opts, :verify_fun)
      assert is_function(fun, 3)
    end
  end

  describe "langchain_req_config/1" do
    test "returns the config for providers exposing a req_config field" do
      for provider <- ["openai", "google", "deepseek"] do
        assert Exhub.TLSCompat.langchain_req_config(provider) ==
                 Exhub.TLSCompat.langchain_req_config()
      end
    end

    test "is empty for chat structs that define no req_config field" do
      for provider <- ["anthropic", "mistral"] do
        assert Exhub.TLSCompat.langchain_req_config(provider) == %{}
      end
    end
  end
end
