defmodule Exhub.TLSCompat do
  @moduledoc """
  TLS compatibility options for upstreams whose certificate chains trip
  Erlang/OTP `public_key` >= 1.17 strict KeyUsage / ExtendedKeyUsage validation.

  `api.moark.com` serves a cross-signed "TrustAsia TLS RSA Root CA" whose
  KeyUsage is `[keyCertSign, cRLSign]` while its ExtendedKeyUsage is
  `[serverAuth]`. OTP requires every non-anchor certificate's KeyUsage to be
  compatible with its EKU purpose and aborts the handshake with:

      {:tls_alert, {:unsupported_certificate,
        {:key_usage_mismatch, {key_usage_ext, eku_ext}}}}

  Browsers/openssl/curl do not enforce this cross-check on CA certificates,
  so only BEAM HTTP clients are affected.

  The `verify_fun/0` below accepts *only* this specific error and handles
  every other event with OTP's default semantics — signature verification,
  hostname checking, validity periods and CA constraints remain fully
  enforced.
  """

  @doc """
  Returns a `{fun/3, user_state}` verify_fun for `:ssl` options that overrides
  only the `{:bad_cert, {:key_usage_mismatch, _}}` path-validation error.
  """
  @spec verify_fun() :: {fun(), term()}
  def verify_fun do
    {fn
       # TrustAsia-style cross-signed roots carry CA-only key usages alongside
       # a serverAuth EKU; accept exactly this mismatch, nothing else.
       _cert, {:bad_cert, {:key_usage_mismatch, _}}, state ->
         {:valid, state}

       # Default semantics for everything else (see public_key:pkix_path_validation).
       _cert, {:extension, _ext}, state ->
         {:unknown, state}

       _cert, event, state when event in [:valid, :valid_peer] ->
         {:valid, state}

       _cert, {:bad_cert, reason}, _state ->
         {:fail, reason}
     end, nil}
  end

  @doc """
  HTTPoison options. The `ssl:` key is merged by HTTPoison over its own
  secure defaults (verify_peer + certifi bundle + hostname check).

  `server_name_indication` must be set explicitly: hackney resolves DNS itself
  and dials a raw socket, so OTP cannot infer SNI from a hostname and CDNs
  reject the ClientHello without it.
  """
  @spec httpoison_opts(String.t()) :: keyword()
  def httpoison_opts(url) when is_binary(url) do
    [ssl: [verify_fun: verify_fun()] ++ sni_opts(url)]
  end

  @doc """
  Req options; `connect_options.transport_opts` reach Mint's underlying
  `:ssl` socket (Mint derives SNI from the hostname itself).
  """
  @spec req_opts() :: keyword()
  def req_opts do
    [connect_options: [transport_opts: [verify_fun: verify_fun()]]]
  end

  defp sni_opts(url) do
    case URI.parse(url) do
      %URI{host: host} when is_binary(host) and host != "" ->
        [server_name_indication: String.to_charlist(host)]

      _ ->
        []
    end
  end
end
