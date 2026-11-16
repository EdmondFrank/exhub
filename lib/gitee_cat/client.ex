defmodule GiteeCat.Client do
  @moduledoc """
  Connection settings for the Gitee API.

  Besides `auth` and `endpoint`, a client carries two per-client overrides that
  the request pipeline honours:

    * `:user_agent` — the `User-agent` header sent with every request. The
      Gitee Enterprise API gateway only accepts MCP access tokens when this
      contains `"mcp-gitee-ent"`; the default keeps the historical
      `"gitee-cat"` value.
    * `:request_options` — extra HTTPoison/hackney options merged into every
      request, e.g. `Exhub.TLSCompat.httpoison_opts/1` (api.gitee.com serves a
      certificate chain that OTP rejects without it) or `recv_timeout:`.
  """

  @default_endpoint "https://api.gitee.com/"
  @default_user_agent "gitee-cat"

  defstruct auth: nil,
            endpoint: @default_endpoint,
            user_agent: @default_user_agent,
            request_options: []

  @type auth :: %{user: binary, password: binary} | %{access_token: binary} | %{cookies: binary}
  @type t :: %__MODULE__{
          auth: auth | nil,
          endpoint: binary,
          user_agent: binary,
          request_options: keyword
        }

  @spec new() :: t
  def new() do
    config = Application.get_env(:exhub, :gitee_cat, %{})

    new(
      config[:auth],
      config[:endpoint] || @default_endpoint,
      user_agent: config[:user_agent] || @default_user_agent,
      request_options: config[:request_options] || []
    )
  end

  @spec new(binary) :: t
  def new(endpoint) when is_binary(endpoint) do
    new(nil, endpoint)
  end

  @spec new(map()) :: t
  def new(auth = %{user: _, password: _}), do: new(auth, @default_endpoint)

  @spec new(map()) :: t
  def new(auth = %{access_token: _}), do: new(auth, @default_endpoint)

  @spec new(map()) :: t
  def new(auth = %{cookie: _}), do: new(auth, @default_endpoint)

  @spec new(map() | nil, binary) :: t
  def new(auth, endpoint) when is_binary(endpoint) do
    new(auth, endpoint, [])
  end

  @spec new(map() | nil, binary, keyword) :: t
  def new(auth, endpoint, opts) when is_binary(endpoint) and is_list(opts) do
    %__MODULE__{
      auth: auth,
      endpoint: ensure_trailing_slash(endpoint),
      user_agent: Keyword.get(opts, :user_agent, @default_user_agent),
      request_options: Keyword.get(opts, :request_options, [])
    }
  end

  defp ensure_trailing_slash(endpoint) do
    if String.ends_with?(endpoint, "/") do
      endpoint
    else
      endpoint <> "/"
    end
  end
end
