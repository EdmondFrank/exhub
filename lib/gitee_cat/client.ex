defmodule GiteeCat.Client do
  defstruct auth: nil, endpoint: "https://api.gitee.com/"

  @config Application.compile_env(:exhub, :gitee_cat, %{})

  @type auth :: %{user: binary, password: binary} | %{access_token: binary} | %{cookies: binary}
  @type t :: %__MODULE__{auth: auth | nil, endpoint: binary}

  @spec new() :: t
  def new() do
    auth = @config[:auth]
    endpoint = @config[:endpoint] || "https://api.gitee.com/"
    %__MODULE__{auth: auth, endpoint: endpoint}
  end

  @spec new(binary) :: t
  def new(endpoint) when is_binary(endpoint) do
    pnew(nil, endpoint)
  end

  @spec new(map()) :: t
  def new(auth = %{user: _, password: _}), do: %__MODULE__{auth: auth}

  @spec new(map()) :: t
  def new(auth = %{access_token: _}), do: %__MODULE__{auth: auth}

  @spec new(map()) :: t
  def new(auth = %{cookie: _}), do: %__MODULE__{auth: auth}

  @spec new(map(), binary) :: t
  def new(auth = %{access_token: _}, endpoint) do
    pnew(auth, endpoint)
  end

  @spec new(map(), binary) :: t
  def new(auth = %{user: _, password: _}, endpoint) do
    pnew(auth, endpoint)
  end

  @spec new(map(), binary) :: t
  def new(auth = %{cookie: _}, endpoint) do
    pnew(auth, endpoint)
  end

  @spec new(auth, binary) :: t
  defp pnew(auth, endpoint) do
    endpoint =
      if String.ends_with?(endpoint, "/") do
        endpoint
      else
        endpoint <> "/"
      end

    %__MODULE__{auth: auth, endpoint: endpoint}
  end
end
