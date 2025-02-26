defmodule Transport do
  @callback start(ctx :: %Context{}) :: {:ok, nil} | {:error, any}
  @callback send(ctx :: %Context{}, message :: %BaseJsonRpcMessage{}) :: {:ok, nil} | {:error, any}
  @callback close() :: {:ok, nil} | {:error, any}
  @callback set_close_handler(handler :: function()) :: {:ok, nil}
  @callback set_error_handler(handler :: function()) :: {:ok, nil}
  @callback set_message_handler(handler :: function()) :: {:ok, nil}

  @spec start(%Context{}) :: {:ok, nil} | {:error, any}
  def start(_ctx), do: {:ok, nil}

  @spec send(%Context{}, %BaseJsonRpcMessage{}) :: {:ok, nil} | {:error, any}
  def send(_ctx, _message), do: {:ok, nil}

  @spec close() :: {:ok, nil} | {:error, any}
  def close(), do: {:ok, nil}

  @spec set_close_handler(function()) :: {:ok, nil}
  def set_close_handler(_handler), do: {:ok, nil}

  @spec set_error_handler(function()) :: {:ok, nil}
  def set_error_handler(_handler), do: {:ok, nil}

  @spec set_message_handler(function()) :: {:ok, nil}
  def set_message_handler(_handler), do: {:ok, nil}
end
