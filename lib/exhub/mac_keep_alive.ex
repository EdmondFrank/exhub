defmodule Exhub.MacKeepAlive do
  @moduledoc """
  Mac Keep Alive module using Quantum scheduler to maintain Bluetooth connections.
  """

  use Quantum, otp_app: :exhub

  require Logger

  @doc """
  Connects to a Bluetooth device by name.
  Returns :ok on success, {:error, reason} on failure.
  """
  def connect_device(device_name) do
    case check_bluetooth_status() do
      :off ->
        Logger.warning("Bluetooth is off, cannot connect to #{device_name}")
        {:error, :bluetooth_off}

      :error ->
        Logger.error("Failed to get Bluetooth status")
        {:error, :bluetooth_status_error}

      :on ->
        find_and_connect_device(device_name)
    end
  end

  defp check_bluetooth_status do
    case System.cmd("blueutil", ["-p"]) do
      {"1\n", 0} -> :on
      {"0\n", 0} -> :off
      _ -> :error
    end
  end

  defp find_and_connect_device(device_name) do
    with {:ok, paired_devices} <- get_paired_devices(),
         {:ok, device} <- find_device_by_name(paired_devices, device_name),
         {:connected, false} <- {:connected, device_connected?(device)} do
      connect_to_device(device)
    else
      {:connected, true} ->
        Logger.info("Device #{device_name} is already connected")
        :already_connected

      {:error, :not_paired} ->
        Logger.warning("Device #{device_name} is not paired")
        {:error, :not_paired}
    end
  end

  defp get_paired_devices do
    case System.cmd("blueutil", ["--format", "json", "--paired"]) do
      {output, 0} ->
        case Jason.decode(output) do
          {:ok, devices} ->
            {:ok, devices}

          {:error, reason} ->
            Logger.error("Failed to parse paired devices: #{inspect(reason)}")
            {:error, :parse_error}
        end

      {error, _} ->
        Logger.error("Failed to get paired devices: #{error}")
        {:error, :cmd_error}
    end
  end

  defp find_device_by_name(devices, device_name) do
    device =
      Enum.find(devices, fn d ->
        Map.get(d, "name") == device_name
      end)

    if device do
      {:ok, device}
    else
      {:error, :not_paired}
    end
  end

  defp device_connected?(device) do
    Map.get(device, "connected", false) == true
  end

  defp connect_to_device(device) do
    address = Map.get(device, "address")

    case System.cmd("blueutil", ["--connect", address]) do
      {_output, 0} ->
        Logger.info("Successfully connected to #{Map.get(device, "name")}")
        :ok

      {error, _} ->
        Logger.error("Failed to connect: #{error}")
        {:error, :connection_failed}
    end
  end

  @doc """
  Runs the keep alive check for a configured device.
  This function is intended to be called by Quantum scheduler.
  """
  def run_keep_alive_check do
    config = Application.get_env(:exhub, __MODULE__, [])
    device_name = Keyword.get(config, :device_name)

    if device_name do
      Logger.info("Running keep alive check for device: #{device_name}")
      connect_device(device_name)
    else
      Logger.warning("No device configured for MacKeepAlive")
      {:error, :no_device_configured}
    end
  end
end
