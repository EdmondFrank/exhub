# exhub-keep-alive

The `exhub-keep-alive` module provides automatic Bluetooth connection maintenance for macOS using the Quantum scheduler.

## Setup

The keep-alive feature is built into the Exhub application. No additional Emacs configuration is required.

## Configuration

Configure the keep-alive feature in `config/config.exs`:

```elixir
config :exhub, Exhub.MacKeepAlive,
  device_name: "Your Bluetooth Device Name",
  jobs: [
    {"*/5 * * * *", {Exhub.MacKeepAlive, :run_keep_alive_check, []}}
  ]
```

### Configuration Options

- `device_name`: The name of your Bluetooth device to connect to (must be paired first)
- `jobs`: Quantum scheduler jobs (cron syntax). Default runs every 5 minutes.

## Prerequisites

- **macOS**: This feature uses macOS-specific Bluetooth commands
- **blueutil**: Install via Homebrew: `brew install blueutil`
- **Paired Device**: The Bluetooth device must be already paired with your Mac

## Usage

The keep-alive feature runs automatically based on the configured schedule.

## Troubleshooting

- If Bluetooth is off, the connection will fail with `{:error, :bluetooth_off}`
- If the device is not paired, the connection will fail with `{:error, :not_paired}`
- Check server logs for detailed connection status information
