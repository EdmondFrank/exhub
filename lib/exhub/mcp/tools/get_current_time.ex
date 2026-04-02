defmodule Exhub.MCP.Tools.GetCurrentTime do
  @moduledoc """
  MCP Tool for getting the current time in a specific timezone.

  This tool allows the LLM to get the current datetime, day of week,
  and DST status for any valid IANA timezone.
  """

  alias Anubis.Server.Response

  use Anubis.Server.Component, type: :tool

  def name, do: "get_current_time"

  @impl true
  def description do
    """
    Get current time in a specific timezone. Returns the current datetime, day of week, and DST status for the given IANA timezone name.
    """
  end

  schema do
    field(:timezone, {:required, :string},
      description: "IANA timezone name (e.g., 'America/New_York', 'Europe/London', 'Asia/Tokyo')."
    )
  end

  @impl true
  def execute(params, frame) do
    timezone = Map.get(params, :timezone)

    case DateTime.now(timezone) do
      {:ok, dt} ->
        result = %{
          timezone: timezone,
          datetime: DateTime.to_iso8601(dt),
          day_of_week: Calendar.strftime(dt, "%A"),
          is_dst: dt.std_offset != 0
        }

        resp = Response.tool() |> Response.text(Jason.encode!(result, pretty: true))
        {:reply, resp, frame}

      {:error, _} ->
        error_msg =
          "Error: Invalid timezone '#{timezone}'. Please use a valid IANA timezone name (e.g., 'America/New_York')."

        resp = Response.tool() |> Response.text(error_msg)
        {:reply, resp, frame}
    end
  end
end
