defmodule Exhub.MCP.Tools.ConvertTime do
  @moduledoc """
  MCP Tool for converting time between timezones.

  Converts a given HH:MM time from a source IANA timezone to a target IANA timezone,
  returning full datetime info and the time difference.
  """

  alias Anubis.Server.Response

  use Anubis.Server.Component, type: :tool

  def name, do: "convert_time"

  @impl true
  def description do
    """
    Convert time between timezones. Provide either 'datetime' (full ISO8601 datetime string) or 'time' (HH:MM 24-hour format, uses current date), along with source and target timezones to get the converted time along with the time difference.
    """
  end

  schema do
    field(:source_timezone, {:required, :string},
      description: "Source IANA timezone name (e.g., 'America/New_York', 'Europe/London')."
    )

    field(:datetime, :string,
      description: "ISO8601 datetime string to convert (e.g., '2024-01-15T14:30:00'). If provided, 'time' field is ignored."
    )

    field(:time, :string,
      description: "Time to convert in 24-hour format (HH:MM). Used if 'datetime' is not provided."
    )

    field(:target_timezone, {:required, :string},
      description: "Target IANA timezone name (e.g., 'Asia/Tokyo', 'America/San_Francisco')."
    )
  end

  @impl true
  def execute(params, frame) do
    source_tz = Map.get(params, :source_timezone)
    datetime_str = Map.get(params, :datetime)
    time_str = Map.get(params, :time)
    target_tz = Map.get(params, :target_timezone)

    result = with {:ok, source_dt} <- get_source_datetime(datetime_str, time_str, source_tz),
                  {:ok, target_dt} <- DateTime.shift_zone(source_dt, target_tz) do
      source_offset = source_dt.utc_offset + source_dt.std_offset
      target_offset = target_dt.utc_offset + target_dt.std_offset
      diff_hours = (target_offset - source_offset) / 3600

      time_diff_str = format_time_diff(diff_hours)

      result_map = %{
        source: %{
          timezone: source_tz,
          datetime: DateTime.to_iso8601(source_dt),
          day_of_week: Calendar.strftime(source_dt, "%A"),
          is_dst: source_dt.std_offset != 0
        },
        target: %{
          timezone: target_tz,
          datetime: DateTime.to_iso8601(target_dt),
          day_of_week: Calendar.strftime(target_dt, "%A"),
          is_dst: target_dt.std_offset != 0
        },
        time_difference: time_diff_str
      }

      {:ok, Jason.encode!(result_map, pretty: true)}
    else
      {:error, :invalid_time_format} ->
        {:error, "Invalid time format '#{time_str}'. Expected HH:MM in 24-hour format."}

      {:error, :invalid_datetime_format} ->
        {:error, "Invalid datetime format '#{datetime_str}'. Expected ISO8601 format."}

      {:error, :no_time_provided} ->
        {:error, "Either 'datetime' or 'time' parameter must be provided."}

      {:error, reason} ->
        {:error, "Error converting time: #{inspect(reason)}"}
    end

    case result do
      {:ok, json} ->
        {:reply, Response.tool() |> Response.text(json), frame}

      {:error, msg} ->
        {:reply, Response.tool() |> Response.text("Error: #{msg}"), frame}
    end
  end

  defp parse_time(time_str) when is_binary(time_str) do
    case String.split(time_str, ":") do
      [h_str, m_str] ->
        with {hour, ""} <- Integer.parse(h_str),
             {minute, ""} <- Integer.parse(m_str),
             true <- hour in 0..23,
             true <- minute in 0..59 do
          {:ok, {hour, minute}}
        else
          _ -> {:error, :invalid_time_format}
        end

      _ ->
        {:error, :invalid_time_format}
    end
  end

  defp parse_time(_), do: {:error, :invalid_time_format}

  defp get_source_datetime(datetime_str, time_str, source_tz) do
    cond do
      is_binary(datetime_str) and datetime_str != "" ->
        parse_datetime(datetime_str, source_tz)

      is_binary(time_str) and time_str != "" ->
        with {:ok, {hour, minute}} <- parse_time(time_str),
             {:ok, source_now} <- DateTime.now(source_tz),
             source_date = DateTime.to_date(source_now) do
          {:ok, DateTime.new!(source_date, Time.new!(hour, minute, 0), source_tz)}
        end

      true ->
        {:error, :no_time_provided}
    end
  end

  defp parse_datetime(datetime_str, source_tz) when is_binary(datetime_str) do
    case DateTime.from_iso8601(datetime_str) do
      {:ok, dt, _offset} ->
        {:ok, dt}

      {:error, _} ->
        # Try parsing as NaiveDateTime (without timezone) and assign source timezone
        case NaiveDateTime.from_iso8601(datetime_str) do
          {:ok, naive_dt} ->
            case DateTime.from_naive(naive_dt, source_tz) do
              {:ok, dt} -> {:ok, dt}
              {:ambiguous, dt1, _dt2} -> {:ok, dt1}
              {:gap, _} -> {:error, :invalid_datetime_format}
              {:error, _} -> {:error, :invalid_datetime_format}
            end

          {:error, _} ->
            {:error, :invalid_datetime_format}
        end
    end
  end

  defp parse_datetime(_, _), do: {:error, :invalid_datetime_format}

  defp format_time_diff(hours) when is_float(hours) do
    if hours == trunc(hours) do
      sign = if hours >= 0, do: "+", else: ""
      "#{sign}#{trunc(hours)}h"
    else
      sign = if hours >= 0, do: "+", else: ""
      formatted =
        :erlang.float_to_binary(abs(hours), decimals: 2)
        |> String.trim_trailing("0")
        |> String.trim_trailing(".")

      "#{sign}#{if hours < 0, do: "-", else: ""}#{formatted}h"
    end
  end
end
