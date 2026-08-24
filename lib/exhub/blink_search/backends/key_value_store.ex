defmodule Exhub.BlinkSearch.Backends.KeyValueStore do
  @moduledoc """
  Key Value Store backend — read/get/del/update key-value pairs.

  Uses a simple file-based store (one `key=value` per line) as a portable
  alternative to SQLite. Supports commands:
  - `set key value` — insert or update
  - `del key` — delete
  - plain key — lookup and copy value
  """

  use Exhub.BlinkSearch.Backend

  alias Exhub.BlinkSearch.Backend

  @set_regex ~r/^set\s([^\s]+)\s([^\s]+)$/
  @del_regex ~r/^del\s([^\s]+)$/

  @impl true
  def search_match(prefix, state) do
    store_path = store_path(state)

    candidates =
      if File.exists?(store_path) do
        store_path
        |> File.read!()
        |> String.split("\n", trim: true)
        |> Enum.map(fn line ->
          case String.split(line, "=", parts: 2) do
            [key, _value] -> key
            _ -> nil
          end
        end)
        |> Enum.reject(&is_nil/1)
        |> Enum.filter(fn key -> String.contains?(key, prefix) end)
        |> Enum.take(20)
      else
        []
      end

    # Add command candidates if prefix matches set/del patterns
    extra =
      cond do
        Regex.match?(@set_regex, prefix) -> [prefix]
        Regex.match?(@del_regex, prefix) -> [prefix]
        true -> []
      end

    candidates ++ extra
  end

  @impl true
  def do_action(candidate, state) do
    text = Backend.candidate_text(candidate)
    store_path = store_path(state)

    cond do
      # SET command
      match = Regex.run(@set_regex, text) ->
        [_, key, value] = match
        store = read_store(store_path)

        new_store =
          if Map.has_key?(store, key) do
            Exhub.send_message(
              ~s|(message "[Blink-Search] Updated key-value (#{Backend.escape_message(key)}, #{Backend.escape_message(value)}) successfully")|
            )

            Map.put(store, key, value)
          else
            Exhub.send_message(
              ~s|(message "[Blink-Search] Inserted key-value (#{Backend.escape_message(key)}, #{Backend.escape_message(value)}) successfully")|
            )

            Map.put(store, key, value)
          end

        write_store(store_path, new_store)

      # DEL command
      match = Regex.run(@del_regex, text) ->
        [_, key] = match
        store = read_store(store_path)
        new_store = Map.delete(store, key)
        write_store(store_path, new_store)

        Exhub.send_message(
          ~s|(message "[Blink-Search] Deleted #{Backend.escape_message(key)} successfully")|
        )

      # GET command — lookup and copy value
      true ->
        store = read_store(store_path)

        case Map.get(store, text) do
          nil ->
            Exhub.send_message(~s|(message "[Blink-Search] Key not found: #{Backend.escape_message(text)}")|)

          value ->
            Exhub.send_message(~s|(kill-new #{Backend.elisp_quote(value)})|)
            Exhub.send_message(~s|(message "[Blink-Search] Copied value for key: #{Backend.escape_message(text)}")|)
        end
    end

    :ok
  end

  @impl true
  def parent(candidate, state), do: do_action(candidate, state)

  # Private helpers

  defp store_path(state) do
    Map.get(state, :db_path, default_store_path())
  end

  defp default_store_path do
    Path.join([System.user_home!(), ".emacs.d", "blink-search-kv.txt"])
  end

  defp read_store(path) do
    if File.exists?(path) do
      path
      |> File.read!()
      |> String.split("\n", trim: true)
      |> Enum.reduce(%{}, fn line, acc ->
        case String.split(line, "=", parts: 2) do
          [key, value] -> Map.put(acc, key, value)
          _ -> acc
        end
      end)
    else
      %{}
    end
  end

  defp write_store(path, store) do
    path |> Path.dirname() |> File.mkdir_p!()

    content =
      store
      |> Enum.map(fn {k, v} -> "#{k}=#{v}" end)
      |> Enum.join("\n")

    File.write!(path, content <> "\n")
  end
end
