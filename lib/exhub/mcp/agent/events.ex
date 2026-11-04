defmodule Exhub.MCP.Agent.Events do
  @moduledoc """
  Shaping for the event stream an ACP agent produces.

  Agents stream every `session/update` they emit — reasoning chunks, the full
  text of each file they read, every command output, plus periodic noise
  (`available_commands_update`, `config_option_update`, `session_info_update`).
  Forwarded verbatim to an orchestrator, that stream dominates the caller's
  context: a two-file read costs ~28 KB of tool output for an answer that fits
  in a paragraph, and every poll is a separate model turn.

  Two shapes are produced here:

  - `digest/2` — the terminal summary returned by `agent_prompt` / `agent_task`:
    final answer, token usage, tool trace, changed files, errors.
  - `slim/2` — the bounded, redacted event list returned by
    `agent_prompt_events`: boilerplate dropped, tool output previews capped,
    total response size capped.
  """

  @boilerplate ~w(available_commands_update session_info_update config_option_update current_mode_update)

  @default_max_output 800
  @default_max_events 200
  @default_max_bytes 20_000
  @default_max_answer 4_000
  @default_max_tools 30

  @write_kinds ~w(edit write delete)

  @doc "Session updates that carry no information for an orchestrator."
  def boilerplate_updates, do: @boilerplate

  @doc """
  Terminal summary of one prompt turn.

  Options: `:agent_id`, `:session_id`, `:status`, `:stop_reason`, `:elapsed_ms`,
  `:max_answer_bytes`, `:max_tools`.
  """
  def digest(events, opts \\ []) when is_list(events) do
    max_answer = Keyword.get(opts, :max_answer_bytes, @default_max_answer)
    max_tools = Keyword.get(opts, :max_tools, @default_max_tools)

    {tools, tool_count} = tool_trace(events, max_tools)
    answer = answer(events)

    %{
      "agent_id" => Keyword.get(opts, :agent_id),
      "session_id" => Keyword.get(opts, :session_id),
      "status" => Keyword.get(opts, :status, "complete"),
      "stop_reason" => Keyword.get(opts, :stop_reason),
      "elapsed_ms" => Keyword.get(opts, :elapsed_ms),
      "answer" => cap(answer, max_answer),
      "answer_truncated" => is_binary(answer) and byte_size(answer) > max_answer,
      "usage" => usage(events),
      "tool_count" => tool_count,
      "tools" => tools,
      "tools_truncated" => tool_count > length(tools),
      "changed_files" => changed_files(tools),
      "errors" => errors(events),
      "permission_requests" => permission_requests(events),
      "event_count" => length(events)
    }
    |> compact()
  end

  @doc """
  Bounded event list for `agent_prompt_events`.

  Options: `:include_thoughts`, `:include_raw`, `:max_output_bytes`,
  `:max_events`, `:max_bytes`.

  Returns `%{events: [...], dropped: n, truncated: boolean}` — `dropped` counts
  events removed by `@boilerplate`/thought filtering *and* by the size caps.
  """
  def slim(events, opts \\ []) when is_list(events) do
    include_thoughts = Keyword.get(opts, :include_thoughts, false)
    include_raw = Keyword.get(opts, :include_raw, false)
    max_output = Keyword.get(opts, :max_output_bytes, @default_max_output)
    max_events = Keyword.get(opts, :max_events, @default_max_events)
    max_bytes = Keyword.get(opts, :max_bytes, @default_max_bytes)

    {kept, _bytes, dropped, truncated} =
      Enum.reduce(events, {[], 0, 0, false}, fn event, {acc, bytes, dropped, truncated} ->
        case slim_event(event, include_thoughts, include_raw, max_output) do
          :drop ->
            {acc, bytes, dropped + 1, truncated}

          event ->
            size = encoded_size(event)

            cond do
              length(acc) >= max_events ->
                {acc, bytes, dropped + 1, true}

              bytes + size > max_bytes ->
                {acc, bytes, dropped + 1, true}

              true ->
                {[event | acc], bytes + size, dropped, truncated}
            end
        end
      end)

    %{events: Enum.reverse(kept), dropped: dropped, truncated: truncated}
  end

  @doc "The last `usage_update` of the batch as `%{used, size, cost}`."
  def usage(events) when is_list(events) do
    events
    |> Enum.filter(&(update_type(&1) == "usage_update"))
    |> List.last()
    |> case do
      nil ->
        nil

      event ->
        u = update(event)
        %{"used" => u["used"], "size" => u["size"], "cost" => u["cost"]} |> compact()
    end
  end

  @doc """
  Assistant text of the turn: the concatenation of `agent_message_chunk`
  updates belonging to the last message id.
  """
  def answer(events) when is_list(events) do
    chunks =
      events
      |> Enum.filter(&(update_type(&1) == "agent_message_chunk"))
      |> Enum.map(fn event ->
        {Map.get(update(event), "messageId"), text_content(update(event))}
      end)
      |> Enum.reject(fn {_id, text} -> is_nil(text) end)

    case chunks do
      [] ->
        nil

      _ ->
        {last_id, _} = List.last(chunks)

        chunks
        |> Enum.filter(fn {id, _} -> id == last_id end)
        |> Enum.map_join("", fn {_id, text} -> text end)
    end
  end

  ## Event shaping

  defp slim_event(%{type: "update"} = event, include_thoughts, include_raw, max_output) do
    u = update(event)
    type = u["sessionUpdate"]

    cond do
      type in @boilerplate ->
        :drop

      type == "agent_thought_chunk" and not include_thoughts ->
        :drop

      type == "tool_call_update" ->
        Map.put(event, :update, slim_tool_update(u, include_raw, max_output))

      type == "tool_call" ->
        Map.put(event, :update, strip_raw(u, include_raw, max_output))

      true ->
        event
    end
  end

  # Terminal events, permission requests and overflow markers are small and
  # always worth forwarding.
  defp slim_event(event, _include_thoughts, _include_raw, _max_output), do: event

  defp slim_tool_update(u, include_raw, max_output) do
    output = output_text(u)

    base =
      if include_raw do
        u
      else
        Map.drop(u, ["content", "rawOutput", "rawInput"])
      end

    case output do
      nil ->
        base

      output ->
        base
        |> Map.put("output", cap(output, max_output))
        |> Map.put("output_bytes", byte_size(output))
        |> Map.put("output_truncated", byte_size(output) > max_output)
    end
  end

  # `tool_call` carries the intent (title, locations, small rawInput); keep the
  # intent, cap anything that can be large.
  defp strip_raw(u, include_raw, max_output) do
    base = if include_raw, do: u, else: Map.drop(u, ["rawOutput"])

    case Map.get(base, "rawInput") do
      raw when is_map(raw) ->
        Map.put(base, "rawInput", Map.new(raw, fn {k, v} -> {k, cap_value(v, max_output)} end))

      _ ->
        base
    end
  end

  defp cap_value(value, limit) when is_binary(value) and byte_size(value) > limit,
    do: cap(value, limit)

  defp cap_value(value, _limit), do: value

  ## Tool trace

  defp tool_trace(events, max) do
    {order, by_id} =
      Enum.reduce(events, {[], %{}}, fn event, {order, by_id} ->
        u = update(event)
        type = Map.get(u, "sessionUpdate")
        id = Map.get(u, "toolCallId")

        if type in ["tool_call", "tool_call_update"] and is_binary(id) do
          entry =
            by_id
            |> Map.get(id, %{"id" => id})
            |> merge_tool(u)

          order = if Map.has_key?(by_id, id), do: order, else: [id | order]
          {order, Map.put(by_id, id, entry)}
        else
          {order, by_id}
        end
      end)

    entries =
      order
      |> Enum.reverse()
      |> Enum.map(&Map.get(by_id, &1))

    {Enum.take(entries, max), length(order)}
  end

  defp merge_tool(entry, u) do
    entry
    |> put_present("title", u["title"])
    |> put_present("kind", u["kind"])
    |> put_present("status", u["status"])
    |> put_present("path", location_path(u))
    |> put_output(u)
  end

  defp put_output(entry, u) do
    case output_text(u) do
      nil ->
        entry

      output ->
        entry
        |> Map.put("output", cap(output, @default_max_output))
        |> Map.put("output_bytes", byte_size(output))
    end
  end

  defp location_path(%{"locations" => [%{"path" => path} | _]}) when is_binary(path), do: path
  defp location_path(%{"locations" => [%{"path" => path} | _]}) when is_list(path), do: path

  defp location_path(%{"locations" => [%{"path" => path} | _]}) when is_map(path),
    do: Map.get(path, "path") || Map.get(path, "uri")

  defp location_path(_), do: nil

  defp changed_files(tools) do
    tools
    |> Enum.filter(fn tool ->
      Map.get(tool, "kind") in @write_kinds and Map.get(tool, "status") == "completed" and
        is_binary(Map.get(tool, "path"))
    end)
    |> Enum.map(&Map.get(&1, "path"))
    |> Enum.uniq()
  end

  ## Misc extraction

  defp errors(events) do
    events
    |> Enum.filter(fn event -> Map.get(event, :type) == "error" end)
    |> Enum.map(fn event -> %{"message" => Map.get(event, :message)} end)
    |> Enum.take(5)
  end

  defp permission_requests(events) do
    events
    |> Enum.filter(fn event -> Map.get(event, :type) == "permission_request" end)
    |> Enum.map(fn event ->
      tool_call = Map.get(event, :tool_call) || %{}

      %{
        "title" => Map.get(tool_call, "title"),
        "kind" => Map.get(tool_call, "kind"),
        "options" => event |> Map.get(:options, []) |> length()
      }
      |> compact()
    end)
    |> Enum.take(5)
  end

  defp update(event), do: Map.get(event, :update) || %{}
  defp update_type(event), do: Map.get(update(event), "sessionUpdate")

  defp text_content(%{"content" => %{"text" => text}}) when is_binary(text), do: text
  defp text_content(_), do: nil

  defp output_text(u) do
    case Map.get(u, "content") do
      content when is_list(content) ->
        content
        |> Enum.map(&content_text/1)
        |> Enum.reject(&is_nil/1)
        |> Enum.join("\n")

      %{"text" => text} when is_binary(text) ->
        text

      _ ->
        nil
    end
    |> case do
      "" -> nil
      text -> text
    end
  end

  defp content_text(%{"content" => %{"text" => text}}) when is_binary(text), do: text
  defp content_text(%{"text" => text}) when is_binary(text), do: text
  defp content_text(_), do: nil

  ## Helpers

  defp put_present(map, _key, nil), do: map
  defp put_present(map, key, value), do: Map.put(map, key, value)

  defp compact(map) do
    map
    |> Enum.reject(fn {_k, v} -> is_nil(v) end)
    |> Map.new()
  end

  @doc false
  def cap(nil, _limit), do: nil

  def cap(text, limit) when is_binary(text) and byte_size(text) > limit do
    text
    |> binary_part(0, limit)
    |> trim_incomplete()
    |> Kernel.<>("…")
  end

  def cap(text, _limit), do: text

  defp trim_incomplete(bin) do
    case :unicode.characters_to_binary(bin) do
      {:incomplete, ok, _rest} -> ok
      {:error, ok, _rest} -> ok
      ok when is_binary(ok) -> ok
    end
  end

  defp encoded_size(event) do
    byte_size(Jason.encode!(event))
  rescue
    _ -> 500
  end
end
