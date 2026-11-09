defmodule Exhub.ResponseHandlers.ExhubChat do
  alias Exhub.Llm.Chat

  # Commit-message generation sends the staged diff to the model verbatim.
  # Without a cap, a large staged change (measured: an 8.5 MB diff) blows past
  # the provider's input limit and the request dies with
  # "Range of input length should be [1, 983616]" — so the diff is truncated
  # to @default_max_diff_chars (overridable via app config) and a per-file
  # summary is always included so the model still sees the full scope.
  @default_max_diff_chars 40_000
  @max_error_chars 300

  @git_diff_args ["-c", "core.quotepath=false", "diff", "--staged", "--no-color"]
  @git_stat_args ["-c", "core.quotepath=false", "diff", "--staged", "--stat", "--no-color"]
  @git_log_args ["log", "--pretty=format:%s", "-5"]

  def call(["exhub-chat", user_message, buffer_name]) do
    case reply_result(Chat.execute(user_message)) do
      {:ok, reply} ->
        Exhub.send_message(~s[(exhub-chat-response 1 #{inf_inspect(reply)} "#{buffer_name}")])
        Exhub.send_message(~s[(exhub-chat-finish-answer "#{buffer_name}")])

      {:error, reason} ->
        notify("Exhub chat failed: " <> short_reason(reason))
    end
  end

  def call([
        "exhub-chat",
        "generate-git-commit-message-with-content",
        diff,
        commits,
        buffer_name,
        region_begin,
        region_end
      ]) do
    notify("Generating...")

    if String.trim(diff) != "" do
      generate_commit_message(diff, commits, buffer_name, region_begin, region_end)
    else
      notify("Please stage your changes first")
    end
  end

  def call([
        "exhub-chat",
        "generate-git-commit-message",
        dir,
        buffer_name,
        region_begin,
        region_end
      ]) do
    notify("Generating...")

    case Exhub.Utils.git_cmd(@git_diff_args, dir) do
      {:ok, diff} ->
        if String.trim(diff) == "" do
          notify("Please stage your changes first")
        else
          commits =
            case Exhub.Utils.git_cmd(@git_log_args, dir) do
              {:ok, output} -> output
              {:error, _} -> ""
            end

          stat =
            case Exhub.Utils.git_cmd(@git_stat_args, dir) do
              {:ok, output} -> output
              {:error, _} -> nil
            end

          generate_commit_message(diff, commits, buffer_name, region_begin, region_end, stat)
        end

      {:error, reason} ->
        notify("Git error: " <> short_reason(reason))
    end
  end

  def call(["exhub-chat", prompt, buffer_name, text, notify_start, notify_end]) do
    notify(notify_start)

    user_message =
      if text |> String.trim() |> String.length() == 0, do: prompt, else: "#{prompt}:\n#{text}"

    case reply_result(Chat.execute(user_message)) do
      {:ok, reply} ->
        Exhub.send_message(~s[(exhub-chat-response 1 #{inf_inspect(reply)} "#{buffer_name}")])

      {:error, reason} ->
        notify("Exhub chat failed: " <> short_reason(reason))
    end

    notify(notify_end)
  end

  def call(["exhub-chat", system_prompt, prompt, buffer_name, text, notify_start, notify_end]) do
    notify(notify_start)
    sys_message = system_prompt |> String.trim()

    user_message =
      if text |> String.trim() |> String.length() == 0, do: prompt, else: "#{prompt}:\n#{text}"

    case reply_result(Chat.execute(sys_message, user_message)) do
      {:ok, reply} ->
        Exhub.send_message(~s[(exhub-chat-response 1 #{inf_inspect(reply)} "#{buffer_name}")])

      {:error, reason} ->
        notify("Exhub chat failed: " <> short_reason(reason))
    end

    notify(notify_end)
  end

  def call([
        "exhub-chat",
        "improve-document",
        system_prompt,
        buffer_name,
        text,
        notify_start,
        notify_end,
        region_begin,
        region_end
      ]) do
    notify(notify_start)

    json_schema = %{
      type: "object",
      properties: %{
        improved_text: %{
          type: "string",
          description: "The improved document text with grammar and spelling corrections applied"
        }
      },
      required: ["improved_text"]
    }

    with {:ok, reply} <- Chat.execute_with_schema(system_prompt, text, json_schema),
         {:ok, decoded} <- Jason.decode(reply),
         improved_text when is_binary(improved_text) <- decoded["improved_text"] do
      Exhub.send_message(
        ~s[(exhub-chat-return-text 1 #{inf_inspect(improved_text)} "#{buffer_name}" #{region_begin} #{region_end})]
      )
    else
      error ->
        notify("Error parsing structured response: #{inf_inspect(error)}")
    end

    notify(notify_end)
  end

  def call([
        "exhub-chat",
        prompt,
        buffer_name,
        text,
        notify_start,
        notify_end,
        region_begin,
        region_end,
        func
      ]) do
    notify(notify_start)

    user_message =
      if text |> String.trim() |> String.length() == 0, do: prompt, else: "#{prompt}:\n#{text}"

    case reply_result(Chat.execute(user_message)) do
      {:ok, reply} ->
        Exhub.send_message(
          ~s[(#{func} 1 #{inf_inspect(reply)} "#{buffer_name}" #{region_begin} #{region_end})]
        )

      {:error, reason} ->
        notify("Exhub chat failed: " <> short_reason(reason))
    end

    notify(notify_end)
  end

  defp commit_message_sys_prompt do
    "
    ## Role:\nYou are a Git commit message generator. Your primary task is to create a concise and informative patch title for a given diff content, while considering the context from recent commit messages for consistency. You will analyze the content starting with `-` or `+` at the beginning of the line to understand the changes made. Your goal is to summarize the changes in a single sentence that is no more than 100 characters long, informed by recent commit history.\n\n## Skills:\n1. **Diff Analysis**: You can analyze the diff content to identify the changes made, focusing on lines that start with `-` or `+`.\n2. **Concise Summarization**: You can create a concise and informative summary of the changes, avoiding mechanical lists.\n3. **Proper Formatting**: You can format the title according to the specified rules: the first word capitalized, all other words lowercase, unless they are proper nouns.\n4. **Special Case Handling**: If the diff content starts with 'Subproject commit', you can extract the submodule name and generate the title 'Update xxx modules', where 'xxx' is the submodule name.\n5. **Context Awareness**: You can integrate context from recent commit messages to ensure the new commit message aligns with the project's commit history.\n\n## Constraints:\n1. **Length Limit**: The commit message should not exceed 100 characters.\n2. **Focus**: The commit message should focus on the changes described in the diff content, but can be informed by the context of recent commits to maintain consistency.\n3. **No Explanations**: Do not provide any explanations or instructions; only provide the commit message.\n
    "
  end

  defp generate_commit_message(diff, commits, buffer_name, region_begin, region_end, stat \\ nil) do
    user_message = build_commit_user_message(diff, commits, stat: stat)

    case reply_result(Chat.execute(commit_message_sys_prompt(), user_message)) do
      {:ok, reply} ->
        case clean_reply(reply) do
          "" ->
            notify("Commit message generation returned an empty reply.")

          message ->
            Exhub.send_message(
              ~s[(exhub-chat-return-text 1 #{inf_inspect(message)} "#{buffer_name}" #{region_begin} #{region_end})]
            )

            notify("Generate messages done.")
        end

      {:error, reason} ->
        notify("Commit message generation failed: " <> short_reason(reason))
    end
  end

  # `Exhub.Llm.Chain.execute/2` returns `{:ok, text}` on success but a bare
  # error-message binary on failure, so normalize both shapes here. Before
  # this, a provider error (e.g. the input-length rejection above) fell through
  # the `with` and the UI still reported success.
  defp reply_result({:ok, reply}), do: {:ok, reply}
  defp reply_result({:error, reason}), do: {:error, reason}
  defp reply_result(other), do: {:error, other}

  @doc false
  def build_commit_user_message(diff, commits, opts \\ []) do
    stat =
      case opts[:stat] |> to_string() |> String.trim() do
        "" -> diff_stat(diff)
        stat -> stat
      end

    {body, note} = truncate_diff(diff)

    """
    Recent commit messages:
    #{String.trim(to_string(commits || ""))}

    Staged changes (summary):
    #{stat}

    Current changes (diff):
    #{body}#{note || ""}
    """
    |> String.trim_trailing()
  end

  @doc false
  def truncate_diff(diff, max \\ nil) do
    max = max || max_diff_chars()
    size = byte_size(diff)

    if size <= max do
      {diff, nil}
    else
      head = diff |> String.slice(0, max) |> cut_at_hunk_boundary()

      if byte_size(head) >= size do
        {diff, nil}
      else
        note =
          "\n\n[... diff truncated: showing the first #{byte_size(head)} of #{size} bytes. " <>
            "The summary above lists every changed file; infer the remainder from it.]"

        {head, note}
      end
    end
  end

  # Drop the trailing partial file hunk so the model never sees half a diff.
  defp cut_at_hunk_boundary(text) do
    case :binary.matches(text, "\ndiff --git ") do
      [] ->
        text

      matches ->
        {pos, _len} = List.last(matches)
        if pos > 0, do: binary_part(text, 0, pos), else: text
    end
  end

  @doc false
  def max_diff_chars do
    case Application.get_env(:exhub, :commit_message_max_diff_chars, @default_max_diff_chars) do
      n when is_integer(n) and n > 0 -> n
      _ -> @default_max_diff_chars
    end
  end

  @doc false
  def diff_stat(diff) do
    diff
    |> String.split("\n")
    |> Enum.reduce([], fn line, acc ->
      cond do
        String.starts_with?(line, "diff --git ") ->
          [%{path: diff_path(line), adds: 0, dels: 0, binary: false} | acc]

        acc == [] ->
          acc

        String.starts_with?(line, "Binary files ") ->
          [%{hd(acc) | binary: true} | tl(acc)]

        String.starts_with?(line, "+++") or String.starts_with?(line, "---") ->
          acc

        String.starts_with?(line, "+") ->
          [%{hd(acc) | adds: hd(acc).adds + 1} | tl(acc)]

        String.starts_with?(line, "-") ->
          [%{hd(acc) | dels: hd(acc).dels + 1} | tl(acc)]

        true ->
          acc
      end
    end)
    |> Enum.reverse()
    |> Enum.map_join("\n", fn %{path: path, adds: adds, dels: dels, binary: binary} ->
      if binary, do: "#{path} | binary", else: "#{path} | +#{adds} -#{dels}"
    end)
  end

  defp diff_path("diff --git " <> rest) do
    case Regex.run(~r/ b\/(.+)$/, rest) do
      [_, path] -> path
      _ -> rest
    end
  end

  defp diff_path(line), do: line

  @doc false
  def clean_reply(reply) when is_binary(reply) do
    reply
    |> String.trim()
    |> strip_code_fence()
    |> strip_wrapping_quotes()
    |> String.trim()
  end

  defp strip_code_fence(text) do
    text
    |> String.replace(~r/\A```[[:alnum:]_.+-]*[ \t]*\r?\n/, "")
    |> String.replace(~r/\r?\n```[ \t]*\z/, "")
  end

  defp strip_wrapping_quotes(text) do
    case text do
      <<quote::binary-size(1), inner::binary>> when quote in ["\"", "'", "`"] ->
        if String.ends_with?(inner, quote) and byte_size(inner) >= 1 do
          binary_part(inner, 0, byte_size(inner) - 1)
        else
          text
        end

      _ ->
        text
    end
  end

  defp short_reason(%{message: message}) when is_binary(message) and message != "" do
    String.slice(message, 0, @max_error_chars)
  end

  defp short_reason(reason) do
    reason
    |> inspect(printable_limit: @max_error_chars)
    |> String.slice(0, @max_error_chars)
  end

  defp inf_inspect(object) do
    inspect(object, printable_limit: :infinity)
  end

  defp notify(msg) do
    Exhub.send_message(~s[(message #{inf_inspect(msg)})])
  end
end
