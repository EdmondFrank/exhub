defmodule Exhub.ResponseHandlers.ExhubChat do
  alias Exhub.Llm.Chat

  def call(["exhub-chat", user_message, buffer_name]) do
    with {:ok, reply} <- Chat.execute(user_message) do
      Exhub.send_message(~s[(exhub-chat-response 1 #{inf_inspect(reply)} "#{buffer_name}")])
      Exhub.send_message(~s[(exhub-chat-finish-answer "#{buffer_name}")])
    end
  end

  def call(["exhub-chat", "generate-git-commit-message", dir, buffer_name, region_begin, region_end]) do
    notify("Generating...")
    case System.cmd("git", ["diff", "--staged"], cd: dir) do
      {diff, 0} when diff != "" ->
        case System.cmd("git", ["log", "--pretty=format:%s", "-5"], cd: dir) do
          {commits, 0} ->
            sys_message = "
            ## Role:\nYou are a Git commit message generator. Your primary task is to create a concise and informative patch title for a given diff content, while considering the context from recent commit messages for consistency. You will analyze the content starting with `-` or `+` at the beginning of the line to understand the changes made. Your goal is to summarize the changes in a single sentence that is no more than 100 characters long, informed by recent commit history.\n\n## Skills:\n1. **Diff Analysis**: You can analyze the diff content to identify the changes made, focusing on lines that start with `-` or `+`.\n2. **Concise Summarization**: You can create a concise and informative summary of the changes, avoiding mechanical lists.\n3. **Proper Formatting**: You can format the title according to the specified rules: the first word capitalized, all other words lowercase, unless they are proper nouns.\n4. **Special Case Handling**: If the diff content starts with 'Subproject commit', you can extract the submodule name and generate the title 'Update xxx modules', where 'xxx' is the submodule name.\n5. **Context Awareness**: You can integrate context from recent commit messages to ensure the new commit message aligns with the project's commit history.\n\n## Constraints:\n1. **Length Limit**: The commit message should not exceed 100 characters.\n2. **Focus**: The commit message should focus on the changes described in the diff content, but can be informed by the context of recent commits to maintain consistency.\n3. **No Explanations**: Do not provide any explanations or instructions; only provide the commit message.\n
            "
            user_message = "Recent commit messages:\n#{commits}\n\nCurrent changes:\n#{diff}"
            with {:ok, reply} <- Chat.execute(sys_message, user_message) do
              Exhub.send_message(~s[(exhub-chat-return-text 1 #{inf_inspect(reply)} "#{buffer_name}" #{region_begin} #{region_end})])
            end
            notify("Generate messages done.")
          {error_output, exit_code} ->
            notify("Failed to fetch recent commits: exit code #{exit_code}, output: #{inf_inspect(error_output)}")
        end
      {"", 0} ->
        notify("Please Staged changes at first")
      reason ->
        notify("Unknown error: #{inf_inspect(reason)}")
    end
  end

  def call(["exhub-chat", prompt, buffer_name, text, notify_start, notify_end]) do
    notify(notify_start)
    user_message = if text |> String.trim() |> String.length() == 0, do: prompt, else: "#{prompt}:\n#{text}"
    with {:ok, reply} <- Chat.execute(user_message) do
      Exhub.send_message(~s[(exhub-chat-response 1 #{inf_inspect(reply)} "#{buffer_name}")])
    end
    notify(notify_end)
  end

  def call(["exhub-chat", system_prompt, prompt, buffer_name, text, notify_start, notify_end]) do
    notify(notify_start)
    sys_message = system_prompt |> String.trim()
    user_message = if text |> String.trim() |> String.length() == 0, do: prompt, else: "#{prompt}:\n#{text}"
    with {:ok, reply} <- Chat.execute(sys_message, user_message) do
      Exhub.send_message(~s[(exhub-chat-response 1 #{inf_inspect(reply)} "#{buffer_name}")])
    end
    notify(notify_end)
  end

  def call(["exhub-chat", "improve-document", system_prompt, buffer_name, text, notify_start, notify_end, region_begin, region_end]) do
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
      Exhub.send_message(~s[(exhub-chat-return-text 1 #{inf_inspect(improved_text)} "#{buffer_name}" #{region_begin} #{region_end})])
    else
      error ->
        notify("Error parsing structured response: #{inf_inspect(error)}")
    end
    notify(notify_end)
  end

  def call(["exhub-chat", prompt, buffer_name, text, notify_start, notify_end, region_begin, region_end, func]) do
    notify(notify_start)
    user_message = if text |> String.trim() |> String.length() == 0, do: prompt, else: "#{prompt}:\n#{text}"
    with {:ok, reply} <- Chat.execute(user_message) do
      Exhub.send_message(~s[(#{func} 1 #{inf_inspect(reply)} "#{buffer_name}" #{region_begin} #{region_end})])
    end
    notify(notify_end)
  end

  defp inf_inspect(object) do
    inspect(object, printable_limit: :infinity)
  end

  defp notify(msg) do
    Exhub.send_message(~s[(message #{inf_inspect(msg)})])
  end
end
