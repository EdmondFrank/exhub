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
        user_message = """
        Please generate a patch title for the following diff content, mainly analyze the content starting with - or + at the beginning of the line, with a concise and informative summary instead of a mechanical list. The title should not exceed 100 characters in length, and the format of the words in the title should be: the first word capitalized, all other words lowercase, unless they are proper nouns, if the diff content starts with 'Subproject commit', you extract the submodule name 'xxx', and reply 'Update xxx modules'. Please just put the commit message and don't give any explanations or instructions.
        \n
        #{diff}
        """
        with {:ok, reply} <- Chat.execute(user_message) do
          Exhub.send_message(~s[(exhub-chat-return-text 1 #{inf_inspect(reply)} "#{buffer_name}" #{region_begin} #{region_end})])
        end
        notify("Generate messages done.")
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
