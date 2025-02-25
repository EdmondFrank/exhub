defmodule Exhub.DefaultResponseHandler do
  alias Exhub.Llm.Chat
  alias Exhub.Llm.Translate

  require Logger
  def call(message) do
    case Jason.decode(message) do
      {:ok, data} ->
        case data do
          ["func", ["exhub-translate", content, style, buffer_name, placeholder, to_lang, action]] ->
            with {:ok, translation} <- Translate.execute(content, to_lang) do
              msg =
                case action do
                  "replace" ->
                    ~s[(exhub-translate-update-translation-in-buffer #{inf_inspect(content)} "#{style}" #{inf_inspect(translation)} "#{buffer_name}" "#{placeholder}")]
                  "posframe" ->
                    ~s[(exhub-translate-show-translation-posframe #{inf_inspect(translation)})]
                  _ ->
                    ~s[(message "Unknown action for exhub-translate")]
                end
              Exhub.send_message(msg)
            end

          ["func", ["exhub-chat", user_message, buffer_name]] ->
            with {:ok, reply} <- Chat.execute(user_message) do
              Exhub.send_message(~s[(exhub-chat-response 1 #{inf_inspect(reply)} "#{buffer_name}")])
              Exhub.send_message(~s[(exhub-chat-finish-answer "#{buffer_name}")])
            end

          ["func", ["exhub-chat", "generate-git-commit-message", dir, buffer_name, region_begin, region_end]] ->
            notify("Generating...")
            case System.cmd("git", ["diff", "--staged"], cd: dir) do
              {diff, 0} when diff != "" ->
                user_message = """
                Please generate a patch title for the following diff content, mainly analyze the content starting with - or + at the beginning of the line, with a concise and informative summary instead of a mechanical list. The title should not exceed 100 characters in length, and the format of the words in the title should be: the first word capitalized, all other words lowercase, unless they are proper nouns, if the diff content starts with 'Subproject commit', you extract the submodule name 'xxx', and reply 'Update xxx modules'. Please just put the commit message in code block and don't give any explanations or instructions.
                \n
                #{diff}
                """
                with {:ok, reply} <- Chat.execute(user_message) do
                  Exhub.send_message(~s[(exhub-chat-return-code 1 #{inf_inspect(reply)} "#{buffer_name}" #{region_begin} #{region_end})])
                end
                notify("Generate messages done.")
              {"", 0} ->
                notify("Please Staged changes at first")
              reason ->
                notify("Unknown error: #{inf_inspect(reason)}")
            end

          ["func", ["exhub-chat", prompt, buffer_name, text, notify_start, notify_end]] ->
            notify(notify_start)
            user_message = if text |> String.trim() |> String.length() == 0, do: prompt, else: "#{prompt}:\n#{text}"
            with {:ok, reply} <- Chat.execute(user_message) do
              Exhub.send_message(~s[(exhub-chat-response 1 #{inf_inspect(reply)} "#{buffer_name}")])
            end
            notify(notify_end)

          ["func", ["exhub-chat", system_prompt, prompt, buffer_name, text, notify_start, notify_end]] ->
            notify(notify_start)
            sys_message = system_prompt |> String.trim()
            user_message = if text |> String.trim() |> String.length() == 0, do: prompt, else: "#{prompt}:\n#{text}"
            with {:ok, reply} <- Chat.execute(sys_message, user_message) do
              Exhub.send_message(~s[(exhub-chat-response 1 #{inf_inspect(reply)} "#{buffer_name}")])
            end
            notify(notify_end)

          ["func", ["exhub-chat", prompt, buffer_name, text, notify_start, notify_end, region_begin, region_end, func]] ->
            notify(notify_start)
            user_message = if text |> String.trim() |> String.length() == 0, do: prompt, else: "#{prompt}:\n#{text}"
            with {:ok, reply} <- Chat.execute(user_message) do
              Exhub.send_message(~s[(#{func} 1 #{inf_inspect(reply)} "#{buffer_name}" #{region_begin} #{region_end})])
            end
            notify(notify_end)

          ["func", ["exhub-gitee", module, func, callback, args]] ->
            with {200, body, _response} <- apply(Module.concat(GiteeCat, module), String.to_existing_atom(func), [GiteeCat.Client.new() | args]) do
              Exhub.send_message(~s[(#{callback} #{inf_inspect(Jason.encode!(body))})])
            end
          msg -> Logger.debug("Unknown message: #{msg}")
        end
        nil
    end
  end

  defp inf_inspect(object) do
    inspect(object, printable_limit: :infinity)
  end

  defp notify(msg) do
    Exhub.send_message(~s[(message #{inf_inspect(msg)})])
  end
end
