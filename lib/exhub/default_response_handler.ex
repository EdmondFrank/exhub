defmodule Exhub.DefaultResponseHandler do
  alias Exhub.ResponseHandlers.ExhubTranslate
  alias Exhub.ResponseHandlers.ExhubChat
  alias Exhub.ResponseHandlers.ExhubTool
  alias Exhub.ResponseHandlers.ExhubFile
  alias Exhub.ResponseHandlers.ExhubGitee

  require Logger

  def call(message) do
    case Jason.decode(message) do
      {:ok, data} ->
        case data do
          ["func", args] ->
            case List.first(args) do
              "exhub-translate" -> ExhubTranslate.call(args)
              "exhub-chat" -> ExhubChat.call(args)
              "exhub-gitee" -> ExhubGitee.call(args)
              "exhub-tool" -> ExhubTool.call(args)
              "exhub-file" -> ExhubFile.call(args)
              action -> Logger.debug("Unknown action: #{action}, data: #{data}")
            end
          _ -> Logger.debug("Unknown message: #{data}")
        end
        nil
    end
  end
end
