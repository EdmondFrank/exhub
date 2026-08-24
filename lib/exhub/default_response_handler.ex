defmodule Exhub.DefaultResponseHandler do
  alias Exhub.ResponseHandlers.ExhubTranslate
  alias Exhub.ResponseHandlers.ExhubChat
  alias Exhub.ResponseHandlers.ExhubTool
  alias Exhub.ResponseHandlers.ExhubFile
  alias Exhub.ResponseHandlers.ExhubGitee
  alias Exhub.ResponseHandlers.ExhubAgent
  alias Exhub.ResponseHandlers.ExhubConfig
  alias Exhub.ResponseHandlers.ExhubVault
  alias Exhub.ResponseHandlers.ExhubBlinkSearch

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
              "exhub-config" -> ExhubConfig.call(args)
              "exhub-agent" -> ExhubAgent.call(args)
              "exhub-vault" -> ExhubVault.call(args)
              "blink-search" -> ExhubBlinkSearch.call(args)
              action -> Logger.debug("Unknown action: #{action}, data: #{inspect(data)}")
            end

          _ ->
            Logger.debug("Unknown message: #{inspect(data)}")
        end

        nil
    end
  end
end
