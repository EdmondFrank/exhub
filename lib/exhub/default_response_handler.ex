defmodule Exhub.DefaultResponseHandler do
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
                    ~s[(exhub-translate-update-translation-in-buffer "#{content}" "#{style}" "#{translation}" "#{buffer_name}" "#{placeholder}")]
                  "posframe" ->
                    ~s[(exhub-translate-show-translation-posframe "#{translation}")]
                  _ ->
                    ~s[(message "Unknown action for exhub-translate")]
                end
              Exhub.send_message(msg)
            end
          msg -> Logger.debug("Unknown message: #{msg}")
        end
        nil
    end
  end
end
