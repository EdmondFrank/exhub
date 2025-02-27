defmodule Exhub.ResponseHandlers.ExhubTranslate do
  alias Exhub.Llm.Translate

  def call(["exhub-translate", content, style, buffer_name, placeholder, to_lang, action]) do
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
  end

  defp inf_inspect(object) do
    inspect(object, printable_limit: :infinity)
  end
end
