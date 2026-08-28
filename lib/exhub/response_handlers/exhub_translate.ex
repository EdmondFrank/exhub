defmodule Exhub.ResponseHandlers.ExhubTranslate do
  alias Exhub.Llm.Translate

  def call(["exhub-translate", content, style, buffer_name, placeholder, to_lang, action]) do
    with {:ok, translation} <- translate(content, to_lang, action) do
      msg =
        case action do
          "fix-grammar" ->
            # Grammar fixes are inserted as-is (no case/style conversion).
            ~s[(exhub-translate-update-translation-in-buffer #{inf_inspect(content)} "origin" #{inf_inspect(translation)} "#{buffer_name}" "#{placeholder}")]

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

  defp translate(content, _to_lang, "fix-grammar"), do: Translate.fix_grammar(content)
  defp translate(content, to_lang, _action), do: Translate.execute(content, to_lang)

  defp inf_inspect(object) do
    inspect(object, printable_limit: :infinity)
  end
end
