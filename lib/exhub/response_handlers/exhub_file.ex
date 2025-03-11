defmodule Exhub.ResponseHandlers.ExhubFile do
  def call(["exhub-file", "markdown-render", callback, [content, buffer_name]]) do
    html = MDEx.to_html!(content, render: [unsafe_: true], features: [sanitize: true])
    Exhub.send_message(~s[(#{callback} #{inf_inspect(html)} "#{buffer_name}")])
  end

  defp inf_inspect(object) do
    inspect(object, printable_limit: :infinity)
  end
end
