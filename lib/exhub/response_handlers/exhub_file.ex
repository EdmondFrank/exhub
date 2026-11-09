defmodule Exhub.ResponseHandlers.ExhubFile do
  @moduledoc false

  require Logger

  # Mermaid diagrams are emitted as base64 `data:` SVG images (MDExMermex),
  # so the sanitizer must allow the data scheme or the <img> loses its src.
  @sanitize_options [add_url_schemes: ["data"]]

  # MDExMermex wraps each diagram in a browser toolbar; shr does not run its
  # JS, so the buttons would only add noise to the EWW preview.
  @toolbar_regex ~r|<div class="mdex-mermex-toolbar">.*?</div>|s

  def call(["exhub-file", "markdown-render", callback, [content, buffer_name]]) do
    html = render_markdown(content)
    Exhub.send_message(~s[(#{callback} #{inf_inspect(html)} "#{buffer_name}")])
  end

  @doc false
  # Renders Markdown to HTML, turning ```mermaid fences into inline SVG images.
  # Falls back to plain Markdown rendering when Mermaid cannot render (e.g. an
  # invalid diagram), so a broken diagram never breaks the whole preview.
  def render_markdown(content) when is_binary(content) do
    content
    |> do_render()
    |> strip_toolbar()
  end

  defp do_render(markdown) do
    MDEx.to_html!(markdown,
      plugins: [{MDExMermex, inject_css: false, inject_js: false}],
      render: [unsafe: true],
      sanitize: @sanitize_options
    )
  rescue
    error ->
      Logger.warning(
        "Mermaid rendering failed, falling back to plain Markdown: " <>
          Exception.message(error)
      )

      MDEx.to_html!(markdown, render: [unsafe: true], sanitize: @sanitize_options)
  end

  defp strip_toolbar(html), do: Regex.replace(@toolbar_regex, html, "")

  defp inf_inspect(object) do
    inspect(object, printable_limit: :infinity)
  end
end
