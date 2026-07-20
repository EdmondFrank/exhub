defmodule Exhub.Genclaw.RenderHelper do
  @moduledoc """
  Shared rendering utilities for GenClaw tools.

  Provides headless Chrome rendering with qlmanage fallback and timeout
  protection. Used by code_scene_draft and code_text_draft.
  """

  require Logger

  @default_render_timeout 30_000
  @max_side 2048
  @min_side 64

  @doc """
  Render HTML/SVG code to a PNG file via headless Chrome.

  Falls back to qlmanage if Chrome fails. Raises RuntimeError if both fail.

  ## Options
    * `:timeout` — render timeout in ms (default 30_000)
    * `:window_size` — "WxH" string for Chrome window (default derived from code)
  """
  def render_to_png(code, output_path, code_type, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, @default_render_timeout)
    File.mkdir_p!(Path.dirname(output_path))

    html_content = if code_type == "html", do: code, else: wrap_svg_in_html(code)

    {w, h} =
      case Keyword.get(opts, :window_size) do
        nil -> parse_canvas_size(code)
        {cw, ch} -> {cw, ch}
      end

    Logger.info("[Genclaw.RenderHelper] render #{code_type} #{w}x#{h} -> #{output_path}")

    tmp_html =
      Path.join(
        System.tmp_dir!(),
        "genclaw_render_#{:crypto.strong_rand_bytes(4) |> Base.encode16(case: :lower)}.html"
      )

    File.write!(tmp_html, html_content)

    try do
      chrome = chrome_path()

      args = [
        "--headless",
        "--disable-gpu",
        "--no-sandbox",
        "--disable-dev-shm-usage",
        "--screenshot=#{output_path}",
        "--window-size=#{w},#{h}",
        "file://#{tmp_html}"
      ]

      case run_chrome_with_timeout([chrome | args], timeout) do
        {:ok, 0} ->
          Logger.info("[Genclaw.RenderHelper] render done: #{output_path}")
          output_path

        {:ok, code} ->
          Logger.warning("[Genclaw.RenderHelper] chrome exited #{code}, trying qlmanage fallback")
          render_with_qlmanage(tmp_html, output_path)

        {:error, :timeout} ->
          Logger.warning("[Genclaw.RenderHelper] chrome timed out after #{timeout}ms, trying qlmanage")
          render_with_qlmanage(tmp_html, output_path)

        {:error, reason} ->
          Logger.warning("[Genclaw.RenderHelper] chrome failed: #{inspect(reason)}, trying qlmanage")
          render_with_qlmanage(tmp_html, output_path)
      end
    rescue
      e ->
        Logger.warning("[Genclaw.RenderHelper] chrome render failed: #{inspect(e)}, trying qlmanage")
        render_with_qlmanage(tmp_html, output_path)
    after
      File.rm(tmp_html)
    end
  end

  @doc """
  Wrap raw SVG code in a minimal HTML document for Chrome rendering.
  """
  def wrap_svg_in_html(svg_code) do
    """
    <!DOCTYPE html>
    <html><head><meta charset="utf-8">
    <style>
    body { margin: 0; display: flex; justify-content: center; align-items: center; min-height: 100vh; background: white; }
    svg { max-width: 100%; max-height: 100vh; }
    </style>
    </head><body>
    #{svg_code}
    </body></html>
    """
  end

  @doc """
  Parse canvas dimensions from SVG code. Returns {width, height} clamped
  to [@min_side, @max_side].
  """
  def parse_canvas_size(code, default \\ {1024, 1024}) do
    svg_tag =
      case Regex.run(~r/<svg\b[^>]*>/, code, return: :index) do
        [{s, len}] -> String.slice(code, s, len)
        _ -> code
      end

    w = extract_num(svg_tag, "width")
    h = extract_num(svg_tag, "height")

    {w, h} =
      if is_nil(w) or is_nil(h) do
        case Regex.run(~r/viewBox\s*=\s*["']\s*[-0-9.]+\s+[-0-9.]+\s+([0-9.]+)\s+([0-9.]+)/, svg_tag) do
          [_, vw, vh] ->
            {w || String.to_float(vw), h || String.to_float(vh)}

          _ ->
            {w || elem(default, 0), h || elem(default, 1)}
        end
      else
        {w, h}
      end

    wi = max(@min_side, min(round(w), @max_side))
    hi = max(@min_side, min(round(h), @max_side))
    {wi, hi}
  end

  @doc """
  Get the Chrome binary path from env or default macOS location.
  """
  def chrome_path do
    System.get_env("CHROME_PATH") ||
      "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
  end

  # ─── Private ─────────────────────────────────────────────────────────────

  defp run_chrome_with_timeout(cmd, timeout) do
    task =
      Task.async(fn ->
        Exile.stream(cmd, stderr: :consume)
        |> Enum.reduce({0, ""}, fn
          {:stdout, _}, {code, acc} -> {code, acc}
          {:exit, code}, {_, acc} -> {code, acc}
        end)
        |> elem(0)
      end)

    case Task.yield(task, timeout) || Task.shutdown(task, :brutal_kill) do
      {:ok, exit_code} -> {:ok, exit_code}
      nil -> {:error, :timeout}
    end
  rescue
    e -> {:error, e}
  end

  defp render_with_qlmanage(input_file, output_path) do
    out_dir = Path.dirname(output_path)
    File.mkdir_p!(out_dir)

    {_, 0} = System.cmd("qlmanage", ["-t", "-s", "1024", "-o", out_dir, input_file])

    ql_output = Path.join(out_dir, Path.basename(input_file, ".html") <> ".png")

    if File.exists?(ql_output) do
      File.rename(ql_output, output_path)
      output_path
    else
      raise RuntimeError, "rendering failed: neither chrome nor qlmanage produced output"
    end
  end

  defp extract_num(str, attr) do
    case Regex.run(~r/#{attr}\s*=\s*["']?\s*([0-9]+(?:\.[0-9]+)?)/, str) do
      [_, n] -> String.to_float(n)
      _ -> nil
    end
  end
end
