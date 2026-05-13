defmodule Exhub.MCP.Tools.MacUse.Screenshot do
  @moduledoc """
  MCP Tool: screenshot

  Capture a screenshot of a macOS application or element.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.MacUse.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "screenshot"

  @impl true
  def description do
    """
    Capture a screenshot of a macOS application or element.

    Uses ScreenCaptureKit for occlusion-proof capture (works even when
    the app is behind other windows). Optionally performs OCR via Vision framework.

    Parameters:
    - app: Application name (required unless pid is provided)
    - pid: Process ID (alternative to app)
    - selector: Optional CSS-like selector to capture a specific element
    - ocr: If true, extract text from the screenshot via OCR
    - output: Output file path (default: returns base64-encoded image)
    - legacy: Use legacy screenshot method
    """
  end

  schema do
    field(:app, :string, description: "Application name")
    field(:pid, :integer, description: "Process ID")
    field(:selector, :string, description: "CSS-like selector for a specific element")
    field(:ocr, :boolean, description: "Extract text via OCR", default: false)
    field(:output, :string, description: "Output file path")
    field(:legacy, :boolean, description: "Use legacy screenshot method", default: false)
  end

  @impl true
  def execute(params, frame) do
    args = ["screenshot"] ++ Helpers.app_args(params)

    args = if Map.get(params, :ocr, false), do: args ++ ["--ocr"], else: args
    args = if Map.get(params, :legacy, false), do: args ++ ["--legacy"], else: args
    args = maybe_add(args, "--output", Map.get(params, :output))

    args =
      case Map.get(params, :selector) do
        nil -> args
        sel -> args ++ [sel]
      end

    case Helpers.run_axcli(args, timeout: 60_000) do
      {:ok, output} ->
        result = if Map.get(params, :ocr, false) do
          %{ocr_text: output}
        else
          case Map.get(params, :output) do
            nil -> %{image_base64: output}
            path -> %{saved_to: path, detail: output}
          end
        end

        resp =
          Response.tool()
          |> Helpers.toon_response(result)

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end

  defp maybe_add(args, _flag, nil), do: args
  defp maybe_add(args, flag, value), do: args ++ [flag, to_string(value)]
end
