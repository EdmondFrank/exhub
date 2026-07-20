defmodule Exhub.Genclaw.SystemPrompt do
  @moduledoc """
  Renders the GenClaw system prompt template with session variables.

  The template lives at `priv/genclaw/system.md` and uses `{{var}}` placeholders
  for L2 Session environment info (cwd, platform, shell, os_version, date, model).
  """

  @template_path "genclaw/system.md"

  @doc """
  Render the system prompt by filling L2 Session placeholders.

  ## Options

    * `:cwd` — working directory (default: `File.cwd!()`)
    * `:model` — model name (default: `"kimi-k2.6"`)
    * `:template_path` — override path to template file
  """
  def render(opts \\ []) do
    path = Keyword.get(opts, :template_path) || default_template_path()
    text = File.read!(path)

    substitutions = %{
      "cwd" => Keyword.get(opts, :cwd) || File.cwd!(),
      "platform" => platform_name(),
      "shell" => System.get_env("SHELL", "unknown"),
      "os_version" => os_version(),
      "session_date" => Date.to_iso8601(Date.utc_today()),
      "model" => Keyword.get(opts, :model, "kimi-k2.6")
    }

    Enum.reduce(substitutions, text, fn {key, value}, acc ->
      String.replace(acc, "{{" <> key <> "}}", to_string(value))
    end)
  end

  defp default_template_path do
    :exhub
    |> :code.priv_dir()
    |> Path.join(@template_path)
  end

  defp platform_name do
    case :os.type() do
      {:unix, :darwin} -> "macOS"
      {:unix, :linux} -> "Linux"
      {:win32, _} -> "Windows"
      _ -> "Unknown"
    end
  end

  defp os_version do
    case :os.version() do
      {major, minor, patch} -> "#{major}.#{minor}.#{patch}"
      _ -> "Unknown"
    end
  end
end
