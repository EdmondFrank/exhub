defmodule Exhub.BlinkSearch.Backends.OTP do
  @moduledoc """
  OTP backend — lists `cotp` entries and copies a code to the clipboard.

  The cotp database password is read from SecretVault (secret name
  `:pass_key`, default `"cotp_pass"`). `Exhub.Application.load_secrets/0`
  stores every secret in `:persistent_term` at boot; `exhub_reload_keys`
  (`Exhub.Router.Config.reload_from_scr/0`) refreshes them at runtime, so
  there is no envless dependency.

  Security model (mirrors the `cotp` skill):

  - `search_match/2` runs `cotp list --json` and keeps only `issuer`/`label` —
    the `otp_code` field is dropped (and would be stale anyway).
  - `do_action/2` runs `cotp extract -l <label> -c`, which copies the code to
    the clipboard. Its stdout (which carries the code) is discarded; only the
    label is reported back to Emacs.
  - The database password is piped to the child's stdin, never argv or env.

  Configuration (pushed from Emacs via `update`):
  - `pass_key` — SecretVault secret name (default `"cotp_pass"`)
  - `db_path` — optional cotp database path (default: cotp's own default)
  """

  use Exhub.BlinkSearch.Backend

  alias Exhub.BlinkSearch.Backend

  @default_pass_key "cotp_pass"
  @max_candidates 50

  @impl true
  def search_match(prefix, state) do
    with {:ok, password} <- password(state),
         {:ok, binary} <- cotp_binary(),
         {:ok, output} <- run_cotp(binary, ["list", "--json"], password, state) do
      output
      |> parse_entries()
      |> filter_entries(prefix)
    else
      _ -> []
    end
  end

  @impl true
  def do_action(candidate, state) do
    label = Backend.candidate_text(candidate)

    with {:ok, password} <- password(state),
         {:ok, binary} <- cotp_binary(),
         {:ok, _output} <- extract(binary, label, password, state) do
      notify("OTP for #{label} copied to clipboard")
    else
      {:error, :missing_password} ->
        notify("cotp password not found in SecretVault (key: #{pass_key(state)})")

      {:error, :cotp_not_found} ->
        notify("cotp binary not found in PATH")

      {:error, {code, _output}} ->
        notify("cotp extract for #{label} failed (exit #{inspect(code)})")

      {:error, reason} ->
        notify("cotp extract for #{label} failed: #{inspect(reason)}")
    end

    :ok
  end

  # Preview navigation (M-n/M-p) and parent (C-j) must not touch the clipboard.
  @impl true
  def select(_candidate, _state), do: :ok

  @impl true
  def parent(_candidate, _state), do: :ok

  @impl true
  def update(config, state) when is_list(config) do
    case config do
      [pass_key | rest] when is_binary(pass_key) ->
        state
        |> Map.put(:pass_key, pass_key)
        |> Map.put(:db_path, normalize_db_path(List.first(rest)))

      _ ->
        state
    end
  end

  def update(_config, state), do: state

  # ---------------------------------------------------------------------------
  # Helpers
  # ---------------------------------------------------------------------------

  # Interior shape: `%{text: label, search: "issuer label"}`. Only `:text`
  # survives into the render payload — `filter_entries/2` maps to plain
  # strings so nothing else leaks to Emacs.
  @doc false
  def parse_entries(output) when is_binary(output) do
    case Jason.decode(output) do
      {:ok, entries} when is_list(entries) ->
        Enum.flat_map(entries, fn
          %{"label" => label} = entry when is_binary(label) and label != "" ->
            issuer = Map.get(entry, "issuer", "")
            [%{text: label, search: "#{issuer} #{label}"}]

          _ ->
            []
        end)

      _ ->
        []
    end
  end

  @doc false
  def filter_entries(entries, prefix) do
    regex = Backend.fuzzy_regex(prefix)

    entries
    |> Enum.filter(&Backend.is_match?(prefix, regex, &1.search))
    |> Enum.map(& &1.text)
    |> Enum.sort()
    |> Enum.take(@max_candidates)
  end

  @doc false
  # `cotp extract` selectors are globs — escape metacharacters so a label from
  # the database cannot widen the match or inject a pattern.
  def escape_glob(text) when is_binary(text) do
    text
    |> String.replace("\\", "\\\\")
    |> String.replace("*", "\\*")
    |> String.replace("?", "\\?")
    |> String.replace("[", "\\[")
  end

  defp extract(binary, label, password, state) do
    args = ["extract", "-l", escape_glob(label), "-c"]
    run_cotp(binary, args, password, state)
  end

  defp run_cotp(binary, args, password, state) do
    # `--password-stdin` / `-d` are global flags and must precede the subcommand.
    # The password arrives on stdin so it never appears in argv.
    argv = [binary] ++ db_args(state) ++ ["--password-stdin"] ++ args

    Backend.run_capture(argv, input: [password])
  end

  defp password(state) do
    case :persistent_term.get(pass_key(state), "") do
      value when is_binary(value) and value != "" -> {:ok, String.trim(value)}
      _ -> {:error, :missing_password}
    end
  end

  defp pass_key(state), do: Map.get(state, :pass_key, @default_pass_key)

  defp db_args(state) do
    case Map.get(state, :db_path) do
      path when is_binary(path) and path != "" -> ["-d", Path.expand(path)]
      _ -> []
    end
  end

  defp normalize_db_path(path) when is_binary(path) and path != "", do: path
  defp normalize_db_path(_), do: nil

  defp cotp_binary do
    case System.find_executable("cotp") || default_binary() do
      nil -> {:error, :cotp_not_found}
      path -> {:ok, path}
    end
  end

  defp default_binary do
    path = "/opt/homebrew/bin/cotp"
    if File.exists?(path), do: path, else: nil
  end

  defp notify(text) do
    Exhub.send_message(~s|(message "[Blink-Search] #{Backend.escape_message(text)}")|)
  end
end
