defmodule Exhub.BlinkSearch.Backends.Envless do
  @moduledoc """
  Envless backend — lists keys from an envless vault.

  Security model (mirrors the `envless` skill):

  - `search_match/2` only ever runs `envless list`, which prints key names,
    never values.
  - `do_action/2` sends the value to the clipboard through
    `envless exec -- sh -c 'printf %s "$KEY" | pbcopy'`: the plaintext flows
    child env → clipboard and never touches stdout, argv, logs, or the elisp
    payload sent back to Emacs.
  - Key names are validated before being interpolated into the shell snippet,
    so a crafted candidate cannot inject commands.

  Configuration (pushed from Emacs via `update`):
  - `root` — vault directory (`ENVLESS_ROOT`, default `~/GTD`)
  - `env` — envless environment name (default `"dev"`)
  """

  use Exhub.BlinkSearch.Backend

  alias Exhub.BlinkSearch.Backend

  @default_root Path.join([System.user_home!(), "GTD"])
  @default_env "dev"
  @env_var "ENVLESS_ROOT"
  @key_regex ~r/^[A-Za-z_][A-Za-z0-9_]*$/
  @max_candidates 50

  @impl true
  def search_match(prefix, state) do
    with {:ok, binary} <- envless_binary(),
         {:ok, output} <- Backend.run_capture([binary, "list" | env_args(state)], run_opts(state)) do
      output
      |> parse_keys()
      |> filter_keys(prefix)
    else
      _ -> []
    end
  end

  @impl true
  def do_action(candidate, state) do
    key = Backend.candidate_text(candidate)

    with {:ok, binary} <- envless_binary(),
         :ok <- validate_key(key),
         {:ok, _output} <- copy_value(binary, key, state) do
      notify("Copied value of #{key} to clipboard")
    else
      {:error, :invalid_key} ->
        notify("Refusing unsafe envless key: #{key}")

      {:error, :envless_not_found} ->
        notify("envless binary not found in PATH")

      {:error, {code, _output}} ->
        notify("envless exec for #{key} failed (exit #{inspect(code)})")

      {:error, reason} ->
        notify("envless exec for #{key} failed: #{inspect(reason)}")
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
      [root | rest] when is_binary(root) ->
        state
        |> Map.put(:root, root)
        |> Map.put(:env, normalize_env(List.first(rest)))

      _ ->
        state
    end
  end

  def update(_config, state), do: state

  # ---------------------------------------------------------------------------
  # Helpers
  # ---------------------------------------------------------------------------

  @doc false
  def parse_keys(output) when is_binary(output) do
    output
    |> String.split("\n", trim: true)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
  end

  @doc false
  def filter_keys(keys, prefix) do
    keys
    |> Backend.filter_match(prefix)
    |> Enum.sort()
    |> Enum.take(@max_candidates)
  end

  @doc false
  def validate_key(key) do
    if is_binary(key) and Regex.match?(@key_regex, key) do
      :ok
    else
      {:error, :invalid_key}
    end
  end

  # Copy the key's value to the clipboard without it ever hitting stdout:
  # `printf` (a shell builtin) expands the envless-injected variable and pipes
  # it straight to `pbcopy`.
  defp copy_value(binary, key, state) do
    snippet = ~s(printf %s "$#{key}" | pbcopy)

    Backend.run_capture(
      [binary, "exec" | env_args(state)] ++ ["--", "sh", "-c", snippet],
      run_opts(state)
    )
  end

  defp run_opts(state) do
    [env: [{@env_var, Path.expand(root(state))}]]
  end

  defp root(state), do: Map.get(state, :root, @default_root)

  defp env_args(state) do
    case Map.get(state, :env, @default_env) do
      env when is_binary(env) and env != "" -> ["--env=#{env}"]
      _ -> []
    end
  end

  defp normalize_env(env) when is_binary(env) and env != "", do: env
  defp normalize_env(_), do: @default_env

  defp envless_binary do
    case System.find_executable("envless") || default_binary() do
      nil -> {:error, :envless_not_found}
      path -> {:ok, path}
    end
  end

  defp default_binary do
    path = Path.join([System.user_home!(), ".cargo", "bin", "envless"])
    if File.exists?(path), do: path, else: nil
  end

  defp notify(text) do
    Exhub.send_message(~s|(message "[Blink-Search] #{Backend.escape_message(text)}")|)
  end
end
