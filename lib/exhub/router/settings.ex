defmodule Exhub.Router.Settings do
  @moduledoc """
  Runtime JSON configuration for outbound router headers.

  Reads model/provider-specific custom outbound headers from a JSON file so
  they can be changed without recompiling `config.exs` or restarting Exhub.

  ## Configuration file path

  The path is selected in this order:

    1. `EXHUB_ROUTER_CONFIG`, when set to a non-empty value.
    2. `~/.config/exhub/router.json`.

  A missing file is treated as an empty configuration. Malformed JSON or
  invalid rule shapes are logged (without exposing header values) and ignored
  rather than taking the router down.

  ## Rule format

  The file is an object with a `headers` list. Each rule may carry `models`,
  `providers`, and `headers`:

      {
        "headers": [
          {
            "models": ["deepseek-*"],
            "providers": ["openai"],
            "headers": {"X-package-id": "8848"}
          }
        ]
      }

  - `models` / `providers` are optional; omitted lists are unrestricted.
  - Model patterns use glob semantics (`*` matches any characters).
  - Matching is case-insensitive.
  - Only string header names and string values are accepted.
  - Credential headers (`authorization`, `x-api-key`, `proxy-authorization`,
    and case-insensitive equivalents) are never emitted.

  ## Caching and reload

  The parsed rules are cached alongside the file's metadata (mtime and size).
  A lookup re-reads the file when the metadata changes. `reload/0` explicitly
  invalidates the cache for operational tooling and tests.
  """

  require Logger

  @cache_key {__MODULE__, :cache}

  # Header names that must always be controlled by Exhub and therefore can
  # never be supplied through the JSON configuration.
  @credential_headers ["authorization", "x-api-key", "proxy-authorization"]

  @doc """
  Resolves the router settings file path.

  Uses `EXHUB_ROUTER_CONFIG` when set and non-empty, otherwise the
  user-home default `~/.config/exhub/router.json`.
  """
  @spec path() :: String.t()
  def path do
    case System.get_env("EXHUB_ROUTER_CONFIG") do
      value when is_binary(value) and value != "" -> value
      _ -> Path.join(System.user_home!(), ".config/exhub/router.json")
    end
  end

  @doc """
  Returns the configured custom outbound headers matching `model` and
  `provider`.

  Multiple matching rules are merged case-insensitively, with later rules in
  the file taking precedence over earlier ones. Never includes credential
  headers.
  """
  @spec headers(String.t(), atom()) :: [{String.t(), String.t()}]
  def headers(model, provider) when is_binary(model) do
    provider = provider |> to_string() |> String.downcase()
    model = String.downcase(model)

    load_rules()
    |> Enum.filter(&matches?(&1, model, provider))
    |> Enum.reduce(%{}, fn rule, acc ->
      Enum.reduce(rule.headers, acc, fn {name, value}, m ->
        Map.put(m, String.downcase(name), {name, value})
      end)
    end)
    |> Map.values()
  end

  @doc """
  Explicitly invalidates the cached settings so the next lookup re-reads the
  configuration file.
  """
  @spec reload() :: :ok
  def reload do
    :persistent_term.erase(@cache_key)
    :ok
  end

  # --- loading and caching ---

  defp load_rules do
    path = path()

    case cached(path) do
      :stale ->
        rules = parse_file(path)
        :persistent_term.put(@cache_key, {path, metadata(path), rules})
        rules

      rules ->
        rules
    end
  end

  defp cached(path) do
    case :persistent_term.get(@cache_key, :miss) do
      :miss ->
        :stale

      {^path, meta, rules} ->
        if unchanged?(meta, path), do: rules, else: :stale

      {_other_path, _meta, _rules} ->
        :stale
    end
  end

  defp metadata(path) do
    case File.stat(path) do
      {:ok, stat} -> {stat.mtime, stat.size}
      _ -> :missing
    end
  end

  defp unchanged?(meta, path) do
    case {meta, File.stat(path)} do
      {:missing, {:error, :enoent}} -> true
      {:missing, _} -> false
      {{mtime, size}, {:ok, stat}} -> stat.mtime == mtime and stat.size == size
      _ -> false
    end
  end

  defp parse_file(path) do
    case File.read(path) do
      {:error, :enoent} ->
        []

      {:error, reason} ->
        Logger.warning("Unable to read router settings file #{path}: #{inspect(reason)}")
        []

      {:ok, content} ->
        parse_content(content)
    end
  end

  defp parse_content(content) do
    case Jason.decode(content) do
      {:ok, %{"headers" => rules}} when is_list(rules) ->
        Enum.flat_map(rules, fn rule ->
          case parse_rule(rule) do
            nil -> []
            parsed -> [parsed]
          end
        end)

      {:ok, _other} ->
        Logger.warning("Router settings file must contain a \"headers\" list")
        []

      {:error, error} ->
        Logger.warning("Invalid JSON in router settings: #{inspect(error)}")
        []
    end
  end

  defp parse_rule(%{"headers" => headers} = rule) when is_map(headers) and map_size(headers) > 0 do
    with {:ok, model_patterns} <- parse_patterns(rule, :models),
         {:ok, provider_patterns} <- parse_patterns(rule, :providers),
         {:ok, rule_headers} <- sanitize_headers(headers) do
      %{models: model_patterns, providers: provider_patterns, headers: rule_headers}
    else
      :invalid -> nil
    end
  end
  defp parse_rule(_), do: nil
  # Parses the `models`/`providers` lists. `:all` means unrestricted. Models
  # become glob regexes; providers are exact (case-insensitive) strings.
  # Returns {:ok, value} or :invalid.
  defp parse_patterns(rule, :providers) do
    case Map.get(rule, "providers") do
      nil ->
        {:ok, :all}

      list when is_list(list) ->
        if Enum.all?(list, &is_binary/1) do
          {:ok, Enum.map(list, &String.downcase/1)}
        else
          :invalid
        end

      _ ->
        :invalid
    end
  end

  defp parse_patterns(rule, :models) do
    case Map.get(rule, "models") do
      nil ->
        {:ok, :all}

      list when is_list(list) ->
        compiled =
          Enum.map(list, fn
            pattern when is_binary(pattern) -> compile_glob(pattern)
            _ -> :invalid
          end)

        if Enum.any?(compiled, &(&1 == :invalid)) do
          :invalid
        else
          {:ok, Enum.map(compiled, &elem(&1, 1))}
        end

      _ ->
        :invalid
    end
  end

  # Converts a glob pattern to a case-insensitive anchored regex. Providers are
  # exact matches; models support `*` wildcards.
  defp compile_glob(pattern) do
    source =
      pattern
      |> String.downcase()
      |> Regex.escape()
      |> String.replace("\\*", ".*")

    case Regex.compile("^" <> source <> "$") do
      {:ok, regex} -> {:ok, regex}
      {:error, _} -> :invalid
    end
  end

  defp sanitize_headers(headers) do
    result =
      Enum.reduce(headers, [], fn {name, value}, acc ->
        if is_binary(name) and is_binary(value) and not credential_header?(name) do
          [{name, value} | acc]
        else
          acc
        end
      end)

    case result do
      [] -> :invalid
      list -> {:ok, Enum.reverse(list)}
    end
  end

  defp credential_header?(name) do
    String.downcase(name) in @credential_headers
  end

  # --- matching ---

  defp matches?(%{models: models, providers: providers}, model, provider) do
    model_ok? =
      case models do
        :all -> true
        compiled -> Enum.any?(compiled, &Regex.match?(&1, model))
      end

    provider_ok? =
      case providers do
        :all -> true
        list -> provider in list
      end

    model_ok? and provider_ok?
  end
end