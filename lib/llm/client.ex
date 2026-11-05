defmodule Exhub.Llm.Client do
  @moduledoc """
  Canonical LangChain facade for ExHub.

  Resolves a model name against `Exhub.Llm.LlmConfigServer`, builds the matching
  `LangChain.ChatModels.*` struct (with the TLS compat `req_config` and the
  token-pool key), and runs a single `LLMChain` turn.

  `chat/2` is the entry point behind the `llm` MCP server and, through it, Lua
  code-mode snippets:

      llm.chat({model = "kimi-k2.6", prompt = "Say hi", system = "Be brief."})

  Model names may be the full configured name (`openai/deepseek-v4.1-flash`), a
  bare model (`kimi-k2.6`), or a variant-suffixed name
  (`openai/qwen3.8-flash--no-think`); an omitted name uses the current default.

  ## Request map

    * `:model` — optional model name (see above)
    * `:messages` — list of `{role, content}` objects; `role` is `system`,
      `user`, or `assistant`
    * `:prompt` — single user message, when `:messages` is absent
    * `:system` — system prompt prepended when no system message is present
    * `:temperature`, `:max_tokens`, `:timeout_ms` — per-call overrides
    * `:json_schema` — JSON Schema for structured output (`chat_json/2`)
    * `:strict` — strict structured output (default `true`)

  Content may be a string or a list of content parts:

      {"type": "text", "text": "..."}
      {"type": "image_url", "url": "https://..."}   -- detail optional
      {"type": "image", "data": "<base64>", "media": "image/png"}

  Tool calling from Lua is intentionally unsupported: a Lua snippet already
  calls the real MCP tools directly, so there is no need to round-trip them
  through the model.

  ## Guardrails

  Config lives under `config :exhub, :llm_tool`:

    * `:enabled` — master switch (default `true`)
    * `:allowed_models` — `nil` (any configured model) or a list of names,
      full or bare, that `resolve/1` accepts
    * `:default_max_tokens` / `:default_timeout_ms` / `:max_timeout_ms`
  """

  alias Exhub.Llm.LlmConfigServer
  alias Exhub.Router.{ModelVariant, TokenPool}
  alias Exhub.TLSCompat

  alias LangChain.Chains.LLMChain
  alias LangChain.ChatModels.{ChatAnthropic, ChatGoogleAI, ChatMistralAI, ChatOpenAI}
  alias LangChain.LangChainError
  alias LangChain.Message
  alias LangChain.Message.ContentPart
  alias LangChain.TokenUsage
  alias LangChain.Utils.ChainResult

  @default_timeout_ms 180_000
  @max_timeout_ms 600_000
  @default_max_tokens 4096

  # Providers whose LangChain struct exposes structured-output fields
  # (`json_response` + `json_schema`). Anthropic, Mistral & co. fall back to a
  # schema instruction appended to the user message.
  @structured_providers ~w(openai google)

  @type clean_result :: %{
          content: String.t(),
          model: String.t(),
          usage: map() | nil
        }

  # --- Configuration ---

  @doc "The effective `:exhub, :llm_tool` configuration."
  @spec config() :: keyword()
  def config, do: Application.get_env(:exhub, :llm_tool, [])

  @doc "Whether the `llm` tool surface is enabled (default `true`)."
  @spec enabled?() :: boolean()
  def enabled?, do: Keyword.get(config(), :enabled, true) != false

  @doc "The configured allow-list, or `nil` when every model is allowed."
  @spec allowed_models() :: [String.t()] | nil
  def allowed_models do
    case Keyword.get(config(), :allowed_models) do
      nil -> nil
      list when is_list(list) -> Enum.map(list, &to_string/1)
      other -> [to_string(other)]
    end
  end

  # --- Models ---

  @doc """
  Every model configured in `Exhub.Llm.LlmConfigServer`, plus the current
  default. API keys and base URLs are never included.
  """
  @spec models() :: %{
          default: String.t() | nil,
          models: [%{name: String.t(), provider: String.t() | nil, model: String.t() | nil}],
          count: non_neg_integer(),
          enabled: boolean(),
          allowed_models: [String.t()] | nil
        }
  def models do
    default = safe(fn -> LlmConfigServer.get_default_llm_name() end)
    names = safe(fn -> LlmConfigServer.list_llm_names() end, [])

    entries =
      names
      |> Enum.map(&model_entry/1)
      |> Enum.reject(&is_nil/1)

    %{
      default: default,
      models: entries,
      count: length(entries),
      enabled: enabled?(),
      allowed_models: allowed_models()
    }
  end

  defp model_entry(name) do
    case safe(fn -> LlmConfigServer.get_llm_config(name) end) do
      {:ok, %{model: model}} ->
        {provider, bare} = split_model(model)
        %{name: name, provider: provider, model: bare}

      _ ->
        %{name: name, provider: nil, model: nil}
    end
  end

  @doc """
  Resolves a model name to `{:ok, name, config}`.

  Accepts a full configured name, a bare model name, a variant-suffixed name,
  or `nil`/`""` for the current default. Enforces the `:allowed_models`
  allow-list.
  """
  @spec resolve(String.t() | nil) ::
          {:ok, String.t(), map()} | {:error, String.t()}
  def resolve(name \\ nil)

  def resolve(nil), do: default_resolved()

  def resolve(name) when is_binary(name) do
    case String.trim(name) do
      "" -> default_resolved()
      name -> resolve_name(name)
    end
  end

  def resolve(_name), do: default_resolved()

  defp default_resolved do
    case safe(fn -> LlmConfigServer.get_default_llm_config() end) do
      {:ok, config} ->
        check_allowed(safe(fn -> LlmConfigServer.get_default_llm_name() end), config)

      {:error, reason} ->
        {:error, to_string(reason)}

      _ ->
        {:error, "LLM configuration is not available (LlmConfigServer not running?)"}
    end
  end

  defp resolve_name(name) do
    known = safe(fn -> LlmConfigServer.list_llm_names() end, [])

    candidates =
      [
        name,
        "openai/" <> name,
        ModelVariant.normalize(name),
        "openai/" <> ModelVariant.normalize(name)
      ] ++
        Enum.filter(known, fn key ->
          String.ends_with?(key, "/" <> name) or
            String.ends_with?(key, "/" <> ModelVariant.normalize(name))
        end)

    candidates
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
    |> Enum.find_value({:error, "LLM '#{name}' not found in configuration"}, fn candidate ->
      case safe(fn -> LlmConfigServer.get_llm_config(candidate) end) do
        {:ok, config} -> check_allowed(candidate, config)
        _ -> nil
      end
    end)
  end

  defp check_allowed(name, config) do
    case allowed_models() do
      nil ->
        {:ok, name, config}

      [] ->
        {:error, "LLM calls are disabled (`allowed_models` is empty)"}

      allowed ->
        bare = bare_name(name)
        configured = config[:model]
        configured_bare = configured && bare_name(configured)

        if name in allowed or bare in allowed or configured in allowed or
             configured_bare in allowed do
          {:ok, name, config}
        else
          {:error, "LLM '#{name}' is not in the allowed list (#{Enum.join(allowed, ", ")})"}
        end
    end
  end

  # The part of a configured name after the provider prefix (`openai/kimi-k2.6`
  # → `kimi-k2.6`), which is what an allow-list usually holds.
  defp bare_name(nil), do: nil
  defp bare_name(name), do: name |> to_string() |> String.split("/") |> List.last()

  # --- Chat ---

  @doc """
  Runs one chat turn.

  Returns `{:ok, %{content, model, usage}}` or `{:error, reason}`. `usage` is
  `%{input, output, total}` when the provider reports token usage.
  """
  @spec chat(map(), keyword()) :: {:ok, clean_result()} | {:error, String.t()}
  def chat(request, opts \\ [])

  def chat(request, opts) when is_map(request) do
    with :ok <- ensure_enabled(),
         {:ok, name, config} <- resolve(get(request, :model) || Keyword.get(opts, :model)) do
      {provider, _bare} = split_model(config[:model])
      schema = get(request, :json_schema)
      hint = if is_map(schema) and provider not in @structured_providers, do: schema_hint(schema)

      with {:ok, messages} <- build_messages(request, hint),
           {:ok, model} <- build_chat_model(config, request) do
        case run_chain(model, messages, config) do
          {:ok, %{content: content, usage: usage}} ->
            {:ok, %{content: content, model: name, usage: usage}}

          {:error, reason} ->
            {:error, reason}
        end
      end
    end
  end

  def chat(_request, _opts), do: {:error, "`request` must be a map"}

  @doc """
  Runs one chat turn constrained by a JSON Schema.

  Returns `{:ok, %{content, model, usage, json}}`; `json` is the decoded
  response when the content is valid JSON, otherwise `nil` (the raw `content`
  is always returned).
  """
  @spec chat_json(map(), keyword()) :: {:ok, map()} | {:error, String.t()}
  def chat_json(request, opts \\ [])

  def chat_json(request, opts) when is_map(request) do
    with {:ok, schema} <- normalize_schema(get(request, :schema) || get(request, :json_schema)) do
      request = request |> Map.put(:json_schema, schema) |> Map.put_new(:strict, true)

      case chat(request, opts) do
        {:ok, result} ->
          json =
            case Jason.decode(result.content) do
              {:ok, decoded} -> decoded
              {:error, _} -> nil
            end

          {:ok, Map.put(result, :json, json)}

        {:error, reason} ->
          {:error, reason}
      end
    end
  end

  def chat_json(_request, _opts), do: {:error, "`request` must be a map"}

  # --- Messages ---

  @doc """
  Converts a request map into LangChain messages.

  Exposed for tests and callers that want to validate a conversation without
  spending an LLM call.
  """
  @spec build_messages(map(), String.t() | nil) ::
          {:ok, [Message.t()]} | {:error, String.t()}
  def build_messages(request, hint \\ nil)

  def build_messages(request, hint) when is_map(request) do
    system = text(get(request, :system))
    prompt = text(get(request, :prompt) || get(request, :user) || get(request, :message))
    messages = decode_messages(get(request, :messages))

    result =
      cond do
        is_list(messages) and messages != [] ->
          to_messages(messages)

        is_binary(prompt) and prompt != "" ->
          to_messages([%{"role" => "user", "content" => prompt}])

        is_nil(messages) ->
          {:error, "`messages` (array) or `prompt` (string) is required"}

        true ->
          {:error, "`messages` must be a non-empty array of {role, content} objects"}
      end

    with {:ok, messages} <- result do
      {:ok, messages |> maybe_prepend_system(system) |> maybe_append_hint(hint)}
    end
  end

  def build_messages(_request, _hint), do: {:error, "`request` must be a map"}

  defp decode_messages(messages) when is_binary(messages) do
    case Jason.decode(messages) do
      {:ok, decoded} -> decoded
      {:error, _} -> messages
    end
  end

  defp decode_messages(messages), do: messages

  defp to_messages(entries) do
    entries
    |> Enum.reduce_while({:ok, []}, fn entry, {:ok, acc} ->
      case to_message(entry) do
        {:ok, message} -> {:cont, {:ok, [message | acc]}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
    |> case do
      {:ok, messages} -> {:ok, Enum.reverse(messages)}
      error -> error
    end
  end

  defp to_message(%{} = entry) do
    role = entry |> get(:role) |> default("user") |> to_string() |> String.downcase()

    with {:ok, role} <- to_role(role),
         {:ok, content} <- to_content(get(entry, :content)) do
      {:ok, Message.new!(%{role: role, content: content})}
    end
  rescue
    e -> {:error, Exception.message(e)}
  end

  defp to_message(_), do: {:error, "each message must be an object with `role` and `content`"}

  defp to_role(role) when role in ["system", "developer"], do: {:ok, :system}
  defp to_role(role) when role in ["user", "human"], do: {:ok, :user}
  defp to_role(role) when role in ["assistant", "ai", "model"], do: {:ok, :assistant}

  defp to_role(role),
    do: {:error, "unsupported message role #{inspect(role)} (use system, user or assistant)"}

  defp to_content(nil), do: {:error, "message content is required"}
  defp to_content(content) when is_binary(content), do: {:ok, content}

  defp to_content(%{} = part), do: to_content([part])

  defp to_content(content) when is_list(content) do
    if content == [] do
      {:error, "message content must not be empty"}
    else
      content
      |> Enum.reduce_while({:ok, []}, fn part, {:ok, acc} ->
        case to_content_part(part) do
          {:ok, content_part} -> {:cont, {:ok, [content_part | acc]}}
          {:error, reason} -> {:halt, {:error, reason}}
        end
      end)
      |> case do
        {:ok, parts} -> {:ok, Enum.reverse(parts)}
        error -> error
      end
    end
  end

  defp to_content(_),
    do: {:error, "message content must be a string or a list of content parts"}

  defp to_content_part(%{} = part) do
    case part |> get(:type) |> to_string_or_nil() do
      "text" ->
        {:ok, ContentPart.text!(get(part, :text) || get(part, :content) || "")}

      "image_url" ->
        with {:ok, url} <- image_url(part) do
          opts = detail_opts(part)
          {:ok, ContentPart.image_url!(url, opts)}
        end

      "image" ->
        with {:ok, data} <- image_data(part) do
          opts = part |> get(:media) |> media_opts()
          {:ok, ContentPart.image!(data, opts)}
        end

      other ->
        {:error, "unsupported content part type #{inspect(other)}"}
    end
  rescue
    e -> {:error, Exception.message(e)}
  end

  defp to_content_part(_),
    do: {:error, "each content part must be an object with a `type`"}

  defp image_url(part) do
    case get(part, :url) || get(part, :content) || get(part, :image_url) do
      %{} = nested -> string_or_error(get(nested, :url), "image_url.url is required")
      url -> string_or_error(url, "image_url (`url` or `image_url`) is required")
    end
  end

  defp image_data(part) when is_map(part) do
    part
    |> then(&(get(&1, :data) || get(&1, :content) || get(&1, :image)))
    |> string_or_error("base64 `data` is required for an image content part")
  end

  defp string_or_error(value, _message) when is_binary(value) and value != "", do: {:ok, value}
  defp string_or_error(_value, message), do: {:error, message}

  defp detail_opts(part) do
    case get(part, :detail) do
      detail when detail in ["low", "high", "auto"] -> [detail: detail]
      _ -> []
    end
  end

  defp media_opts(nil), do: []
  defp media_opts(""), do: []
  defp media_opts(media), do: [media: to_string(media)]

  # --- Model building ---

  @doc """
  Builds the LangChain chat model struct for a resolved config.

  Exposed for tests (the TLS `req_config` assertion) and for callers that need
  the struct without running a turn.
  """
  @spec build_chat_model(map(), map()) :: {:ok, struct()} | {:error, String.t()}
  def build_chat_model(config, request \\ %{})

  def build_chat_model(config, request) when is_map(config) and is_map(request) do
    cond do
      is_nil(config[:model]) ->
        {:error, "LLM config has no `model`"}

      is_nil(config[:api_base]) ->
        {:error, "LLM config has no `api_base`"}

      true ->
        do_build_chat_model(config, request)
    end
  end

  def build_chat_model(_config, _request), do: {:error, "`config` must be a map"}

  defp do_build_chat_model(config, request) do
    {provider, model_name} = split_model(config[:model])
    timeout_ms = clamp_timeout(get(request, :timeout_ms))
    max_tokens = positive_int(get(request, :max_tokens))
    temperature = number(get(request, :temperature))
    schema = get(request, :json_schema)
    strict = get(request, :strict) != false

    base = %{model: model_name, api_key: config[:api_key], receive_timeout: timeout_ms}
    base = if is_integer(max_tokens), do: Map.put(base, :max_tokens, max_tokens), else: base
    base = if is_number(temperature), do: Map.put(base, :temperature, temperature), else: base
    base = Map.put_new(base, :req_config, TLSCompat.langchain_req_config(provider))
    base = put_structured_output(base, provider, schema, strict)

    model =
      case provider do
        "google" ->
          base |> Map.put(:endpoint, config[:api_base]) |> ChatGoogleAI.new!()

        "anthropic" ->
          base |> Map.put(:endpoint, "#{config[:api_base]}/messages") |> ChatAnthropic.new!()

        "mistral" ->
          base
          |> Map.put(:endpoint, "#{config[:api_base]}/chat/completions")
          |> ChatMistralAI.new!()

        _ ->
          base
          |> Map.put(:endpoint, "#{config[:api_base]}/chat/completions")
          |> ChatOpenAI.new!()
      end

    {:ok, model}
  rescue
    e -> {:error, Exception.message(e)}
  end

  # OpenAI-compatible structs wrap the schema in the `json_schema` envelope;
  # Google takes the bare schema. Anthropic/Mistral have no such fields and are
  # handled by the schema hint appended to the user message.
  defp put_structured_output(base, _provider, nil, _strict), do: base
  defp put_structured_output(base, _provider, schema, _strict) when not is_map(schema), do: base

  defp put_structured_output(base, "google", schema, _strict) do
    base
    |> Map.put(:json_response, true)
    |> Map.put(:json_schema, schema)
  end

  defp put_structured_output(base, provider, schema, strict)
       when provider in @structured_providers do
    base
    |> Map.put(:json_response, true)
    |> Map.put(:json_schema, %{
      "name" => "exhub_response",
      "schema" => schema,
      "strict" => strict
    })
  end

  defp put_structured_output(base, _provider, _schema, _strict), do: base

  # --- Chain execution ---

  defp run_chain(model, messages, config) do
    model = maybe_apply_token_pool(model, messages, config)

    chain = LLMChain.new!(%{llm: model}) |> LLMChain.add_messages(messages)

    case LLMChain.run(chain, mode: :while_needs_response) do
      {:ok, %LLMChain{} = chain} ->
        case ChainResult.to_string(chain) do
          {:ok, text} -> {:ok, %{content: text, usage: usage(chain)}}
          {:error, _chain, %LangChainError{message: message}} -> {:error, message}
          other -> {:error, format_reason(other)}
        end

      {:error, _chain, %LangChainError{message: message}} ->
        {:error, message}

      {:error, reason} ->
        {:error, format_reason(reason)}
    end
  rescue
    e in LangChainError -> {:error, e.message}
    e -> {:error, Exception.message(e)}
  catch
    kind, reason -> {:error, format_reason({kind, reason})}
  end

  # Swaps the api_key for the token-pool key when the pool applies. The pool is
  # keyed on the bare model name (same call `Exhub.Llm.Chain` makes).
  defp maybe_apply_token_pool(%{model: model} = chat_model, messages, config) do
    case TokenPool.resolve_langchain_key(model, messages, fallback: config[:api_key]) do
      key when is_binary(key) and key != "" -> Map.put(chat_model, :api_key, key)
      _ -> chat_model
    end
  end

  defp maybe_apply_token_pool(model, _messages, _config), do: model

  defp usage(%LLMChain{last_message: last_message}) do
    case TokenUsage.get(last_message) do
      %TokenUsage{input: input, output: output} ->
        input = input || 0
        output = output || 0
        %{input: input, output: output, total: input + output}

      _ ->
        nil
    end
  end

  defp usage(_chain), do: nil

  # --- Small helpers ---

  defp ensure_enabled do
    if enabled?() do
      :ok
    else
      {:error, "LLM calls are disabled (config :exhub, :llm_tool, enabled: false)"}
    end
  end

  defp normalize_schema(nil),
    do: {:error, "`schema` is required — provide a JSON Schema object"}

  defp normalize_schema(schema) when is_binary(schema) do
    case Jason.decode(schema) do
      {:ok, %{} = decoded} -> {:ok, decoded}
      {:ok, _other} -> {:error, "`schema` must be a JSON object"}
      {:error, _} -> {:error, "`schema` is not valid JSON"}
    end
  end

  defp normalize_schema(%{} = schema), do: {:ok, schema}

  defp normalize_schema(_schema),
    do: {:error, "`schema` must be a JSON Schema object (or a JSON object string)"}

  defp schema_hint(schema) do
    "Respond with only a single JSON value that conforms to this JSON Schema. " <>
      "No prose, no code fences.\n" <> Jason.encode!(schema)
  end

  defp maybe_prepend_system(messages, nil), do: messages
  defp maybe_prepend_system(messages, ""), do: messages

  defp maybe_prepend_system(messages, system) do
    if Enum.any?(messages, &(&1.role == :system)) do
      messages
    else
      [Message.new_system!(system) | messages]
    end
  end

  defp maybe_append_hint(messages, nil), do: messages
  defp maybe_append_hint(messages, ""), do: messages

  defp maybe_append_hint(messages, hint) do
    case Enum.reverse(messages) do
      [%Message{role: :user} = last | rest] ->
        Enum.reverse([%{last | content: append_text(last.content, hint)} | rest])

      _ ->
        messages ++ [Message.new_user!(hint)]
    end
  end

  defp append_text(content, addition) when is_binary(content), do: content <> "\n\n" <> addition

  defp append_text(content, addition) when is_list(content) do
    content ++ [ContentPart.text!(addition)]
  end

  defp append_text(_content, addition), do: addition

  defp split_model(nil), do: {"openai", nil}

  defp split_model(model) when is_binary(model) do
    case String.split(model, "/", parts: 2) do
      [provider, name] -> {provider, name}
      [name] -> {"openai", name}
    end
  end

  defp clamp_timeout(nil), do: Keyword.get(config(), :default_timeout_ms, @default_timeout_ms)

  defp clamp_timeout(value) when is_integer(value) and value > 0 do
    max_timeout = Keyword.get(config(), :max_timeout_ms, @max_timeout_ms)
    value |> max(1_000) |> min(max_timeout)
  end

  defp clamp_timeout(_value), do: Keyword.get(config(), :default_timeout_ms, @default_timeout_ms)

  defp positive_int(value) when is_integer(value) and value > 0, do: value
  defp positive_int(value) when is_float(value) and value > 0, do: trunc(value)
  defp positive_int(_value), do: Keyword.get(config(), :default_max_tokens, @default_max_tokens)

  defp number(value) when is_number(value), do: value
  defp number(_value), do: nil

  defp text(nil), do: nil

  defp text(value) when is_binary(value) do
    case String.trim(value) do
      "" -> nil
      trimmed -> trimmed
    end
  end

  defp text(value), do: to_string(value)

  defp to_string_or_nil(nil), do: nil
  defp to_string_or_nil(""), do: nil
  defp to_string_or_nil(value) when is_binary(value), do: value
  defp to_string_or_nil(value), do: to_string(value)

  defp default(nil, value), do: value
  defp default(value, _default), do: value

  # Reads an atom key, then its string form, so maps from Anubis (atom keys)
  # and from JSON/Lua (string keys) behave the same.
  defp get(map, key) when is_map(map) do
    case Map.fetch(map, key) do
      {:ok, value} -> value
      :error -> Map.get(map, to_string(key))
    end
  end

  defp get(_map, _key), do: nil

  defp format_reason(%LangChainError{message: message}), do: message
  defp format_reason(reason) when is_binary(reason), do: reason
  defp format_reason(reason), do: inspect(reason)

  defp safe(fun, default \\ nil) do
    fun.()
  rescue
    _ -> default
  catch
    _, _ -> default
  end
end
