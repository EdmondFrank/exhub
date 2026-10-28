defmodule Exhub.Genclaw.LLMHelper do
  @moduledoc """
  Shared LLM utilities for GenClaw tools and middleware.

  Provides `call_llm/3`, `build_langchain_model/1`, and `extract_text/1`
  to eliminate duplication across code_scene_draft, code_text_draft,
  perception, and reason modules.
  """

  require Logger

  alias Exhub.Llm.LlmConfigServer
  alias Exhub.TLSCompat

  @default_receive_timeout 180_000

  @doc """
  Call the default LLM with a system prompt and user prompt.

  Returns `{:ok, text}` or `{:error, reason, model_name}`.

  ## Options
    * `:receive_timeout` — LLM receive timeout in ms (default 180_000)
    * `:model` — an LLM name from `Exhub.Llm.LlmConfigServer`, or a bare model
      name such as `"deepseek-v4.1-flash"`. Defaults to the current default LLM.
  """
  def call_llm(system_prompt, user_prompt, opts \\ []) do
    receive_timeout = Keyword.get(opts, :receive_timeout, @default_receive_timeout)

    case resolve_llm_config(Keyword.get(opts, :model)) do
      {:ok, llm_config} ->
        model = build_langchain_model(llm_config, receive_timeout: receive_timeout)

        messages = [
          LangChain.Message.new_system!(system_prompt),
          LangChain.Message.new_user!(user_prompt)
        ]

        chain =
          LangChain.Chains.LLMChain.new!(%{llm: model})
          |> LangChain.Chains.LLMChain.add_messages(messages)

        case LangChain.Chains.LLMChain.run(chain) do
          {:ok, chain} ->
            case List.last(chain.messages) do
              %LangChain.Message{role: :assistant, content: content} ->
                {:ok, extract_text(content)}

              _ ->
                {:error, "no assistant response", llm_config[:model]}
            end

          {:error, _chain, reason} ->
            {:error, reason, llm_config[:model]}

          {:error, reason} ->
            {:error, reason, llm_config[:model]}
        end

      {:error, e} ->
        {:error, e, nil}
    end
  end

  # `:model` may name an LLM config verbatim (`openai/deepseek-v4.1-flash`) or
  # only its model (`deepseek-v4.1-flash`, the form `toonflow.agents.*` uses).
  # Try the exact key, then the Gitee AI provider prefix, then any key whose
  # model part matches, so a bare configured name still resolves.
  defp resolve_llm_config(nil), do: LlmConfigServer.get_default_llm_config()

  defp resolve_llm_config(name) when is_binary(name) do
    case String.trim(name) do
      "" ->
        LlmConfigServer.get_default_llm_config()

      name ->
        candidates =
          [name, "openai/" <> name] ++
            Enum.filter(LlmConfigServer.list_llm_names(), &String.ends_with?(&1, "/" <> name))

        Enum.find_value(
          candidates,
          {:error, "LLM '#{name}' not found in configuration"},
          fn candidate ->
            case LlmConfigServer.get_llm_config(candidate) do
              {:ok, config} -> {:ok, config}
              _ -> nil
            end
          end
        )
    end
  end

  defp resolve_llm_config(_name), do: LlmConfigServer.get_default_llm_config()

  @doc """
  Build a LangChain chat model struct from an LLM config map.

  ## Options
    * `:receive_timeout` — receive timeout in ms (default 180_000)
  """
  def build_langchain_model(config, opts \\ []) do
    receive_timeout = Keyword.get(opts, :receive_timeout, @default_receive_timeout)
    [provider, model_name] = String.split(config[:model], "/", parts: 2)

    base = %{model: model_name, api_key: config[:api_key], receive_timeout: receive_timeout}

    # Merged into the model's Req request; carries the TLS compat verify_fun
    # down to Mint. Empty for chat structs that define no `req_config` field.
    base = Map.put_new(base, :req_config, TLSCompat.langchain_req_config(provider))

    case provider do
      "google" ->
        Map.put(base, :endpoint, config[:api_base])
        |> LangChain.ChatModels.ChatGoogleAI.new!()

      "anthropic" ->
        Map.put(base, :endpoint, "#{config[:api_base]}/messages")
        |> LangChain.ChatModels.ChatAnthropic.new!()

      _ ->
        Map.put(base, :endpoint, "#{config[:api_base]}/chat/completions")
        |> LangChain.ChatModels.ChatOpenAI.new!()
    end
  end

  @doc """
  Extract plain text from various LangChain message content formats.
  """
  def extract_text(content) when is_binary(content), do: content

  def extract_text(content) when is_list(content) do
    content
    |> Enum.map_join("", fn
      text when is_binary(text) -> text
      %LangChain.Message.ContentPart{type: :text, content: c} -> c || ""
      %LangChain.Message.ContentPart{content: c} when is_binary(c) -> c
      %{content: c} when is_binary(c) -> c
      _ -> ""
    end)
  end

  def extract_text(%LangChain.Message.ContentPart{type: :text, content: c}), do: c || ""
  def extract_text(%LangChain.Message.ContentPart{content: c}) when is_binary(c), do: c
  def extract_text(_), do: ""
end
