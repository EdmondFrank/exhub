defmodule Exhub.Genclaw.LLMHelper do
  @moduledoc """
  Shared LLM utilities for GenClaw tools and middleware.

  Provides `call_llm/3`, `build_langchain_model/1`, and `extract_text/1`
  to eliminate duplication across code_scene_draft, code_text_draft,
  perception, and reason modules.
  """

  require Logger

  alias Exhub.Llm.LlmConfigServer

  @default_receive_timeout 180_000

  @doc """
  Call the default LLM with a system prompt and user prompt.

  Returns `{:ok, text}` or `{:error, reason, model_name}`.

  ## Options
    * `:receive_timeout` — LLM receive timeout in ms (default 180_000)
  """
  def call_llm(system_prompt, user_prompt, opts \\ []) do
    receive_timeout = Keyword.get(opts, :receive_timeout, @default_receive_timeout)

    case LlmConfigServer.get_default_llm_config() do
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

  @doc """
  Build a LangChain chat model struct from an LLM config map.

  ## Options
    * `:receive_timeout` — receive timeout in ms (default 180_000)
  """
  def build_langchain_model(config, opts \\ []) do
    receive_timeout = Keyword.get(opts, :receive_timeout, @default_receive_timeout)
    [provider, model_name] = String.split(config[:model], "/", parts: 2)

    base = %{model: model_name, api_key: config[:api_key], receive_timeout: receive_timeout}

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
