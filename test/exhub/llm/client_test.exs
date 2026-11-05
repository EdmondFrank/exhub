Code.require_file("../../support/llm_stub_provider.ex", __DIR__)

defmodule Exhub.Llm.ClientTest do
  use ExUnit.Case, async: false

  alias Exhub.Llm.Client
  alias Exhub.Llm.LlmConfigServer
  alias LangChain.Message.ContentPart

  @llms %{
    "openai/qwen3.8-flash" => %{
      api_base: "https://api.moark.com/v1/",
      api_key: "test-openai",
      model: "openai/qwen3.8-flash"
    },
    "openai/kimi-k2.6" => %{
      api_base: "https://api.moark.com/v1/",
      api_key: "test-kimi",
      model: "openai/kimi-k2.6"
    },
    "google/gemini-2.5-pro" => %{
      api_base: "https://generativelanguage.googleapis.com/v1beta",
      api_key: "test-google",
      model: "google/gemini-2.5-pro"
    },
    "anthropic/claude-3.7-sonnet" => %{
      api_base: "https://api.anthropic.com/v1/",
      api_key: "test-anthropic",
      model: "anthropic/claude-3.7-sonnet"
    }
  }

  @schema %{"type" => "object", "properties" => %{"title" => %{"type" => "string"}}}

  setup_all do
    {base, stop} = Exhub.Llm.StubProvider.start()
    on_exit(stop)

    %{stub_base: base}
  end

  setup %{stub_base: base} do
    original_llms = Application.get_env(:exhub, :llms)
    original_tool = Application.get_env(:exhub, :llm_tool)

    Application.put_env(:exhub, :llms, Map.merge(@llms, Exhub.Llm.StubProvider.llms(base)))

    on_exit(fn ->
      restore(:llms, original_llms)
      restore(:llm_tool, original_tool)
    end)

    start_supervised!(LlmConfigServer)
    :ok
  end

  defp restore(key, nil), do: Application.delete_env(:exhub, key)
  defp restore(key, value), do: Application.put_env(:exhub, key, value)

  defp text(%{content: content}), do: text(content)
  defp text(content) when is_binary(content), do: content
  defp text(content) when is_list(content), do: Enum.map_join(content, "", &(&1.content || ""))

  defp verify_fun(model) do
    with %{connect_options: connect_options} <- Map.get(model, :req_config),
         transport_opts when is_list(transport_opts) <- connect_options[:transport_opts],
         {fun, state} when is_function(fun, 3) and state == nil <- transport_opts[:verify_fun] do
      fun
    else
      _ -> nil
    end
  end

  # The payload the stub provider received (scenario "echo" reflects it back).
  defp sent_payload(%{content: body}), do: Jason.decode!(body)

  describe "models/0" do
    test "lists the configured models without leaking secrets" do
      result = Client.models()

      assert result.default == "openai/qwen3.8-flash--no-think"
      assert result.count == length(result.models)
      assert result.enabled == true
      assert result.allowed_models == nil

      names = Enum.map(result.models, & &1.name) |> Enum.sort()
      assert Map.keys(@llms) -- names == []

      entry = Enum.find(result.models, &(&1.name == "openai/kimi-k2.6"))
      assert entry.provider == "openai"
      assert entry.model == "kimi-k2.6"

      refute inspect(result) =~ "test-openai"
      refute inspect(result) =~ "api.moark.com"
      refute inspect(result) =~ "stub-key"
    end
  end

  describe "resolve/1" do
    test "resolves the default when the name is nil or empty" do
      assert {:ok, "openai/qwen3.8-flash--no-think", %{model: "openai/qwen3.8-flash"}} =
               Client.resolve(nil)

      assert {:ok, "openai/qwen3.8-flash--no-think", _config} = Client.resolve("")
      assert {:ok, "openai/qwen3.8-flash--no-think", _config} = Client.resolve("   ")
    end

    test "resolves a full configured name" do
      assert {:ok, "openai/qwen3.8-flash", %{api_key: "test-openai"}} =
               Client.resolve("openai/qwen3.8-flash")
    end

    test "resolves a bare model name" do
      assert {:ok, "openai/kimi-k2.6", %{model: "openai/kimi-k2.6"}} =
               Client.resolve("kimi-k2.6")
    end

    test "resolves a variant-suffixed name to the base config" do
      assert {:ok, "openai/qwen3.8-flash--no-think", %{model: "openai/qwen3.8-flash"}} =
               Client.resolve("openai/qwen3.8-flash--no-think")
    end

    test "returns an error for an unknown model" do
      assert {:error, message} = Client.resolve("openai/nope")
      assert message =~ "not found"
    end

    test "enforces allowed_models" do
      Application.put_env(:exhub, :llm_tool, allowed_models: ["kimi-k2.6"])

      assert {:ok, "openai/kimi-k2.6", _config} = Client.resolve("kimi-k2.6")
      assert {:error, message} = Client.resolve("openai/qwen3.8-flash")
      assert message =~ "not in the allowed list"
    end
  end

  describe "guardrails" do
    test "chat refuses to run when the tool surface is disabled" do
      Application.put_env(:exhub, :llm_tool, enabled: false)

      assert {:error, message} = Client.chat(%{model: "stub-ok", prompt: "hi"})
      assert message =~ "LLM calls are disabled"
      assert Client.models().enabled == false
    end

    test "an empty allow-list disables resolution" do
      Application.put_env(:exhub, :llm_tool, allowed_models: [])

      assert {:error, message} = Client.resolve("kimi-k2.6")
      assert message =~ "allowed_models` is empty"
    end

    test "rejects a request that is not a map" do
      assert {:error, "`request` must be a map"} = Client.chat("nope")
      assert {:error, "`request` must be a map"} = Client.chat_json("nope")
      assert {:error, "`request` must be a map"} = Client.build_messages("nope")
    end
  end

  describe "chat/2 against a stub provider" do
    test "returns the reply, the resolved model and the token usage" do
      assert {:ok, result} = Client.chat(%{model: "stub-ok", prompt: "hello"})

      assert result.content == "hi there"
      assert result.model == "openai/stub-ok"
      assert result.usage == %{input: 7, output: 3, total: 10}
    end

    test "reports nil usage when the provider omits it" do
      assert {:ok, result} = Client.chat(%{model: "stub-no-usage", prompt: "hello"})

      assert result.content == "no usage"
      assert result.usage == nil
    end

    test "sends the bare model name, the system message and the per-call overrides" do
      assert {:ok, result} =
               Client.chat(%{
                 model: "openai/stub-echo",
                 prompt: "hello",
                 system: "be brief",
                 temperature: 0.2,
                 max_tokens: 64
               })

      payload = sent_payload(result)

      assert payload["model"] == "stub-echo"
      assert payload["temperature"] == 0.2
      assert payload["max_tokens"] == 64
      assert payload["stream"] == false
      refute Map.has_key?(payload, "tools")
      refute Map.has_key?(payload, "response_format")

      assert [system, user] = payload["messages"]
      assert system["role"] == "system"
      assert system["content"] == [%{"type" => "text", "text" => "be brief"}]
      assert user["role"] == "user"
      assert user["content"] == [%{"type" => "text", "text" => "hello"}]
    end

    test "defaults max_tokens to the configured value" do
      assert {:ok, result} = Client.chat(%{model: "stub-echo", prompt: "hi"})

      assert sent_payload(result)["max_tokens"] == 4096
    end

    test "does not duplicate a system message already in `messages`" do
      assert {:ok, result} =
               Client.chat(%{
                 model: "stub-echo",
                 system: "ignored",
                 messages: [
                   %{"role" => "system", "content" => "from messages"},
                   %{"role" => "user", "content" => "hi"}
                 ]
               })

      roles = sent_payload(result)["messages"] |> Enum.map(& &1["role"])
      assert roles == ["system", "user"]
    end

    test "propagates the provider error message" do
      assert {:error, "Incorrect API key provided"} =
               Client.chat(%{model: "stub-unauthorized", prompt: "hi"})

      assert {:error, "server exploded"} = Client.chat(%{model: "stub-boom", prompt: "hi"})
    end

    test "validates the request before spending a call" do
      assert {:error, message} = Client.chat(%{model: "stub-ok"})
      assert message =~ "`messages` (array) or `prompt`"
    end
  end

  describe "build_messages/2" do
    test "builds a single user message from `prompt`" do
      assert {:ok, [%{role: :user} = user]} = Client.build_messages(%{prompt: "hello"})
      assert text(user) == "hello"
    end

    test "accepts string keys and prepends a system message" do
      assert {:ok, [system, user]} =
               Client.build_messages(%{"system" => "be brief", "prompt" => "hi"})

      assert system.role == :system
      assert text(system) == "be brief"
      assert user.role == :user
      assert text(user) == "hi"
    end

    test "does not duplicate a system message already in `messages`" do
      request = %{
        system: "ignored",
        messages: [
          %{"role" => "system", "content" => "from messages"},
          %{"role" => "user", "content" => "hi"}
        ]
      }

      assert {:ok, [system, _user]} = Client.build_messages(request)
      assert text(system) == "from messages"
    end

    test "accepts the role aliases providers use" do
      request = %{
        messages: [
          %{"role" => "developer", "content" => "d"},
          %{"role" => "human", "content" => "h"},
          %{"role" => "ai", "content" => "a"},
          %{"role" => "model", "content" => "m"},
          %{"role" => "Assistant", "content" => "A"}
        ]
      }

      assert {:ok, messages} = Client.build_messages(request)
      assert Enum.map(messages, & &1.role) == [:system, :user, :assistant, :assistant, :assistant]
    end

    test "converts content parts" do
      request = %{
        messages: [
          %{
            "role" => "user",
            "content" => [
              %{"type" => "text", "text" => "what is this?"},
              %{"type" => "image_url", "url" => "https://example.com/a.png", "detail" => "low"}
            ]
          }
        ]
      }

      assert {:ok,
              [%{content: [%ContentPart{type: :text}, %ContentPart{type: :image_url} = image]}]} =
               Client.build_messages(request)

      assert image.content == "https://example.com/a.png"
      assert image.options[:detail] == "low"
    end

    test "converts a nested image_url object and a base64 image part" do
      request = %{
        messages: [
          %{
            "role" => "user",
            "content" => [
              %{"type" => "image_url", "image_url" => %{"url" => "https://example.com/b.png"}},
              %{"type" => "image", "data" => "QUJD", "media" => "image/png"}
            ]
          }
        ]
      }

      assert {:ok, [%{content: [url_part, image_part]}]} = Client.build_messages(request)

      assert url_part.type == :image_url
      assert url_part.content == "https://example.com/b.png"
      assert url_part.options[:detail] == nil
      assert image_part.type == :image
      assert image_part.content == "QUJD"
      assert image_part.options[:media] == "image/png"
    end

    test "rejects malformed content" do
      assert {:error, message} =
               Client.build_messages(%{messages: [%{"role" => "user", "content" => []}]})

      assert message =~ "must not be empty"

      assert {:error, message} = Client.build_messages(%{messages: [%{"role" => "user"}]})
      assert message =~ "content is required"

      assert {:error, message} =
               Client.build_messages(%{
                 messages: [%{"role" => "user", "content" => [%{"type" => "audio"}]}]
               })

      assert message =~ "unsupported content part type"
    end

    test "decodes a JSON-encoded messages array" do
      encoded = Jason.encode!([%{"role" => "user", "content" => "hi"}])
      assert {:ok, [%{role: :user} = user]} = Client.build_messages(%{messages: encoded})
      assert text(user) == "hi"
    end

    test "appends the schema hint to the last user message" do
      assert {:ok, [%{role: :user} = user]} =
               Client.build_messages(%{prompt: "extract"}, "Respond as JSON.")

      assert text(user) =~ "extract"
      assert text(user) =~ "Respond as JSON."
      assert Jason.encode!(@schema) =~ "object"
    end

    test "appends the schema hint as a new user message when the last turn is the assistant's" do
      request = %{
        messages: [
          %{"role" => "user", "content" => "q"},
          %{"role" => "assistant", "content" => "a"}
        ]
      }

      assert {:ok, [user, assistant, hint]} = Client.build_messages(request, "Respond as JSON.")
      assert {user.role, assistant.role, hint.role} == {:user, :assistant, :user}
      assert text(hint) == "Respond as JSON."
    end

    test "rejects a missing prompt/messages and unknown roles" do
      assert {:error, message} = Client.build_messages(%{})
      assert message =~ "`messages` (array) or `prompt`"

      assert {:error, message} =
               Client.build_messages(%{messages: [%{"role" => "wizard", "content" => "x"}]})

      assert message =~ "unsupported message role"
    end
  end

  describe "build_chat_model/2" do
    test "openai-compatible models carry the TLS verify_fun" do
      assert {:ok, model} =
               Client.build_chat_model(%{
                 model: "openai/kimi-k2.6",
                 api_base: "https://api.moark.com/v1",
                 api_key: "k"
               })

      assert model.__struct__ == LangChain.ChatModels.ChatOpenAI
      assert is_function(verify_fun(model), 3)
    end

    test "google models carry the TLS verify_fun" do
      assert {:ok, model} =
               Client.build_chat_model(%{
                 model: "google/gemini-2.5-pro",
                 api_base: "https://generativelanguage.googleapis.com/v1beta",
                 api_key: "k"
               })

      assert model.__struct__ == LangChain.ChatModels.ChatGoogleAI
      assert is_function(verify_fun(model), 3)
    end

    test "mistral models use the /chat/completions endpoint and no req_config" do
      assert {:ok, model} =
               Client.build_chat_model(%{
                 model: "mistral/mistral-large",
                 api_base: "https://api.mistral.ai/v1",
                 api_key: "k"
               })

      assert model.__struct__ == LangChain.ChatModels.ChatMistralAI
      assert model.endpoint == "https://api.mistral.ai/v1/chat/completions"
      refute Map.has_key?(model, :req_config)
    end

    test "anthropic structs have no req_config field" do
      assert {:ok, model} =
               Client.build_chat_model(%{
                 model: "anthropic/claude-3.7-sonnet",
                 api_base: "https://api.anthropic.com/v1",
                 api_key: "k"
               })

      assert model.__struct__ == LangChain.ChatModels.ChatAnthropic
      refute Map.has_key?(model, :req_config)
    end

    test "reports a config missing its model or api_base" do
      assert {:error, "LLM config has no `model`"} =
               Client.build_chat_model(%{api_base: "https://x/v1", api_key: "k"})

      assert {:error, "LLM config has no `api_base`"} =
               Client.build_chat_model(%{model: "openai/kimi-k2.6", api_key: "k"})

      assert {:error, "`config` must be a map"} = Client.build_chat_model("nope")
    end

    test "applies temperature, max_tokens and clamps the timeout" do
      config = %{model: "openai/kimi-k2.6", api_base: "https://x/v1", api_key: "k"}

      assert {:ok, model} =
               Client.build_chat_model(config, %{
                 temperature: 0.2,
                 max_tokens: 128,
                 timeout_ms: 5_000_000
               })

      assert model.temperature == 0.2
      assert model.max_tokens == 128
      assert model.receive_timeout == 600_000

      assert {:ok, fast} = Client.build_chat_model(config, %{timeout_ms: 1})
      assert fast.receive_timeout == 1_000

      assert {:ok, default} = Client.build_chat_model(config, %{timeout_ms: nil})
      assert default.receive_timeout == 180_000

      assert {:ok, invalid} = Client.build_chat_model(config, %{timeout_ms: "soon"})
      assert invalid.receive_timeout == 180_000
    end

    test "falls back to the default max_tokens for unusable values" do
      config = %{model: "openai/kimi-k2.6", api_base: "https://x/v1", api_key: "k"}

      assert {:ok, zero} = Client.build_chat_model(config, %{max_tokens: 0})
      assert zero.max_tokens == 4096

      assert {:ok, float} = Client.build_chat_model(config, %{max_tokens: 2.9})
      assert float.max_tokens == 2
    end

    test "wraps an OpenAI JSON schema in the provider envelope" do
      assert {:ok, model} =
               Client.build_chat_model(
                 %{model: "openai/kimi-k2.6", api_base: "https://x/v1", api_key: "k"},
                 %{json_schema: @schema}
               )

      assert model.json_response == true
      assert model.json_schema["schema"] == @schema
      assert model.json_schema["strict"] == true
    end

    test "honours strict: false and leaves non-structured providers alone" do
      config = %{model: "openai/kimi-k2.6", api_base: "https://x/v1", api_key: "k"}

      assert {:ok, model} =
               Client.build_chat_model(config, %{json_schema: @schema, strict: false})

      assert model.json_schema["strict"] == false

      assert {:ok, anthropic} =
               Client.build_chat_model(
                 %{model: "anthropic/claude-3.7-sonnet", api_base: "https://x/v1", api_key: "k"},
                 %{json_schema: @schema}
               )

      # Anthropic is not in @structured_providers: it keeps the struct defaults
      # and gets the schema hint appended to the prompt instead.
      assert anthropic.json_response == false
      assert anthropic.json_schema == nil
    end

    test "google takes the bare schema, without the OpenAI envelope" do
      assert {:ok, model} =
               Client.build_chat_model(
                 %{
                   model: "google/gemini-2.5-pro",
                   api_base: "https://generativelanguage.googleapis.com/v1beta",
                   api_key: "k"
                 },
                 %{json_schema: @schema}
               )

      assert model.json_response == true
      assert model.json_schema == @schema
    end
  end

  describe "chat_json/2" do
    test "returns the decoded JSON alongside the raw content" do
      assert {:ok, result} =
               Client.chat_json(%{model: "stub-json", prompt: "extract", schema: @schema})

      assert result.json == %{"title" => "Ada", "tags" => ["x"]}
      assert Jason.decode!(result.content) == result.json
      assert result.model == "openai/stub-json"
    end

    test "returns json: nil when the reply is not valid JSON" do
      assert {:ok, result} =
               Client.chat_json(%{model: "stub-prose", prompt: "extract", schema: @schema})

      assert result.json == nil
      assert result.content == "no json here"
    end

    test "sends a strict json_schema response_format for openai-compatible models" do
      assert {:ok, result} =
               Client.chat_json(%{model: "stub-echo", prompt: "extract", schema: @schema})

      assert %{"type" => "json_schema", "json_schema" => envelope} =
               sent_payload(result)["response_format"]

      assert envelope["name"] == "exhub_response"
      assert envelope["schema"] == @schema
      assert envelope["strict"] == true
    end

    test "accepts a JSON string schema and honours strict: false" do
      assert {:ok, result} =
               Client.chat_json(%{
                 model: "stub-echo",
                 prompt: "extract",
                 schema: Jason.encode!(@schema),
                 strict: false
               })

      assert %{"json_schema" => %{"strict" => false}} = sent_payload(result)["response_format"]
    end

    test "requires a schema" do
      assert {:error, message} = Client.chat_json(%{prompt: "hi"})
      assert message =~ "`schema` is required"
    end

    test "rejects a schema that is not a JSON object" do
      assert {:error, message} = Client.chat_json(%{prompt: "hi", schema: "[]"})
      assert message =~ "must be a JSON object"

      assert {:error, message} = Client.chat_json(%{prompt: "hi", schema: "null"})
      assert message =~ "must be a JSON object"

      assert {:error, message} = Client.chat_json(%{prompt: "hi", schema: "nope"})
      assert message =~ "not valid JSON"

      assert {:error, message} = Client.chat_json(%{prompt: "hi", schema: 42})
      assert message =~ "must be a JSON Schema object"
    end
  end
end
