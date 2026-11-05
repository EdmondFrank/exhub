Code.require_file("../../support/llm_stub_provider.ex", __DIR__)

defmodule Exhub.Lua.LlmBridgeTest do
  @moduledoc """
  Runs real `code_mode` snippets against the built-in `llm` server.

  `Exhub.Lua.Engine` bridges every visible tool into the sandbox, so this is the
  path an agent takes with `llm.chat({...})` — the headline claim in
  `docs/modules/lua.md`. The tools come from `BuiltInRegistry` (exactly what the
  hub advertises to code mode) and dispatch through `BuiltInRegistry.call_tool/3`,
  the in-process path used for built-in servers, so no HTTP hop or booted
  application is involved.
  """
  use ExUnit.Case, async: false

  alias Exhub.Llm.LlmConfigServer
  alias Exhub.Lua.Engine
  alias Exhub.MCP.Hub.BuiltInRegistry

  setup_all do
    {base, stop} = Exhub.Llm.StubProvider.start()
    on_exit(stop)

    %{stub_base: base}
  end

  setup %{stub_base: base} do
    original_llms = Application.get_env(:exhub, :llms)
    original_tool = Application.get_env(:exhub, :llm_tool)

    Application.put_env(:exhub, :llms, Exhub.Llm.StubProvider.llms(base))

    on_exit(fn ->
      restore(:llms, original_llms)
      restore(:llm_tool, original_tool)
    end)

    start_supervised!(LlmConfigServer)
    :ok
  end

  defp restore(key, nil), do: Application.delete_env(:exhub, key)
  defp restore(key, value), do: Application.put_env(:exhub, key, value)

  defp tools, do: BuiltInRegistry.list_tools("llm")

  defp run(code) do
    Engine.run(code, tools(),
      call_fun: &BuiltInRegistry.call_tool/3,
      timeout_ms: 60_000
    )
  end

  # A tool call answers with one MCP content part holding the tool's JSON
  # payload, so a snippet reads it at `content[1].text`.
  defp tool_payload(code) do
    assert {:ok, raw} = run(code)

    assert %{"content" => [%{"type" => "text", "text" => text} | _], "isError" => false} =
             Jason.decode!(raw)

    Jason.decode!(text)
  end

  describe "bridging" do
    test "every llm tool is callable, nested and flat" do
      assert {:ok, raw} =
               run(
                 ~s|return {chat = type(llm.chat), chat_json = type(llm.chat_json), | <>
                   ~s|models = type(llm.models), flat = type(tools["llm__chat"])}|
               )

      assert Jason.decode!(raw) == %{
               "chat" => "function",
               "chat_json" => "function",
               "models" => "function",
               "flat" => "function"
             }
    end

    test "the tool payload sits in the envelope's single text part" do
      assert {:ok, raw} =
               run(~s|local out = llm.chat({model = "stub-ok", prompt = "Say hi"}) return out|)

      assert %{"content" => [%{"type" => "text", "text" => text}], "isError" => false} =
               Jason.decode!(raw)

      assert %{"content" => "hi there"} = Jason.decode!(text)
    end
  end

  describe "llm.chat" do
    test "returns the reply, the resolved model and the usage" do
      payload =
        tool_payload(~s|local out = llm.chat({model = "stub-ok", prompt = "Say hi"}) return out|)

      assert payload["content"] == "hi there"
      assert payload["model"] == "openai/stub-ok"
      assert payload["usage"] == %{"input" => 7, "output" => 3, "total" => 10}
    end

    test "crosses Lua arguments through as the request payload" do
      # The `echo` scenario reflects the request body back as the reply.
      payload =
        tool_payload(
          ~s|local out = llm.chat({model = "stub-echo", prompt = "hello", | <>
            ~s|system = "be brief", temperature = 0.2, max_tokens = 64}) return out|
        )

      sent = payload["content"] |> Jason.decode!()

      assert sent["model"] == "stub-echo"
      assert sent["temperature"] == 0.2
      assert sent["max_tokens"] == 64
      assert [system, user] = sent["messages"]
      assert system["role"] == "system"
      assert user["content"] == [%{"type" => "text", "text" => "hello"}]
    end

    test "works through the flat tools table too" do
      payload =
        tool_payload(
          ~s|local out = tools["llm__chat"]({model = "stub-echo", prompt = "flat"}) return out|
        )

      assert payload["content"] |> Jason.decode!() |> Map.get("model") == "stub-echo"
    end

    test "a provider failure is a catchable ToolFailure" do
      assert {:ok, raw} =
               run(
                 ~s|local ok, err = pcall(llm.chat, {model = "stub-unauthorized", prompt = "x"}) | <>
                   ~s|return {ok = ok, err = err}|
               )

      assert Jason.decode!(raw) == %{
               "ok" => false,
               "err" => "ToolFailure: llm.chat failed: Incorrect API key provided"
             }
    end
  end

  describe "llm.models" do
    test "lists the configured models without leaking secrets" do
      payload = tool_payload(~s|local out = llm.models() return out|)

      assert payload["count"] == 7
      assert payload["enabled"] == true
      assert payload["allowed_models"] == nil

      names = payload["models"] |> Enum.map(& &1["name"])
      assert "openai/stub-ok" in names

      refute payload |> Jason.encode!() =~ "stub-key"
      refute payload |> Jason.encode!() =~ "127.0.0.1"
    end
  end

  describe "llm.chat_json" do
    test "returns the decoded json" do
      payload =
        tool_payload(
          ~s|local out = llm.chat_json({model = "stub-json", prompt = "extract", | <>
            ~s|schema = {type = "object", properties = {title = {type = "string"}}}}) return out|
        )

      assert payload["json"] == %{"title" => "Ada", "tags" => ["x"]}
      assert payload["content"] == Jason.encode!(%{"title" => "Ada", "tags" => ["x"]})
      assert payload["model"] == "openai/stub-json"
    end

    test "a schema written as a Lua table reaches the provider as a JSON Schema" do
      # The `echo` scenario reflects the request body back as the reply, so the
      # decoded `json` here *is* the payload the provider received.
      sent =
        tool_payload(
          ~s|local out = llm.chat_json({model = "stub-echo", prompt = "extract", | <>
            ~s|schema = {type = "object", properties = {title = {type = "string"}}}}) return out|
        )

      assert %{"type" => "json_schema", "json_schema" => envelope} =
               sent["json"]["response_format"]

      assert envelope["schema"] == %{
               "type" => "object",
               "properties" => %{"title" => %{"type" => "string"}}
             }

      assert envelope["strict"] == true

      assert sent["json"]["messages"] |> hd() |> Map.get("content") |> hd() |> Map.get("text") ==
               "extract"
    end
  end
end
