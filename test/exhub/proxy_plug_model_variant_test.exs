defmodule Exhub.ProxyPlugModelVariantTest do
  use ExUnit.Case, async: false

  alias Exhub.ProxyPlug
  alias Exhub.Router.Settings

  setup do
    original = System.get_env("EXHUB_ROUTER_CONFIG")

    path =
      Path.join(
        System.tmp_dir!(),
        "exhub_variant_cfg_#{System.unique_integer([:positive])}.json"
      )

    File.write!(
      path,
      ~s({"reasoning_effort":[{"models":["qwen3.6-flash"],"effort":"high"}]})
    )

    System.put_env("EXHUB_ROUTER_CONFIG", path)
    Settings.reload()

    on_exit(fn ->
      Settings.reload()
      File.rm(path)

      case original do
        nil -> System.delete_env("EXHUB_ROUTER_CONFIG")
        value -> System.put_env("EXHUB_ROUTER_CONFIG", value)
      end
    end)

    :ok
  end

  defp encode(path, model) do
    conn = %Plug.Conn{
      request_path: path,
      body_params: %{"model" => model, "messages" => [%{"role" => "user", "content" => "hi"}]}
    }

    ProxyPlug.encode_body_with_model_transforms(conn.body_params, conn)
    |> Jason.decode!()
  end

  test "no-think adds chat_template_kwargs and strips the suffix from the model" do
    body = encode("/openai/v1/chat/completions", "qwen3.6-flash--no-think")

    assert body["model"] == "qwen3.6-flash"

    assert body["chat_template_kwargs"] == %{
             "enable_thinking" => false,
             "preserve_thinking" => false
           }
  end

  test "an effort suffix sets reasoning_effort and overrides the router.json rule" do
    body = encode("/openai/v1/chat/completions", "qwen3.6-flash--no-think+effort-medium")

    assert body["model"] == "qwen3.6-flash"
    assert body["reasoning_effort"] == "medium"
  end

  test "the router.json reasoning_effort rule still applies without a suffix" do
    body = encode("/openai/v1/chat/completions", "qwen3.6-flash")

    assert body["reasoning_effort"] == "high"
  end

  test "variant controls are ignored outside the OpenAI routes (suffix still stripped)" do
    body = encode("/anthropic/v1/messages", "qwen3.6-flash--no-think")

    assert body["model"] == "qwen3.6-flash"
    refute Map.has_key?(body, "chat_template_kwargs")
  end

  test "non-Qwen names are forwarded verbatim" do
    body = encode("/openai/v1/chat/completions", "deepseek-v3--no-think")

    assert body["model"] == "deepseek-v3--no-think"
    refute Map.has_key?(body, "chat_template_kwargs")
  end
end
