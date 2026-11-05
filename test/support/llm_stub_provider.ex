defmodule Exhub.Llm.StubProviderPlug do
  @moduledoc false
  # Minimal OpenAI-compatible `/chat/completions` stub used by the `llm` tests to
  # exercise the real LangChain → Req → HTTP path.
  #
  # The scenario is the first path segment, so one listener serves every case:
  # a config with `api_base: "<base>/ok"` is called at `<base>/ok/chat/completions`.
  import Plug.Conn

  @usage %{"prompt_tokens" => 7, "completion_tokens" => 3, "total_tokens" => 10}
  @json_reply Jason.encode!(%{"title" => "Ada", "tags" => ["x"]})

  def init(opts), do: opts

  def call(conn, _opts) do
    {:ok, body, conn} = read_body(conn)
    scenario = conn.request_path |> String.split("/", trim: true) |> List.first()

    case scenario do
      "ok" -> completion(conn, 200, "hi there", @usage)
      "no-usage" -> completion(conn, 200, "no usage", nil)
      "json" -> completion(conn, 200, @json_reply, nil)
      "prose" -> completion(conn, 200, "no json here", nil)
      # Echoes the request body back as the assistant message, so a test can
      # assert exactly what the client sent.
      "echo" -> completion(conn, 200, body, nil)
      "unauthorized" -> error(conn, 401, "Incorrect API key provided")
      "boom" -> error(conn, 500, "server exploded")
    end
  end

  defp completion(conn, status, content, usage) do
    body = %{
      "id" => "chatcmpl-stub",
      "object" => "chat.completion",
      "created" => 0,
      "model" => "stub",
      "choices" => [
        %{
          "index" => 0,
          "finish_reason" => "stop",
          "message" => %{"role" => "assistant", "content" => content}
        }
      ]
    }

    body = if usage, do: Map.put(body, "usage", usage), else: body

    json(conn, status, body)
  end

  defp error(conn, status, message) do
    json(conn, status, %{"error" => %{"message" => message, "type" => "stub_error"}})
  end

  defp json(conn, status, body) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(status, Jason.encode!(body))
  end
end

defmodule Exhub.Llm.StubProvider do
  @moduledoc false
  # Starts the stub provider on a random port. Returns the base URL and a
  # shutdown closure; pass the closure to `on_exit/1` so the listener is per test
  # module and never shared.
  #
  # `:req` (LangChain's HTTP client) must be running for a chat call to work, and
  # it is not started under `mix test --no-start`.

  @doc "Starts the stub `:ok` provider. Returns `{base_url, shutdown_fun}`."
  @spec start() :: {String.t(), (-> :ok)}
  def start do
    {:ok, _} = Application.ensure_all_started([:req, :plug_cowboy])

    ref = :"exhub_llm_stub_#{System.unique_integer([:positive])}"
    {:ok, _pid} = Plug.Cowboy.http(Exhub.Llm.StubProviderPlug, [], port: 0, ref: ref)

    {"http://127.0.0.1:#{:ranch.get_port(ref)}", fn -> Plug.Cowboy.shutdown(ref) end}
  end

  @doc "The scenarios the stub understands, as configured model names."
  @spec scenarios() :: [String.t()]
  def scenarios, do: ~w(ok no-usage json prose echo unauthorized boom)

  @doc """
  One LLM config per stub scenario: `openai/stub-ok` → `<base>/ok`.
  """
  @spec llms(String.t()) :: %{optional(String.t()) => map()}
  def llms(base) do
    for scenario <- scenarios(), into: %{} do
      name = "openai/stub-" <> scenario
      {name, %{api_base: base <> "/" <> scenario, api_key: "stub-key", model: name}}
    end
  end
end
