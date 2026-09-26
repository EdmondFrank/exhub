defmodule Exhub.BrowserAgent.KuriHttp do
  @moduledoc """
  The kuri daemon backend: HTTP calls to the `kuri` server ExHub already runs.

  `Exhub.KuriDaemon` supervises `kuri` (default `127.0.0.1:18080`), which drives
  Chrome over CDP and exposes it as an HTTP API. Driving that server instead of
  the `kuri-agent` CLI matters for two reasons:

    * **Refs survive between calls.** The daemon keeps its accessibility ref
      registry server-side, so `snap` → `click` works, while `kuri-agent`
      assigns refs per process and can never resolve a ref printed by an earlier
      invocation.
    * **One process, many operations.** No `Exile` spawn per browser command.

  Tab selection is explicit: `opts[:tab_id]`, else `:exhub, __MODULE__[:tab_id]`,
  else the `cdp_url` in `~/.kuri/session.json` (the tab `browser_tabs` attached).
  Requests are authorized with `Exhub.KuriDaemon.api_token/0` (or `opts[:token]`).

  The daemon returns the *whole* accessibility tree, so `snap/1` filters it to the
  actionable roles the policy can offer, matching the compact table `kuri-agent
  snap` produces (and dropping `_i` inline-text refs, which the daemon will not
  act on). When CDP's accessibility snapshot fails — it does on wide DOMs — the
  caller falls back to `dom_snapshot/1`.
  """

  alias Exhub.BrowserAgent.{DomBridge, KuriCli, Scroll, Snapshot}

  @default_host "127.0.0.1"
  @default_port 18080
  @timeout 60_000

  # The daemon's inline-text refs (`e1_i280`) look editable but are not
  # registered for actions; acting on one fails with "Ref not found".
  @inline_ref ~r/_i\d+$/

  @typedoc "Result shared with the other backends."
  @type result :: {:ok, String.t()} | {:error, String.t()}

  @doc """
  Takes an accessibility snapshot and returns the actionable element table.

  Returns `{:ok, elements}` where `elements` is a list of maps shaped like parsed
  snapshot nodes, ready for `Exhub.BrowserAgent.Snapshot.parse/1`.
  """
  @spec snap(keyword()) :: {:ok, [map()]} | {:error, String.t()}
  def snap(opts \\ []) do
    with {:ok, tab} <- tab_id(opts),
         {:ok, payload} <- get("/snapshot", [tab_id: tab], opts) do
      elements = payload |> List.wrap() |> Enum.filter(&actionable?/1)

      # Keep a non-empty observation for pages with no actionable controls, so
      # the loop does not read an empty table as "not observed yet".
      {:ok, if(elements == [], do: List.wrap(payload), else: elements)}
    end
  end

  @doc "Reads the page as a stamped DOM table, for pages with an unusable a11y tree."
  @spec dom_snapshot(keyword()) :: result()
  def dom_snapshot(opts \\ []) do
    with {:ok, output} <- evaluate(DomBridge.snapshot_script(), opts) do
      DomBridge.extract_json(output)
    end
  end

  @doc "Returns the visible page text."
  @spec text(keyword()) :: result()
  def text(opts \\ []) do
    with {:ok, tab} <- tab_id(opts),
         {:ok, payload} <- get("/text", [tab_id: tab], opts) do
      script_value(payload)
    end
  end

  @doc "Clicks the element with the given ref (`eN`, `e1_24`, or a `dN` DOM ref)."
  @spec click(String.t(), keyword()) :: result()
  def click(ref, opts \\ []) do
    if DomBridge.dom_ref?(ref),
      do: dom_action(ref, :click, nil, opts),
      else: action(ref, "click", nil, opts)
  end

  @doc "Clears and fills an editable element. Prefer over `type/2` to replace."
  @spec fill(String.t(), String.t(), keyword()) :: result()
  def fill(ref, value, opts \\ []) do
    if DomBridge.dom_ref?(ref),
      do: dom_action(ref, :fill, value, opts),
      else: action(ref, "fill", value, opts)
  end

  @doc "Types text into an element without clearing it first."
  @spec type(String.t(), String.t(), keyword()) :: result()
  def type(ref, value, opts \\ []) do
    if DomBridge.dom_ref?(ref),
      do: dom_action(ref, :type, value, opts),
      else: action(ref, "type", value, opts)
  end

  @doc "Selects a value from a dropdown."
  @spec select(String.t(), String.t(), keyword()) :: result()
  def select(ref, value, opts \\ []) do
    if DomBridge.dom_ref?(ref),
      do: dom_action(ref, :select, value, opts),
      else: action(ref, "select", value, opts)
  end

  @doc "Scrolls the page one viewport in `direction` (`:up` or `:down`)."
  @spec scroll(:up | :down, keyword()) :: result()
  def scroll(direction \\ :down, opts \\ []) do
    with {:ok, _offset} <- evaluate(Scroll.script(direction), opts) do
      {:ok, "scrolled #{direction}"}
    end
  end

  @doc "Evaluates a JavaScript expression in the page and returns its value."
  @spec eval(String.t(), keyword()) :: result()
  def eval(expression, opts \\ []), do: evaluate(expression, opts)

  @doc "Navigates the attached tab to `url`."
  @spec go(String.t(), keyword()) :: result()
  def go(url, opts \\ []) do
    with {:ok, tab} <- tab_id(opts),
         {:ok, payload} <- get("/navigate", [tab_id: tab, url: url], opts) do
      {:ok, Jason.encode!(payload)}
    end
  end

  @doc "Lists open Chrome tabs."
  @spec tabs(keyword()) :: result()
  def tabs(opts \\ []) do
    with {:ok, payload} <- get("/tabs", [], opts), do: {:ok, Jason.encode!(payload)}
  end

  @doc """
  Attaches to a tab by writing `~/.kuri/session.json` the way `kuri-agent use` does.

  The daemon has no equivalent call, and both backends read that file for the
  attached tab, so this delegates to `Exhub.BrowserAgent.KuriCli`.
  """
  @spec use(String.t()) :: result()
  def use(ws_url), do: KuriCli.use(ws_url)

  @doc "Shows the attached tab (from the shared session file)."
  @spec status(keyword()) :: result()
  def status(opts \\ []) do
    with {:ok, tab} <- tab_id(opts) do
      {:ok, Jason.encode!(%{"tab_id" => tab})}
    end
  end

  @doc "Resolves the Chrome tab id this backend talks to."
  @spec tab_id(keyword()) :: {:ok, String.t()} | {:error, String.t()}
  def tab_id(opts \\ []) do
    from_opts = Keyword.get(opts, :tab_id)
    from_config = Application.get_env(:exhub, __MODULE__, [])[:tab_id]

    cond do
      is_binary(from_opts) and from_opts != "" -> {:ok, from_opts}
      is_binary(from_config) and from_config != "" -> {:ok, from_config}
      true -> session_tab_id()
    end
  end

  # --- internals ---

  defp dom_action(ref, action, value, opts) do
    with {:ok, output} <- evaluate(DomBridge.action_script(ref, action, value), opts) do
      case DomBridge.interpret_action(output) do
        :ok -> {:ok, "dom #{action} #{ref}"}
        {:error, message} -> {:error, message}
      end
    end
  end

  defp action(ref, verb, value, opts) do
    with {:ok, tab} <- tab_id(opts),
         params = [tab_id: tab, ref: ref, action: verb] ++ value_params(value),
         {:ok, payload} <- get("/action", params, opts) do
      {:ok, Jason.encode!(payload)}
    end
  end

  defp value_params(nil), do: []
  defp value_params(value), do: [value: to_string(value)]

  defp evaluate(expression, opts) do
    with {:ok, tab} <- tab_id(opts),
         {:ok, payload} <- get("/evaluate", [tab_id: tab, expression: expression], opts) do
      script_value(payload)
    end
  end

  # The daemon nests script results as `result.result.value`. The value is not
  # always a string: `window.scrollBy(...)` returns `undefined` (the daemon sends
  # `{"type":"object","value":{}}`) and a script may return a number or boolean.
  # Strings pass through, any other JSON value is encoded so callers still get a
  # string, and a result with no `value` key (CDP `undefined`) is empty rather
  # than an error.
  defp script_value(payload) when is_binary(payload), do: {:ok, payload}

  defp script_value(payload) when is_map(payload) do
    case script_result(payload) do
      %{"value" => value} when is_binary(value) -> {:ok, value}
      %{"value" => value} -> {:ok, Jason.encode!(value)}
      %{} -> {:ok, ""}
      nil -> {:error, "kuri HTTP script result carried no value"}
    end
  end

  defp script_value(_payload), do: {:error, "kuri HTTP script result carried no value"}

  defp script_result(payload) do
    nested = get_in(payload, ["result", "result"])
    if is_map(nested), do: nested, else: get_in(payload, ["result"])
  end

  defp actionable?(element) when is_map(element) do
    ref = element["ref"] || element[:ref]
    role = element["role"] || element[:role]

    is_binary(ref) and is_binary(role) and
      (role in Snapshot.clickable_roles() or role in Snapshot.editable_roles()) and
      not Regex.match?(@inline_ref, ref)
  end

  defp actionable?(_element), do: false

  defp get(path, params, opts) do
    url = base_url(opts) <> path
    headers = [{"authorization", "Bearer " <> token(opts)}]
    http = http_client(opts)

    case http.get(url, headers, params: params, recv_timeout: @timeout) do
      {:ok, code, body} when code in 200..299 -> decode(body)
      {:ok, code, body} -> {:error, "kuri HTTP #{code} for #{path}: #{trim(body)}"}
      {:error, reason} -> {:error, "kuri HTTP request to #{path} failed: #{inspect(reason)}"}
    end
  end

  defp decode(body) when is_binary(body) do
    case Jason.decode(body) do
      {:ok, %{"error" => error}} -> {:error, "kuri: #{to_string(error)}"}
      {:ok, decoded} -> {:ok, decoded}
      {:error, _} -> {:error, "kuri HTTP returned invalid JSON: #{trim(body)}"}
    end
  end

  defp decode(body), do: {:ok, body}

  defp base_url(opts) do
    case Keyword.get(opts, :base_url) do
      url when is_binary(url) and url != "" -> url
      _ -> "http://#{host()}:#{port()}"
    end
  end

  defp host, do: Application.get_env(:exhub, :kuri_host, @default_host)
  defp port, do: Application.get_env(:exhub, :kuri_port, @default_port)

  defp token(opts) do
    Keyword.get(opts, :token) || Exhub.KuriDaemon.api_token()
  end

  defp http_client(opts) do
    Keyword.get(opts, :http) ||
      Application.get_env(:exhub, __MODULE__, [])[:http_client] ||
      Exhub.BrowserAgent.KuriHttp.Client
  end

  defp session_tab_id do
    with path when is_binary(path) <- session_path(),
         {:ok, body} <- File.read(path),
         {:ok, %{"cdp_url" => url}} <- Jason.decode(body),
         tab when is_binary(tab) and tab != "" <- Path.basename(to_string(url)) do
      {:ok, tab}
    else
      _ ->
        {:error, "no attached Chrome tab: run browser_tabs with `use`, or pass tab_id"}
    end
  end

  defp session_path do
    case System.user_home() do
      nil -> nil
      home -> Path.join([home, ".kuri", "session.json"])
    end
  end

  defp trim(body) when is_binary(body), do: String.slice(body, 0, 200)
  defp trim(body), do: inspect(body)
end

defmodule Exhub.BrowserAgent.KuriHttp.Client do
  @moduledoc false
  # Default HTTP transport for `Exhub.BrowserAgent.KuriHttp`. Kept deliberately
  # thin (and replaceable via `:http` / `:http_client` config) so the backend can
  # be unit-tested without a network.

  @spec get(String.t(), [{String.t(), String.t()}], keyword()) ::
          {:ok, non_neg_integer(), String.t()} | {:error, term()}
  def get(url, headers, opts) do
    case HTTPoison.get(url, headers, Keyword.take(opts, [:params, :recv_timeout, :timeout])) do
      {:ok, %HTTPoison.Response{status_code: status, body: body}} -> {:ok, status, body}
      {:error, %HTTPoison.Error{reason: reason}} -> {:error, reason}
    end
  end
end
