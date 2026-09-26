defmodule Exhub.BrowserAgent.DomBridge do
  @moduledoc """
  DOM fallback for pages whose accessibility tree cannot be fetched.

  Chrome's CDP `Accessibility.getFullAXTree` fails on wide DOMs (measured between
  ~4,000 and ~6,000 nodes, well below what a documentation page like DevDocs or
  hexdocs renders), which makes those pages unobservable — and therefore
  unreachable — for the Jev loop. This module supplies a pure-DOM equivalent:

    * `snapshot_script/0` stamps every visible interactive element with
      `data-kuri-dom-ref="dN"` and returns a JSON array shaped exactly like a kuri
      accessibility snapshot (`ref`, `role`, `name`, `value`, `state`), so
      `Exhub.BrowserAgent.Snapshot` parses either source unchanged.
    * `action_script/3` replays a `dN` ref back onto its element (`click`,
      `fill`, `type`, `select`) and returns `"ok"` or `"missing"`.

  Only roles a decision can act on are emitted (`Snapshot.clickable_roles/0` plus
  `editable_roles/0`), mirroring the compact table `kuri-agent snap` returns.

  Refs are page-scoped: a navigation invalidates them. The loop re-observes every
  cycle, so a stale ref only costs one step — and it reports `"missing"` rather
  than silently no-op'ing.
  """

  # Deliberately distinguishable from kuri's accessibility refs (`eN`, `e1_24`).
  @ref_prefix "d"
  @ref_attribute "data-kuri-dom-ref"

  # Bound the injected table: the model only needs the controls it can act on,
  # and `Exhub.BrowserAgent.Agent` trims it further (to `opts[:max_elements]`).
  @max_elements 150
  @name_limit 120

  @typedoc "One DOM-derived element, shaped like a parsed accessibility node."
  @type element :: %{optional(String.t()) => String.t() | nil}

  @doc "Returns true for a DOM-derived ref (`d0`, `d7`, …)."
  @spec dom_ref?(term()) :: boolean()
  def dom_ref?(ref) when is_binary(ref), do: Regex.match?(~r/^d\d+$/, ref)
  def dom_ref?(_), do: false

  @doc "The attribute stamped on elements so a `dN` ref can be resolved later."
  @spec ref_attribute() :: String.t()
  def ref_attribute, do: @ref_attribute

  @doc "JavaScript that returns a JSON array of actionable, stamped elements."
  @spec snapshot_script() :: String.t()
  def snapshot_script, do: snapshot_js()

  @doc """
  JavaScript that replays `action` on `ref` and returns `"ok"` or `"missing"`.

  `:fill` and `:type` are equivalent in the DOM (no key events); the value is
  JSON-encoded before it is embedded, and `ref` must be a `dN` ref.
  """
  @spec action_script(String.t(), atom(), String.t() | nil) :: String.t()
  def action_script(ref, action, value \\ nil) do
    unless dom_ref?(ref) do
      raise ArgumentError, "not a DOM ref: #{inspect(ref)}"
    end

    """
    (function () {
      var el = document.querySelector('[#{@ref_attribute}="#{ref}"]');
      if (!el) return 'missing';
      function setValue(v) {
        el.value = v;
        el.dispatchEvent(new Event('input', {bubbles: true}));
        el.dispatchEvent(new Event('change', {bubbles: true}));
      }
      if (el.scrollIntoView) el.scrollIntoView({block: 'center'});
      if (el.focus) el.focus();
      #{action_body(action, value)}
      return 'ok';
    })()
    """
  end

  @doc """
  Extracts the JSON array from an `eval`/`/evaluate` payload.

  Both backends may wrap the result (the daemon nests it as `result.result.value`,
  the CLI prints bare JSON); taking the span between the first `[` and the last
  `]` tolerates either envelope.
  """
  @spec extract_json(String.t() | nil) :: {:ok, String.t()} | {:error, String.t()}
  def extract_json(payload) when is_binary(payload) do
    # Greedy with `/s`, so the match runs from the first `[` to the last `]`.
    case Regex.run(~r/\[.*\]/s, payload) do
      [json] -> {:ok, json}
      _ -> {:error, "DOM snapshot contained no JSON array"}
    end
  end

  def extract_json(_payload), do: {:error, "empty DOM snapshot"}

  @doc """
  Turns an action script's output into a result.

  The script returns `"ok"` on success and `"missing"` when the ref is no longer
  in the DOM (the page navigated or re-rendered since the observation).
  """
  @spec interpret_action(String.t() | nil) :: :ok | {:error, String.t()}
  def interpret_action(output) when is_binary(output) do
    cond do
      String.contains?(output, "missing") -> {:error, "DOM ref is no longer on the page"}
      String.contains?(output, "ok") -> :ok
      true -> {:error, "unexpected DOM action result: #{String.trim(output)}"}
    end
  end

  def interpret_action(_output), do: {:error, "DOM action returned nothing"}

  # --- internals ---

  defp action_body(:click, _value), do: "el.click();"

  defp action_body(action, value) when action in [:fill, :type, :select],
    do: "setValue(#{Jason.encode!(value)});"

  defp action_body(action, _value),
    do: raise(ArgumentError, "unsupported DOM action: #{inspect(action)}")

  # Emitted as a single expression so both `kuri-agent eval` and `/evaluate`
  # accept it. Element order is document order, which is the order the policy sees
  # when it caps target heads.
  defp snapshot_js do
    """
    (function () {
      var ATTR = '#{@ref_attribute}';
      var PREFIX = '#{@ref_prefix}';
      var MAX = #{@max_elements};
      var NAME_LIMIT = #{@name_limit};
      var ACTIONABLE = {
        link: 1, button: 1, menuitem: 1, menuitemcheckbox: 1, menuitemradio: 1,
        tab: 1, option: 1, checkbox: 1, radio: 1, switch: 1,
        textbox: 1, searchbox: 1, combobox: 1, spinbutton: 1
      };
      var EDITABLE = {textbox: 1, searchbox: 1, combobox: 1, spinbutton: 1};

      function roleOf(el, tag, type) {
        var explicit = el.getAttribute('role');
        if (explicit) return explicit.toLowerCase();
        if (el.isContentEditable) return 'textbox';
        if (tag === 'a') return 'link';
        if (tag === 'button' || tag === 'summary') return 'button';
        if (tag === 'select') return 'combobox';
        if (tag === 'textarea') return 'textbox';
        if (tag === 'option') return 'option';
        if (tag === 'input') {
          if (type === 'submit' || type === 'button' || type === 'reset' || type === 'image') {
            return 'button';
          }
          if (type === 'checkbox') return 'checkbox';
          if (type === 'radio') return 'radio';
          if (type === 'range') return 'slider';
          return 'textbox';
        }
        return null;
      }

      function nameOf(el) {
        var raw = el.getAttribute('aria-label') || el.getAttribute('alt') ||
          el.getAttribute('title') || el.getAttribute('placeholder') ||
          el.innerText || el.textContent || '';
        return String(raw).replace(/\\s+/g, ' ').trim().slice(0, NAME_LIMIT);
      }

      function statesOf(el) {
        var state = [];
        if (el.disabled) state.push('disabled');
        if (el.checked) state.push('checked');
        if (el.getAttribute('aria-expanded') === 'true') state.push('expanded');
        if (el.required || el.getAttribute('aria-required') === 'true') state.push('required');
        return state.length ? state.join(' ') : null;
      }

      var nodes = document.querySelectorAll(
        'a[href],button,input,textarea,select,summary,[contenteditable],[role]'
      );
      var out = [];

      for (var i = 0; i < nodes.length && out.length < MAX; i++) {
        var el = nodes[i];
        var tag = (el.tagName || '').toLowerCase();
        var type = (el.getAttribute('type') || '').toLowerCase();
        var role = roleOf(el, tag, type);
        if (!role || !ACTIONABLE[role]) continue;

        var rect = el.getBoundingClientRect();
        if (!rect || rect.width === 0 || rect.height === 0) continue;

        var style = window.getComputedStyle(el);
        if (style && (style.visibility === 'hidden' || style.display === 'none')) continue;

        var value = EDITABLE[role] && typeof el.value === 'string' ? el.value : null;
        var ref = PREFIX + out.length;
        el.setAttribute(ATTR, ref);
        out.push({ref: ref, role: role, name: nameOf(el), value: value, state: statesOf(el)});
      }

      return JSON.stringify(out);
    })()
    """
  end
end
