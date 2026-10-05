defmodule Exhub.LspBridge.Handlers.ExecuteCommand do
  @moduledoc """
  `workspace/executeCommand` — port of `core/handler/execute_command.py`.

  Sends a command (from a code action) to the server. The response is usually
  `null`; some servers answer with a `WorkspaceEdit`, which the front end
  applies like a rename. This handler is **not capability-gated** — servers
  frequently carry commands without advertising `executeCommandProvider`.
  """

  @behaviour Exhub.LspBridge.Handler

  @impl true
  def name, do: "execute-command"

  @impl true
  def method, do: "workspace/executeCommand"

  @impl true
  def cancel_on_change?, do: false

  @impl true
  def provider, do: nil

  @impl true
  def request_params(%{"command" => command} = args, _ctx) do
    %{"command" => command, "arguments" => Map.get(args, "arguments", [])}
  end

  def request_params(_args, _ctx), do: %{}

  @impl true
  def process_response(nil, _ctx), do: {:message, "Command executed."}

  def process_response(%{} = result, _ctx) do
    if Map.has_key?(result, "changes") or Map.has_key?(result, "documentChanges") do
      {:workspace_edit, result, "Command executed."}
    else
      {:message, "Command executed."}
    end
  end

  def process_response(_other, _ctx), do: {:message, "Command executed."}
end
