defmodule Exhub.MCP.Agent.Status do
  @moduledoc """
  Mirrors ACP agent lifecycle into the shared participant registry.

  Without this, `agent_await` cannot be used as a fan-in wait: the registry only
  learns a status when somebody calls `agent_set_status`, so a subagent that is
  busy running a prompt still looks `idle` and `agent_await` returns
  immediately. `prompt_start` / `agent_prompt` / `agent_task` therefore drive the
  status here: `working` while a prompt runs, `idle` when it completes, `error`
  when it fails.
  """

  alias Exhub.Agent.Registry

  @bus_statuses ~w(starting idle working blocked waiting_input done error offline)

  @doc "Best-effort status mirror; never raises and never blocks the caller."
  @spec mirror(String.t(), String.t(), String.t() | nil) :: :ok
  def mirror(agent_id, status, detail \\ nil) do
    attrs =
      if status in @bus_statuses do
        # A new status without a detail must not inherit the previous detail.
        %{id: agent_id, status: status, detail: detail || ""}
      else
        %{id: agent_id, detail: status}
      end

    Registry.register(attrs)
    :ok
  rescue
    _ -> :ok
  catch
    :exit, _ -> :ok
  end
end
