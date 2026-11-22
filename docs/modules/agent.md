# exhub-agent

Emacs client for the ExHub **Agent Hub** (Sagents). Each registered agent
profile — `assistant`, `coder`, `researcher`, `genclaw`, `toonflow` — owns one
conversation persisted in SQLite, so a thread survives Emacs restarts and is
the same one shown in the browser UI at `http://localhost:9069/agent-hub`.

The server side is `Exhub.ResponseHandlers.ExhubAgent`, a thin bridge to
`Exhub.Sagents.Hub` over the WebSocket (`exhub-call`). Replies render through
the `exhub-chat` buffer plumbing, exactly like `exhub-chat`.

## Setup

Add the following to your Emacs configuration file (e.g., `~/.emacs.d/init.el`):
```elisp
(require 'exhub-agent)
```

## Usage

- `exhub-agent-chat`: pick a profile and chat with it (continues that agent's
  latest conversation).
- `exhub-agent-tool-reply`: pick a profile and send it the output of a shell
  command as the prompt.
- `exhub-agent-start`: start a profile's agent without sending a message.
- `exhub-agent-kill`: stop every running conversation of a profile.

The agent list comes from `Exhub.Sagents.Factory.agents/0`; add a profile there
(see [agent-hub.md](agent-hub.md#adding-a-custom-agent)) to make it selectable
in Emacs.

## Keybindings

When in `exhub-agent-mode`, the following keybindings are available:

| Key     | Command                  |
|---------|--------------------------|
| `C-c c` | `exhub-agent-chat`       |
| `C-c r` | `exhub-agent-tool-reply` |
| `C-c s` | `exhub-agent-start`      |
| `C-c k` | `exhub-agent-kill`       |

## History

This module used to drive a `swarm_ex` "world" of ad-hoc agents with
`instructor_ex` for routing. Both dependencies were unmaintained (and produced
Logger deprecation warnings on Elixir 1.19), so the World was removed in favour
of the Agent Hub; agent profiles, conversation persistence, tool groups, and
human-in-the-loop approvals all come from Sagents now.
