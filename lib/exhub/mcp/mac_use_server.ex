defmodule Exhub.MCP.MacUseServer do
  @moduledoc """
  MCP Server for macOS native app automation via axcli.

  ## Purpose
  Provides Playwright/Puppeteer-style control over any macOS application
  using the Accessibility API. Explore UI trees, interact with elements,
  take screenshots, and automate native apps — all from MCP.

  Built on top of [axcli](https://github.com/andelf/axcli), a Rust CLI
  that wraps Apple's Accessibility and ScreenCaptureKit frameworks.

  ## Requirements
  - `axcli` installed: `cargo install axcli`
  - Terminal app granted Accessibility access (System Settings → Privacy & Security → Accessibility)
  - Terminal app granted Screen Recording access (for screenshots)

  ## Tool Categories

  ### App Discovery
  - `list_apps`      — List running macOS apps visible to accessibility

  ### Inspection
  - `snapshot`       — Print accessibility tree of an app or element
  - `get_attribute`  — Get an element attribute value

  ### Interaction (background-safe by default)
  - `click`          — Click an element
  - `dblclick`       — Double-click an element
  - `input`          — Focus and type text (appends)
  - `fill`           — Clear field then type text
  - `press`          — Press key combo (Enter, Command+a, etc.)
  - `hover`          — Move mouse to element center
  - `focus`          — Focus an element

  ### Scrolling
  - `scroll_to`      — Scroll element into view
  - `scroll`         — Scroll within element (up/down/left/right)

  ### Screenshots
  - `screenshot`     — Capture screenshot (occlusion-proof, optional OCR)

  ### Utility
  - `wait`           — Wait for element to appear or sleep N ms
  - `activate`       — Bring app to foreground

  ### Global Input (temporarily disabled — axcli lacks these subcommands)
  # - `mouse`          — Global mouse control (pos, move, click, scroll)
  # - `keyboard`       — Global keyboard input (type, press)

  The server is accessible at `/mac-use/mcp`.
  """

  use Anubis.Server,
    name: "exhub-mac-use-server",
    version: "1.0.0",
    capabilities: [:tools]

  # App discovery
  component(Exhub.MCP.Tools.MacUse.ListApps)

  # Inspection
  component(Exhub.MCP.Tools.MacUse.Snapshot)
  component(Exhub.MCP.Tools.MacUse.GetAttribute)

  # Interaction
  component(Exhub.MCP.Tools.MacUse.Click)
  component(Exhub.MCP.Tools.MacUse.DblClick)
  component(Exhub.MCP.Tools.MacUse.Input)
  component(Exhub.MCP.Tools.MacUse.Fill)
  component(Exhub.MCP.Tools.MacUse.Press)
  component(Exhub.MCP.Tools.MacUse.Hover)
  component(Exhub.MCP.Tools.MacUse.Focus)

  # Scrolling
  component(Exhub.MCP.Tools.MacUse.ScrollTo)
  component(Exhub.MCP.Tools.MacUse.Scroll)

  # Screenshots
  component(Exhub.MCP.Tools.MacUse.Screenshot)

  # Utility
  component(Exhub.MCP.Tools.MacUse.Wait)
  component(Exhub.MCP.Tools.MacUse.Activate)

  # Global input (temporarily disabled — axcli lacks these subcommands)
  # component Exhub.MCP.Tools.MacUse.Mouse
  # component Exhub.MCP.Tools.MacUse.Keyboard

  @impl true
  def init(client_info, frame) do
    _ = client_info
    {:ok, frame}
  end

  @impl true
  def handle_request(request, frame) do
    Exhub.MCP.ServerHelpers.handle_request_with_filtered_tools(__MODULE__, request, frame)
  end
end
