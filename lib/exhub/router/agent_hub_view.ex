defmodule Exhub.Router.AgentHubView do
  @moduledoc """
  HTML view for the Agent Hub overview page.

  Displays all registered agents with their running status
  and provides start/stop controls.
  """

  @doc """
  Returns the complete HTML content for the Agent Hub page.
  """
  @spec render() :: String.t()
  def render do
    """
    <!DOCTYPE html>
    <html lang="en">
    <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>Agent Hub — Exhub</title>
      <style>
        * { box-sizing: border-box; margin: 0; padding: 0; }
        body {
          font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif;
          background: #0d1117;
          color: #c9d1d9;
          line-height: 1.6;
        }
        .container {
          max-width: 1200px;
          margin: 0 auto;
          padding: 16px;
        }
        header {
          background: #161b22;
          border-bottom: 1px solid #30363d;
          padding: 16px 0;
          margin-bottom: 20px;
        }
        header .container {
          display: flex;
          align-items: center;
          justify-content: space-between;
          flex-wrap: wrap;
          gap: 8px;
        }
        h1 {
          color: #58a6ff;
          font-size: 22px;
        }
        .subtitle {
          color: #8b949e;
          font-size: 13px;
        }
        .header-right {
          display: flex;
          align-items: center;
          gap: 8px;
        }
        .nav-link {
          color: #58a6ff;
          text-decoration: none;
          font-size: 13px;
          padding: 6px 12px;
          border: 1px solid #30363d;
          border-radius: 6px;
          transition: background 0.15s;
        }
        .nav-link:hover {
          background: #21262d;
        }
        .refresh-btn {
          background: #238636;
          color: white;
          border: none;
          padding: 6px 14px;
          border-radius: 6px;
          cursor: pointer;
          font-size: 13px;
        }
        .refresh-btn:hover {
          background: #2ea043;
        }
        .agents-grid {
          display: grid;
          grid-template-columns: repeat(auto-fill, minmax(320px, 1fr));
          gap: 16px;
          margin-bottom: 20px;
        }
        .agent-card {
          background: #161b22;
          border: 1px solid #30363d;
          border-radius: 10px;
          padding: 20px;
          transition: transform 0.2s, box-shadow 0.2s;
        }
        .agent-card:hover {
          transform: translateY(-2px);
          box-shadow: 0 8px 24px rgba(0,0,0,0.3);
        }
        .agent-header {
          display: flex;
          align-items: center;
          justify-content: space-between;
          margin-bottom: 12px;
        }
        .agent-name {
          font-size: 18px;
          font-weight: 600;
          color: #f0f6fc;
        }
        .status-badge {
          display: inline-flex;
          align-items: center;
          gap: 6px;
          padding: 4px 10px;
          border-radius: 12px;
          font-size: 12px;
          font-weight: 500;
        }
        .status-running {
          background: #238636;
          color: white;
        }
        .status-stopped {
          background: #30363d;
          color: #8b949e;
        }
        .status-dot {
          width: 8px;
          height: 8px;
          border-radius: 50%;
          display: inline-block;
        }
        .status-dot.running {
          background: #3fb950;
          box-shadow: 0 0 6px #3fb950;
        }
        .status-dot.stopped {
          background: #8b949e;
        }
        .agent-description {
          color: #8b949e;
          font-size: 13px;
          margin-bottom: 16px;
          min-height: 40px;
        }
        .agent-actions {
          display: flex;
          gap: 8px;
        }
        .btn {
          padding: 8px 16px;
          border-radius: 6px;
          font-size: 13px;
          font-weight: 500;
          cursor: pointer;
          border: none;
          transition: background 0.15s, opacity 0.15s;
        }
        .btn:disabled {
          opacity: 0.5;
          cursor: not-allowed;
        }
        .btn-start {
          background: #238636;
          color: white;
        }
        .btn-start:hover:not(:disabled) {
          background: #2ea043;
        }
        .btn-stop {
          background: #da3633;
          color: white;
        }
        .btn-stop:hover:not(:disabled) {
          background: #f85149;
        }
        .btn-chat {
          background: #1f6feb;
          color: white;
        }
        .btn-chat:hover:not(:disabled) {
          background: #388bfd;
        }
        .btn-reset {
          background: #30363d;
          color: #c9d1d9;
          border: 1px solid #484f58;
        }
        .btn-reset:hover:not(:disabled) {
          background: #484f58;
        }
        .loading {
          text-align: center;
          padding: 40px;
          color: #8b949e;
        }
        .error {
          background: #da3633;
          color: white;
          padding: 12px;
          border-radius: 8px;
          margin-bottom: 16px;
          font-size: 14px;
        }
        .toast {
          position: fixed;
          bottom: 20px;
          right: 20px;
          background: #161b22;
          border: 1px solid #30363d;
          border-radius: 8px;
          padding: 12px 20px;
          font-size: 13px;
          color: #c9d1d9;
          box-shadow: 0 4px 12px rgba(0,0,0,0.4);
          transform: translateY(100px);
          opacity: 0;
          transition: transform 0.3s, opacity 0.3s;
          z-index: 1000;
        }
        .toast.show {
          transform: translateY(0);
          opacity: 1;
        }
        .toast.success {
          border-color: #238636;
        }
        .toast.error {
          border-color: #da3633;
        }
        @media (max-width: 768px) {
          h1 { font-size: 18px; }
          .agents-grid {
            grid-template-columns: 1fr;
          }
        }
      </style>
    </head>
    <body>
      <header>
        <div class="container">
          <div>
            <h1>Agent Hub</h1>
            <p class="subtitle">Manage and interact with your AI agents</p>
          </div>
          <div class="header-right">
            <a href="/dashboard" class="nav-link">Token Dashboard</a>
            <button class="refresh-btn" onclick="loadAgents()">↻ Refresh</button>
          </div>
        </div>
      </header>

      <div class="container">
        <div id="error"></div>
        <div class="agents-grid" id="agents-grid">
          <div class="loading">Loading agents...</div>
        </div>
      </div>

      <div class="toast" id="toast"></div>

      <script>
        function showToast(message, type) {
          var toast = document.getElementById('toast');
          toast.textContent = message;
          toast.className = 'toast show ' + (type || '');
          setTimeout(function() {
            toast.className = 'toast';
          }, 3000);
        }

        async function loadAgents() {
          try {
            document.getElementById('error').innerHTML = '';
            var response = await fetch('/agent-hub/agents');
            var data = await response.json();
            var agents = data.agents || [];

            if (agents.length === 0) {
              document.getElementById('agents-grid').innerHTML =
                '<div class="loading">No agents registered</div>';
              return;
            }

            var html = agents.map(function(agent) {
              var statusClass = agent.running ? 'running' : 'stopped';
              var statusText = agent.running ? 'Running' : 'Stopped';

              return '<div class="agent-card">' +
                '<div class="agent-header">' +
                  '<span class="agent-name">' + escapeHtml(agent.name) + '</span>' +
                  '<span class="status-badge status-' + statusClass + '">' +
                    '<span class="status-dot ' + statusClass + '"></span>' +
                    statusText +
                  '</span>' +
                '</div>' +
                '<div class="agent-actions">' +
                  (agent.running
                    ? '<button class="btn btn-stop" onclick="stopAgent(\\'' + escapeHtml(agent.name) + '\\')">Stop</button>' +
                      '<button class="btn btn-chat" onclick="openChat(\\'' + escapeHtml(agent.name) + '\\')">Chat</button>'
                    : '<button class="btn btn-start" onclick="startAgent(\\'' + escapeHtml(agent.name) + '\\')">Start</button>'
                  ) +
                  '<button class="btn btn-reset" onclick="resetAgent(\\'' + escapeHtml(agent.name) + '\\')">Reset</button>' +
                '</div>' +
              '</div>';
            }).join('');

            document.getElementById('agents-grid').innerHTML = html;
          } catch (err) {
            document.getElementById('error').innerHTML =
              '<div class="error">Error: ' + err.message + '</div>';
            console.error('Agent Hub error:', err);
          }
        }

        async function startAgent(name) {
          try {
            var response = await fetch('/agent-hub/agents/' + encodeURIComponent(name) + '/start', {
              method: 'POST'
            });
            var result = await response.json();

            if (response.ok) {
              showToast('Agent started: ' + name, 'success');
            } else {
              showToast('Failed to start agent: ' + (result.error || 'Unknown error'), 'error');
            }

            setTimeout(loadAgents, 500);
          } catch (err) {
            showToast('Failed to start agent: ' + err.message, 'error');
          }
        }

        async function stopAgent(name) {
          try {
            await fetch('/agent-hub/agents/' + encodeURIComponent(name) + '/stop', { method: 'POST' });
            showToast('Agent stopped: ' + name, 'success');
            loadAgents();
          } catch (err) {
            showToast('Failed to stop agent: ' + err.message, 'error');
          }
        }

        async function resetAgent(name) {
          try {
            await fetch('/agent-hub/agents/' + encodeURIComponent(name) + '/reset', { method: 'POST' });
            showToast('Agent reset: ' + name, 'success');
          } catch (err) {
            showToast('Failed to reset agent: ' + err.message, 'error');
          }
        }

        function openChat(name) {
          window.location.href = '/agent-hub/agents/' + encodeURIComponent(name) + '/chat';
        }

        function escapeHtml(text) {
          var div = document.createElement('div');
          div.appendChild(document.createTextNode(text));
          return div.innerHTML;
        }

        loadAgents();
        setInterval(loadAgents, 10000);
      </script>
    </body>
    </html>
    """
  end
end
