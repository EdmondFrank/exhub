defmodule Exhub.Router.AgentHubView do
  @moduledoc """
  HTML view for the Agent Hub overview page.

  Lists chat conversations (most recently active first) and the registered
  agent profiles, and lets the user start a new conversation against any
  profile. Conversation state lives in SQLite, so this page is the durable
  entry point — opening a conversation rehydrates its transcript.
  """

  @doc """
  Returns the complete HTML for the Agent Hub overview page.
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
        .container { max-width: 1100px; margin: 0 auto; padding: 16px; }
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
        h1 { color: #58a6ff; font-size: 22px; }
        h2 { color: #f0f6fc; font-size: 15px; margin-bottom: 12px; }
        .subtitle { color: #8b949e; font-size: 13px; }
        .header-right, .row { display: flex; align-items: center; gap: 8px; }
        .nav-link, .btn {
          color: #58a6ff;
          text-decoration: none;
          font-size: 13px;
          padding: 6px 12px;
          border: 1px solid #30363d;
          border-radius: 6px;
          background: transparent;
          cursor: pointer;
          transition: background 0.15s;
        }
        .nav-link:hover, .btn:hover:not(:disabled) { background: #21262d; }
        .btn:disabled { opacity: 0.45; cursor: not-allowed; }
        .btn-primary { background: #238636; border-color: #2ea043; color: #fff; }
        .btn-primary:hover:not(:disabled) { background: #2ea043; }
        .btn-danger { border-color: #6e2c2c; color: #f85149; }
        .btn-danger:hover:not(:disabled) { background: #3d1d1d; }
        .panel {
          background: #161b22;
          border: 1px solid #30363d;
          border-radius: 10px;
          padding: 16px;
          margin-bottom: 20px;
        }
        .panel .row { flex-wrap: wrap; }
        select, input[type=text] {
          background: #0d1117;
          color: #c9d1d9;
          border: 1px solid #30363d;
          border-radius: 6px;
          padding: 7px 10px;
          font-size: 13px;
          font-family: inherit;
          min-width: 200px;
        }
        select:focus, input[type=text]:focus { outline: none; border-color: #1f6feb; }
        .conversation {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 12px;
          padding: 12px 14px;
          border: 1px solid #30363d;
          border-radius: 8px;
          background: #161b22;
          margin-bottom: 8px;
        }
        .conversation:hover { border-color: #1f6feb; }
        .conversation-main { min-width: 0; }
        .conversation-title {
          color: #f0f6fc;
          font-size: 15px;
          font-weight: 600;
          text-decoration: none;
          display: block;
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
        }
        .conversation-title:hover { color: #58a6ff; }
        .conversation-meta { color: #8b949e; font-size: 12px; }
        .badge {
          display: inline-flex;
          align-items: center;
          gap: 6px;
          padding: 2px 8px;
          border-radius: 12px;
          font-size: 11px;
          font-weight: 500;
          border: 1px solid #30363d;
          color: #8b949e;
        }
        .badge-running { border-color: #238636; color: #3fb950; }
        .badge-interrupted { border-color: #9e6a03; color: #d29922; }
        .dot { width: 7px; height: 7px; border-radius: 50%; background: #8b949e; }
        .dot.running { background: #3fb950; box-shadow: 0 0 6px #3fb950; }
        .dot.interrupted { background: #d29922; }
        .empty { color: #8b949e; font-size: 14px; padding: 24px 0; text-align: center; }
        .error {
          background: #3d1d1d;
          border: 1px solid #da3633;
          color: #ffb3b0;
          padding: 10px 12px;
          border-radius: 8px;
          margin-bottom: 16px;
          font-size: 13px;
        }
        .agents {
          display: grid;
          grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
          gap: 10px;
        }
        .agent-card {
          display: flex;
          flex-direction: column;
          gap: 10px;
          border: 1px solid #30363d;
          border-radius: 10px;
          padding: 12px;
          background: #0d1117;
          cursor: pointer;
          transition: border-color 0.15s, background 0.15s;
        }
        .agent-card:hover { border-color: #1f6feb; }
        .agent-card.selected { border-color: #1f6feb; background: #101a2b; }
        .agent-card-head {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 8px;
        }
        .agent-name { color: #f0f6fc; font-size: 14px; font-weight: 600; }
        .agent-section { display: flex; flex-direction: column; gap: 5px; }
        .agent-label {
          color: #8b949e;
          font-size: 10px;
          font-weight: 600;
          letter-spacing: 0.06em;
          text-transform: uppercase;
        }
        .tags { display: flex; flex-wrap: wrap; gap: 4px; }
        .tag {
          border: 1px solid #30363d;
          border-radius: 6px;
          padding: 1px 7px;
          font-size: 11px;
          color: #c9d1d9;
          background: #161b22;
          white-space: nowrap;
        }
        .tag .count { color: #8b949e; margin-left: 5px; font-size: 10px; }
        .tag-native { border-color: #1f6feb; color: #79c0ff; }
        .tag-mw { color: #8b949e; border-style: dashed; }
        .tag-hitl { border-color: #9e6a03; color: #d29922; }
        .tag-none { color: #6e7681; border-style: dashed; }
        .agent-hint {
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          gap: 6px;
          margin-top: 10px;
          font-size: 12px;
          color: #8b949e;
        }
        .agent-hint .tag { cursor: default; }
        .toast {
          position: fixed;
          bottom: 20px;
          right: 20px;
          background: #161b22;
          border: 1px solid #30363d;
          border-radius: 8px;
          padding: 12px 20px;
          font-size: 13px;
          box-shadow: 0 4px 12px rgba(0,0,0,0.4);
          transform: translateY(100px);
          opacity: 0;
          transition: transform 0.3s, opacity 0.3s;
          z-index: 1000;
        }
        .toast.show { transform: translateY(0); opacity: 1; }
        .toast.success { border-color: #238636; }
        .toast.error { border-color: #da3633; }
        @media (max-width: 720px) {
          .conversation { flex-direction: column; align-items: flex-start; }
          .agents { grid-template-columns: 1fr; }
          select, input[type=text] { width: 100%; }
        }
      </style>
    </head>
    <body>
      <header>
        <div class="container">
          <div>
            <h1>Agent Hub</h1>
            <p class="subtitle">Persistent conversations with sagents agents</p>
          </div>
          <div class="header-right">
            <a href="/dashboard" class="nav-link">Token Dashboard</a>
            <button class="btn" onclick="loadAll()">↻ Refresh</button>
          </div>
        </div>
      </header>

      <div class="container">
        <div id="error"></div>

        <div class="panel">
          <h2>New conversation</h2>
          <div class="row">
            <select id="agent-select"></select>
            <input type="text" id="new-title" placeholder="Title (optional)">
            <button class="btn btn-primary" id="create-btn" onclick="createConversation()">Start conversation</button>
          </div>
          <div class="agent-hint" id="agent-hint"></div>
        </div>

        <div class="panel">
          <h2>Conversations</h2>
          <div id="conversations"><div class="empty">Loading…</div></div>
        </div>

        <div class="panel">
          <h2>Agent profiles</h2>
          <p class="subtitle" style="margin-bottom: 12px;">
            Tool groups each profile can call. Click a profile to select it for a new conversation.
          </p>
          <div class="agents" id="agents"><span class="subtitle">Loading…</span></div>
        </div>
      </div>

      <div class="toast" id="toast"></div>

      <script>
        function escapeHtml(text) {
          var div = document.createElement('div');
          div.appendChild(document.createTextNode(text == null ? '' : String(text)));
          return div.innerHTML;
        }

        function showToast(message, type) {
          var toast = document.getElementById('toast');
          toast.textContent = message;
          toast.className = 'toast show ' + (type || '');
          setTimeout(function() { toast.className = 'toast'; }, 3000);
        }

        function showError(message) {
          document.getElementById('error').innerHTML =
            '<div class="error">' + escapeHtml(message) + '</div>';
        }

        function timeAgo(iso) {
          if (!iso) return 'never';
          var then = new Date(iso).getTime();
          if (isNaN(then)) return iso;
          var seconds = Math.round((Date.now() - then) / 1000);
          if (seconds < 60) return 'just now';
          if (seconds < 3600) return Math.round(seconds / 60) + 'm ago';
          if (seconds < 86400) return Math.round(seconds / 3600) + 'h ago';
          return Math.round(seconds / 86400) + 'd ago';
        }

        async function loadAll() {
          document.getElementById('error').innerHTML = '';
          await Promise.all([loadAgents(), loadConversations()]);
        }

        var AGENTS = [];

        function toolTag(text, cls, count) {
          return '<span class="tag ' + (cls || '') + '">' + escapeHtml(text) +
            (count ? '<span class="count">' + count + '</span>' : '') + '</span>';
        }

        function middlewareTag(middleware) {
          var interrupts = middleware.interrupts || [];
          var chips = [toolTag(middleware.name, interrupts.length ? 'tag-hitl' : 'tag-mw')];

          interrupts.forEach(function(tool) {
            chips.push(toolTag(tool, 'tag-hitl'));
          });

          return chips.join('');
        }

        function agentCard(agent, index) {
          var groups = (agent.tool_groups || []).map(function(group) {
            return toolTag(group.name, '', group.tool_count || null);
          });

          var native = (agent.native_tools || []).map(function(name) {
            return toolTag(name, 'tag-native');
          });

          var middleware = (agent.middleware || []).map(middlewareTag);

          function section(label, tags) {
            var body = tags.length
              ? '<div class="tags">' + tags.join('') + '</div>'
              : '<div class="tags"><span class="tag tag-none">none</span></div>';

            return '<div class="agent-section"><div class="agent-label">' + label + '</div>' + body + '</div>';
          }

          var live = agent.running
            ? '<span class="badge badge-running"><span class="dot running"></span>Live</span>'
            : '';

          return '<div class="agent-card" data-agent-index="' + index + '">' +
            '<div class="agent-card-head"><span class="agent-name">' + escapeHtml(agent.name) + '</span>' + live + '</div>' +
            section('MCP tool groups', groups) +
            section('Native tools', native) +
            section('Middleware', middleware) +
          '</div>';
        }

        function selectedAgent() {
          var select = document.getElementById('agent-select');

          return AGENTS.filter(function(agent) {
            return agent.name === select.value;
          })[0];
        }

        function markSelectedCard() {
          var current = document.getElementById('agent-select').value;
          var cards = document.querySelectorAll('#agents .agent-card');

          for (var i = 0; i < cards.length; i++) {
            var agent = AGENTS[Number(cards[i].getAttribute('data-agent-index'))];
            cards[i].classList.toggle('selected', !!agent && agent.name === current);
          }
        }

        function renderAgentHint() {
          var hint = document.getElementById('agent-hint');
          var agent = selectedAgent();

          markSelectedCard();

          if (!agent) {
            hint.innerHTML = '';
            return;
          }

          var groups = (agent.tool_groups || []).map(function(group) {
            return toolTag(group.name, '', group.tool_count || null);
          });

          var parts = ['<span>Tool groups:</span>'].concat(
            groups.length ? groups : ['<span class="tag tag-none">none</span>']
          );

          var nativeCount = (agent.native_tools || []).length;

          if (nativeCount > 0) {
            parts.push('<span>' + nativeCount + ' native tool' + (nativeCount === 1 ? '' : 's') + '</span>');
          }

          hint.innerHTML = parts.join('');
        }

        async function loadAgents() {
          try {
            var data = await (await fetch('/agent-hub/agents')).json();
            AGENTS = data.agents || [];

            var select = document.getElementById('agent-select');
            var previous = select.value;

            select.innerHTML = AGENTS.map(function(agent) {
              return '<option value="' + escapeHtml(agent.name) + '">' + escapeHtml(agent.name) + '</option>';
            }).join('');

            if (previous && AGENTS.some(function(agent) { return agent.name === previous; })) {
              select.value = previous;
            }

            document.getElementById('agents').innerHTML = AGENTS.map(agentCard).join('') ||
              '<span class="subtitle">No agent profiles registered</span>';

            renderAgentHint();
          } catch (err) {
            showError('Failed to load agents: ' + err.message);
          }
        }

        function pickAgent(name) {
          document.getElementById('agent-select').value = name;
          renderAgentHint();
        }

        async function loadConversations() {
          try {
            var data = await (await fetch('/agent-hub/conversations')).json();
            var conversations = data.conversations || [];
            var container = document.getElementById('conversations');

            if (conversations.length === 0) {
              container.innerHTML = '<div class="empty">No conversations yet — start one above.</div>';
              return;
            }

            container.innerHTML = conversations.map(function(conv) {
              var state = conv.status === 'interrupted' ? 'interrupted' : (conv.running ? 'running' : 'idle');
              var badge = conv.running
                ? '<span class="badge badge-' + (state === 'interrupted' ? 'interrupted' : 'running') + '">' +
                    '<span class="dot ' + (state === 'interrupted' ? 'interrupted' : 'running') + '"></span>' +
                    (state === 'interrupted' ? 'Needs approval' : 'Live') + '</span>'
                : '<span class="badge">Stopped</span>';

              var title = conv.title || conv.agent_name + ' chat';

              return '<div class="conversation">' +
                '<div class="conversation-main">' +
                  '<a class="conversation-title" href="/agent-hub/c/' + encodeURIComponent(conv.id) + '">' +
                    escapeHtml(title) + '</a>' +
                  '<div class="conversation-meta">' +
                    escapeHtml(conv.agent_name) + ' · last message ' +
                    escapeHtml(timeAgo(conv.last_message_at || conv.created_at)) +
                  '</div>' +
                '</div>' +
                '<div class="row">' + badge +
                  '<a class="btn" href="/agent-hub/c/' + encodeURIComponent(conv.id) + '">Open</a>' +
                  '<button class="btn" onclick="renameConversation(\\'' + escapeHtml(conv.id) + '\\', ' +
                    escapeHtml(JSON.stringify(title)) + ')">Rename</button>' +
                  '<button class="btn btn-danger" onclick="deleteConversation(\\'' + escapeHtml(conv.id) + '\\')">Delete</button>' +
                '</div>' +
              '</div>';
            }).join('');
          } catch (err) {
            showError('Failed to load conversations: ' + err.message);
          }
        }

        async function createConversation() {
          var agent = document.getElementById('agent-select').value;
          var title = document.getElementById('new-title').value.trim();
          var button = document.getElementById('create-btn');

          if (!agent) {
            showToast('Pick an agent profile first', 'error');
            return;
          }

          button.disabled = true;

          try {
            var response = await fetch('/agent-hub/conversations', {
              method: 'POST',
              headers: { 'Content-Type': 'application/json' },
              body: JSON.stringify({ agent_name: agent, title: title || null })
            });
            var result = await response.json();

            if (!response.ok) throw new Error(result.error || ('HTTP ' + response.status));

            window.location.href = '/agent-hub/c/' + encodeURIComponent(result.conversation.id);
          } catch (err) {
            showToast('Failed to create conversation: ' + err.message, 'error');
            button.disabled = false;
          }
        }

        async function renameConversation(id, currentTitle) {
          var title = prompt('Conversation title', currentTitle);
          if (title === null) return;

          try {
            await fetch('/agent-hub/conversations/' + encodeURIComponent(id), {
              method: 'PATCH',
              headers: { 'Content-Type': 'application/json' },
              body: JSON.stringify({ title: title })
            });
            showToast('Renamed', 'success');
            loadConversations();
          } catch (err) {
            showToast('Rename failed: ' + err.message, 'error');
          }
        }

        async function deleteConversation(id) {
          if (!confirm('Delete this conversation and its transcript?')) return;

          try {
            await fetch('/agent-hub/conversations/' + encodeURIComponent(id), { method: 'DELETE' });
            showToast('Deleted', 'success');
            loadConversations();
          } catch (err) {
            showToast('Delete failed: ' + err.message, 'error');
          }
        }

        document.getElementById('agents').addEventListener('click', function(event) {
          var card = event.target.closest('.agent-card');
          if (!card) return;

          var agent = AGENTS[Number(card.getAttribute('data-agent-index'))];
          if (agent) pickAgent(agent.name);
        });

        document.getElementById('agent-select').addEventListener('change', renderAgentHint);

        loadAll();
        setInterval(loadConversations, 10000);
      </script>
    </body>
    </html>
    """
  end
end
