defmodule Exhub.Router.AgentHubChatView do
  @moduledoc """
  HTML view for a single Agent Hub conversation.

  The transcript is rendered on the server from SQLite display messages, so a
  reload (or a second browser) shows the whole conversation. New turns stream
  over SSE; a human-in-the-loop interrupt renders an approval panel directly in
  the transcript.
  """

  @doc """
  Returns the complete HTML for one conversation.

  `conversation` is a `Exhub.Sagents.Store` conversation map and `messages` its
  display messages, oldest first.
  """
  @spec render(map(), [map()]) :: String.t()
  def render(conversation, messages) do
    title = conversation.title || "#{conversation.agent_name} chat"
    transcript = messages |> Enum.map(&render_message/1) |> Enum.join("\n")

    body =
      if transcript == "" do
        """
        <div class="welcome-message">
          Send a message to start this conversation with #{escape(conversation.agent_name)}
        </div>
        """
      else
        transcript
      end

    """
    <!DOCTYPE html>
    <html lang="en">
    <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>#{escape(title)} — Agent Hub</title>
      <style>
        * { box-sizing: border-box; margin: 0; padding: 0; }
        body {
          font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif;
          background: #0d1117;
          color: #c9d1d9;
          line-height: 1.6;
          height: 100vh;
          display: flex;
          flex-direction: column;
        }
        header {
          background: #161b22;
          border-bottom: 1px solid #30363d;
          padding: 12px 16px;
          flex-shrink: 0;
        }
        .header-content {
          max-width: 960px;
          margin: 0 auto;
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 12px;
        }
        .header-left { display: flex; align-items: center; gap: 12px; min-width: 0; }
        .back-btn, .btn {
          color: #58a6ff;
          text-decoration: none;
          font-size: 13px;
          padding: 6px 12px;
          border: 1px solid #30363d;
          border-radius: 6px;
          background: transparent;
          cursor: pointer;
          transition: background 0.15s;
          white-space: nowrap;
        }
        .back-btn:hover, .btn:hover:not(:disabled) { background: #21262d; }
        .btn:disabled { opacity: 0.45; cursor: not-allowed; }
        .btn-danger { border-color: #6e2c2c; color: #f85149; }
        .btn-danger:hover:not(:disabled) { background: #3d1d1d; }
        h1 {
          color: #f0f6fc;
          font-size: 17px;
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
        }
        .agent-status {
          display: inline-flex;
          align-items: center;
          gap: 6px;
          font-size: 12px;
          color: #8b949e;
          white-space: nowrap;
        }
        .dot { width: 8px; height: 8px; border-radius: 50%; background: #8b949e; }
        .dot.running { background: #3fb950; box-shadow: 0 0 6px #3fb950; }
        .dot.interrupted { background: #d29922; box-shadow: 0 0 6px #d29922; }
        .header-right { display: flex; gap: 8px; }
        .chat-container {
          flex: 1;
          overflow: hidden;
          display: flex;
          flex-direction: column;
          max-width: 960px;
          width: 100%;
          margin: 0 auto;
          padding: 0 16px;
        }
        .messages { flex: 1; overflow-y: auto; padding: 16px 0; }
        .message { margin-bottom: 16px; display: flex; gap: 12px; }
        .message.user { flex-direction: row-reverse; }
        .message-avatar {
          width: 32px;
          height: 32px;
          border-radius: 50%;
          display: flex;
          align-items: center;
          justify-content: center;
          font-size: 13px;
          font-weight: 600;
          flex-shrink: 0;
        }
        .message.user .message-avatar { background: #1f6feb; color: #fff; }
        .message.assistant .message-avatar { background: #238636; color: #fff; }
        .message.system .message-avatar { background: #30363d; color: #c9d1d9; }
        .message-content {
          max-width: 76%;
          padding: 10px 14px;
          border-radius: 12px;
          font-size: 14px;
          line-height: 1.5;
          white-space: pre-wrap;
          word-break: break-word;
        }
        .message.user .message-content {
          background: #1f6feb;
          color: #fff;
          border-bottom-right-radius: 4px;
        }
        .message.assistant .message-content {
          background: #21262d;
          color: #c9d1d9;
          border-bottom-left-radius: 4px;
        }
        .message.system .message-content {
          background: #161b22;
          border: 1px solid #30363d;
          color: #8b949e;
          font-size: 13px;
        }
        .message-content pre, .tool-body pre {
          background: #161b22;
          border: 1px solid #30363d;
          border-radius: 6px;
          padding: 10px;
          margin: 8px 0 0;
          overflow-x: auto;
          font-size: 12px;
          white-space: pre-wrap;
          word-break: break-word;
          max-height: 320px;
          overflow-y: auto;
        }
        .message-content code, pre {
          font-family: 'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, monospace;
        }
        .thinking {
          border-left: 2px solid #30363d;
          padding-left: 10px;
          color: #8b949e;
          font-style: italic;
          font-size: 13px;
        }
        .tool-call, .tool-result {
          max-width: 76%;
          background: #161b22;
          border: 1px solid #30363d;
          border-radius: 8px;
          padding: 10px 12px;
          margin-bottom: 16px;
          font-size: 12px;
          color: #8b949e;
        }
        .tool-header { display: flex; align-items: center; gap: 8px; flex-wrap: wrap; }
        .tool-name { color: #58a6ff; font-weight: 600; }
        .tool-status {
          border: 1px solid #30363d;
          border-radius: 999px;
          padding: 0 6px;
          font-size: 10px;
          text-transform: uppercase;
          letter-spacing: 0.04em;
        }
        .tool-status.completed { border-color: #238636; color: #3fb950; }
        .tool-status.failed, .tool-status.cancelled { border-color: #6e2c2c; color: #f85149; }
        .tool-status.pending, .tool-status.executing { border-color: #9e6a03; color: #d29922; }
        .tool-status.interrupted { border-color: #9e6a03; color: #d29922; }
        .tool-body { margin-top: 6px; }
        .approval {
          max-width: 76%;
          background: #1c2128;
          border: 1px solid #d29922;
          border-radius: 8px;
          padding: 12px 14px;
          margin-bottom: 16px;
          font-size: 13px;
        }
        .approval h3 { font-size: 13px; color: #d29922; margin-bottom: 8px; }
        .approval-item {
          border-top: 1px solid #30363d;
          padding: 8px 0;
          display: flex;
          align-items: flex-start;
          justify-content: space-between;
          gap: 10px;
        }
        .approval-item:first-of-type { border-top: none; }
        .approval-detail { min-width: 0; }
        .approval-actions { display: flex; gap: 6px; flex-shrink: 0; }
        .typing-indicator { display: inline-flex; gap: 4px; padding: 2px 0; }
        .typing-indicator span {
          width: 6px;
          height: 6px;
          background: #8b949e;
          border-radius: 50%;
          animation: typing 1.4s infinite;
        }
        .typing-indicator span:nth-child(2) { animation-delay: 0.2s; }
        .typing-indicator span:nth-child(3) { animation-delay: 0.4s; }
        @keyframes typing {
          0%, 100% { opacity: 0.3; transform: translateY(0); }
          50% { opacity: 1; transform: translateY(-4px); }
        }
        .input-area { padding: 16px 0; border-top: 1px solid #30363d; flex-shrink: 0; }
        .input-wrapper { display: flex; gap: 8px; }
        .input-wrapper textarea {
          flex: 1;
          background: #21262d;
          border: 1px solid #30363d;
          border-radius: 8px;
          padding: 10px 14px;
          color: #c9d1d9;
          font-size: 14px;
          font-family: inherit;
          resize: none;
          min-height: 44px;
          max-height: 200px;
          line-height: 1.5;
        }
        .input-wrapper textarea:focus { outline: none; border-color: #1f6feb; }
        .input-wrapper textarea::placeholder { color: #484f58; }
        .send-btn {
          background: #238636;
          color: #fff;
          border: none;
          border-radius: 8px;
          padding: 10px 20px;
          font-size: 14px;
          font-weight: 500;
          cursor: pointer;
          align-self: flex-end;
        }
        .send-btn:hover:not(:disabled) { background: #2ea043; }
        .send-btn:disabled { opacity: 0.5; cursor: not-allowed; }
        .toast {
          position: fixed;
          bottom: 20px;
          right: 20px;
          background: #161b22;
          border: 1px solid #30363d;
          border-radius: 8px;
          padding: 12px 20px;
          font-size: 13px;
          transform: translateY(100px);
          opacity: 0;
          transition: transform 0.3s, opacity 0.3s;
          z-index: 1000;
        }
        .toast.show { transform: translateY(0); opacity: 1; }
        .toast.error { border-color: #da3633; color: #ffb3b0; }
        .welcome-message { text-align: center; color: #8b949e; padding: 40px 20px; font-size: 14px; }
        @media (max-width: 768px) {
          .message-content, .tool-call, .tool-result, .approval { max-width: 88%; }
        }
      </style>
    </head>
    <body data-conversation-id="#{escape(conversation.id)}">
      <header>
        <div class="header-content">
          <div class="header-left">
            <a href="/agent-hub" class="back-btn">← All conversations</a>
            <h1>#{escape(title)}</h1>
            <span class="agent-status">
              <span class="dot" id="status-dot"></span>
              <span id="status-text">#{escape(conversation.agent_name)}</span>
            </span>
          </div>
          <div class="header-right">
            <button class="btn" onclick="renameConversation()">Rename</button>
            <button class="btn" onclick="resetConversation()">Reset</button>
            <button class="btn btn-danger" onclick="stopConversation()">Stop</button>
          </div>
        </div>
      </header>

      <div class="chat-container">
        <div class="messages" id="messages">#{body}</div>

        <div class="input-area">
          <div class="input-wrapper">
            <textarea
              id="message-input"
              placeholder="Type your message…"
              rows="1"
              onkeydown="handleKeyDown(event)"
              oninput="autoResize(this)"
            ></textarea>
            <button class="send-btn" id="send-btn" onclick="sendMessage()">Send</button>
          </div>
        </div>
      </div>

      <div class="toast" id="toast"></div>

      <script>
        var conversationId = document.body.dataset.conversationId;
        var isStreaming = false;
        var currentAssistantMessage = null;

        function escapeHtml(text) {
          var div = document.createElement('div');
          div.appendChild(document.createTextNode(text == null ? '' : String(text)));
          return div.innerHTML;
        }

        function showToast(message, type) {
          var toast = document.getElementById('toast');
          toast.textContent = message;
          toast.className = 'toast show ' + (type || '');
          setTimeout(function() { toast.className = 'toast'; }, 4000);
        }

        function scrollToBottom() {
          var messages = document.getElementById('messages');
          messages.scrollTop = messages.scrollHeight;
        }

        function handleKeyDown(event) {
          if (event.key === 'Enter' && !event.shiftKey) {
            event.preventDefault();
            sendMessage();
          }
        }

        function autoResize(textarea) {
          textarea.style.height = 'auto';
          textarea.style.height = Math.min(textarea.scrollHeight, 200) + 'px';
        }

        function clearWelcome() {
          var welcome = document.querySelector('.welcome-message');
          if (welcome) welcome.remove();
        }

        function addMessage(role, content) {
          clearWelcome();
          var messagesDiv = document.getElementById('messages');
          var messageDiv = document.createElement('div');
          messageDiv.className = 'message ' + role;

          var avatar = document.createElement('div');
          avatar.className = 'message-avatar';
          avatar.textContent = role === 'user' ? 'U' : (role === 'system' ? 'S' : 'A');

          var contentDiv = document.createElement('div');
          contentDiv.className = 'message-content';
          contentDiv.textContent = content || '';

          messageDiv.appendChild(avatar);
          messageDiv.appendChild(contentDiv);
          messagesDiv.appendChild(messageDiv);
          scrollToBottom();

          return contentDiv;
        }

        function addTypingIndicator() {
          clearWelcome();
          var messagesDiv = document.getElementById('messages');
          var messageDiv = document.createElement('div');
          messageDiv.className = 'message assistant';
          messageDiv.id = 'typing-indicator';

          var avatar = document.createElement('div');
          avatar.className = 'message-avatar';
          avatar.textContent = 'A';

          var contentDiv = document.createElement('div');
          contentDiv.className = 'message-content';
          contentDiv.innerHTML = '<div class="typing-indicator"><span></span><span></span><span></span></div>';

          messageDiv.appendChild(avatar);
          messageDiv.appendChild(contentDiv);
          messagesDiv.appendChild(messageDiv);
          scrollToBottom();
        }

        function removeTypingIndicator() {
          var indicator = document.getElementById('typing-indicator');
          if (indicator) indicator.remove();
        }

        function addToolCall(tool) {
          removeTypingIndicator();
          var messagesDiv = document.getElementById('messages');
          var div = document.createElement('div');
          div.className = 'tool-call';
          div.innerHTML = '<div class="tool-header">🔧 <span class="tool-name">' + escapeHtml(tool) +
            '</span><span class="tool-status pending">running</span></div>';
          messagesDiv.appendChild(div);
          scrollToBottom();
          currentAssistantMessage = null;
        }

        function addInterruptApproval(interrupt) {
          removeTypingIndicator();
          var requests = (interrupt && interrupt.action_requests) || [];
          var messagesDiv = document.getElementById('messages');
          var panel = document.createElement('div');
          panel.className = 'approval';

          var html = '<h3>Approval required</h3>';

          if (requests.length === 0) {
            html += '<div>This turn paused for approval, but no action details were returned.</div>';
          }

          requests.forEach(function(request, index) {
            var args = request.arguments == null ? '' : JSON.stringify(request.arguments, null, 2);
            html += '<div class="approval-item">' +
              '<div class="approval-detail"><span class="tool-name">' + escapeHtml(request.tool_name || 'tool') +
              '</span><pre>' + escapeHtml(args) + '</pre></div>' +
              '<div class="approval-actions">' +
                '<button class="btn" onclick="decide(' + index + ', \\'approve\\')">Approve</button>' +
                '<button class="btn btn-danger" onclick="decide(' + index + ', \\'reject\\')">Reject</button>' +
              '</div>' +
            '</div>';
          });

          html += '<div class="row" style="margin-top:8px; display:flex; gap:8px;">' +
            '<button class="btn" onclick="decideAll(\\'approve\\')">Approve all</button>' +
            '<button class="btn btn-danger" onclick="decideAll(\\'reject\\')">Reject all</button>' +
          '</div>';

          panel.innerHTML = html;
          panel.dataset.requests = String(requests.length);
          messagesDiv.appendChild(panel);
          scrollToBottom();
        }

        async function sendMessage() {
          var input = document.getElementById('message-input');
          var message = input.value.trim();
          if (!message || isStreaming) return;

          addMessage('user', message);
          input.value = '';
          input.style.height = 'auto';

          isStreaming = true;
          document.getElementById('send-btn').disabled = true;
          addTypingIndicator();

          try {
            var response = await fetch('/agent-hub/c/' + encodeURIComponent(conversationId) + '/chat', {
              method: 'POST',
              headers: { 'Content-Type': 'application/json' },
              body: JSON.stringify({ message: message })
            });

            if (!response.ok) throw new Error('HTTP ' + response.status);

            removeTypingIndicator();
            currentAssistantMessage = addMessage('assistant', '');

            var reader = response.body.getReader();
            var decoder = new TextDecoder();
            var buffer = '';
            var finished = false;

            while (true) {
              var result = await reader.read();
              if (result.done) break;

              buffer += decoder.decode(result.value, { stream: true });
              var lines = buffer.split('\\n');
              buffer = lines.pop() || '';

              for (var i = 0; i < lines.length; i++) {
                var line = lines[i].trim();
                if (!line.startsWith('data: ')) continue;

                var data = line.slice(6);
                if (data === '[DONE]') { finished = true; continue; }

                try {
                  finished = handleStreamEvent(JSON.parse(data)) || finished;
                } catch (e) {
                  console.error('Failed to parse event:', e);
                }
              }
            }
          } catch (err) {
            removeTypingIndicator();
            showToast('Chat failed: ' + err.message, 'error');
          } finally {
            isStreaming = false;
            document.getElementById('send-btn').disabled = false;
            currentAssistantMessage = null;
            checkStatus();
          }
        }

        function handleStreamEvent(event) {
          switch (event.type) {
            case 'delta':
              if (!currentAssistantMessage) currentAssistantMessage = addMessage('assistant', '');
              if (event.text) {
                currentAssistantMessage.textContent += event.text;
                scrollToBottom();
              }
              return false;

            case 'tool_call':
              addToolCall(event.tool);
              return false;

            case 'tool_result':
              return false;

            case 'complete':
              if (currentAssistantMessage && event.text) {
                currentAssistantMessage.textContent = event.text;
              }
              return true;

            case 'interrupted':
              addInterruptApproval(event.interrupt);
              return true;

            case 'error':
              removeTypingIndicator();
              showToast('Agent error: ' + event.error, 'error');
              return true;

            default:
              return false;
          }
        }

        async function decide(index, decision) {
          var panel = document.querySelector('.approval');
          var count = panel ? parseInt(panel.dataset.requests || '0', 10) : 0;
          var decisions = [];
          for (var i = 0; i < count; i++) {
            decisions.push({ type: i === index ? decision : 'reject' });
          }
          await submitDecisions(decisions);
        }

        async function decideAll(decision) {
          var panel = document.querySelector('.approval');
          var count = panel ? parseInt(panel.dataset.requests || '0', 10) : 0;
          var decisions = [];
          for (var i = 0; i < count; i++) decisions.push({ type: decision });
          await submitDecisions(decisions);
        }

        async function submitDecisions(decisions) {
          try {
            var response = await fetch('/agent-hub/c/' + encodeURIComponent(conversationId) + '/decision', {
              method: 'POST',
              headers: { 'Content-Type': 'application/json' },
              body: JSON.stringify({ decisions: decisions })
            });

            if (!response.ok) throw new Error('HTTP ' + response.status);

            showToast('Decision sent — reloading transcript', 'success');
            setTimeout(function() { window.location.reload(); }, 800);
          } catch (err) {
            showToast('Failed to send decision: ' + err.message, 'error');
          }
        }

        async function checkStatus() {
          try {
            var status = await (await fetch('/agent-hub/c/' + encodeURIComponent(conversationId) + '/status')).json();
            var dot = document.getElementById('status-dot');
            var text = document.getElementById('status-text');

            if (status.status === 'interrupted') {
              dot.className = 'dot interrupted';
              text.textContent = 'Needs approval';
            } else if (status.running) {
              dot.className = 'dot running';
              text.textContent = status.status === 'running' ? 'Running' : 'Idle (live)';
            } else {
              dot.className = 'dot';
              text.textContent = 'Stopped';
            }
          } catch (err) {
            console.error('Status check failed:', err);
          }
        }

        async function renameConversation() {
          var title = prompt('Conversation title', document.querySelector('h1').textContent);
          if (title === null) return;

          try {
            await fetch('/agent-hub/conversations/' + encodeURIComponent(conversationId), {
              method: 'PATCH',
              headers: { 'Content-Type': 'application/json' },
              body: JSON.stringify({ title: title })
            });
            document.querySelector('h1').textContent = title;
            document.title = title + ' — Agent Hub';
            showToast('Renamed', 'success');
          } catch (err) {
            showToast('Rename failed: ' + err.message, 'error');
          }
        }

        async function resetConversation() {
          if (!confirm('Clear this transcript and the agent state?')) return;

          try {
            await fetch('/agent-hub/c/' + encodeURIComponent(conversationId) + '/reset', { method: 'POST' });
            showToast('Conversation reset', 'success');
            setTimeout(function() { window.location.reload(); }, 500);
          } catch (err) {
            showToast('Reset failed: ' + err.message, 'error');
          }
        }

        async function stopConversation() {
          try {
            await fetch('/agent-hub/c/' + encodeURIComponent(conversationId) + '/stop', { method: 'POST' });
            showToast('Agent stopped', 'success');
            checkStatus();
          } catch (err) {
            showToast('Stop failed: ' + err.message, 'error');
          }
        }

        async function loadPendingInterrupt() {
          if (document.querySelector('.approval')) return;

          try {
            var data = await (await fetch('/agent-hub/c/' + encodeURIComponent(conversationId) + '/interrupt')).json();
            if (data.interrupt) addInterruptApproval(data.interrupt);
          } catch (err) {
            console.error('Interrupt check failed:', err);
          }
        }

        checkStatus();
        loadPendingInterrupt();
        setInterval(checkStatus, 5000);
        scrollToBottom();
      </script>
    </body>
    </html>
    """
  end

  # --- transcript rendering ---

  defp render_message(%{content_type: "tool_call"} = message) do
    content = Map.get(message, :content) || %{}
    name = content["name"] || content["tool_name"] || "tool"
    status = message.status || "pending"
    arguments = format_arguments(content["arguments"])
    display_text = content["display_text"]

    """
    <div class="tool-call">
      <div class="tool-header">
        <span>🔧</span>
        <span class="tool-name">#{escape(name)}</span>
        <span class="tool-status #{escape(status)}">#{escape(status)}</span>
      </div>
      #{if display_text, do: "<div class=\"tool-body\">#{escape(display_text)}</div>", else: ""}
      #{if arguments, do: "<div class=\"tool-body\"><pre>#{escape(arguments)}</pre></div>", else: ""}
    </div>
    """
  end

  defp render_message(%{content_type: "tool_result"} = message) do
    content = Map.get(message, :content) || %{}
    name = content["name"] || "tool"
    text = truncate(content["content"] || content["display_text"] || "", 4000)
    is_error = content["is_error"]
    label = if is_error, do: "failed", else: "result"

    """
    <div class="tool-result">
      <div class="tool-header">
        <span>↩︎</span>
        <span class="tool-name">#{escape(name)}</span>
        <span class="tool-status #{if is_error, do: "failed", else: "completed"}">#{label}</span>
      </div>
      #{if text == "", do: "", else: "<div class=\"tool-body\"><pre>#{escape(text)}</pre></div>"}
    </div>
    """
  end

  defp render_message(%{content_type: "thinking"} = message) do
    text = (message.content || %{})["text"] || ""

    """
    <div class="message assistant">
      <div class="message-avatar">A</div>
      <div class="message-content"><div class="thinking">#{escape(text)}</div></div>
    </div>
    """
  end

  defp render_message(%{content_type: "error"} = message) do
    text = (message.content || %{})["text"] || (message.content || %{})["error"] || ""

    """
    <div class="message system">
      <div class="message-avatar">!</div>
      <div class="message-content">#{escape(text)}</div>
    </div>
    """
  end

  defp render_message(message) do
    text = (message.content || %{})["text"] || ""
    role = message_role(message.message_type)

    """
    <div class="message #{role}">
      <div class="message-avatar">#{avatar(role)}</div>
      <div class="message-content">#{escape(text)}</div>
    </div>
    """
  end

  defp message_role("user"), do: "user"
  defp message_role("system"), do: "system"
  defp message_role(_role), do: "assistant"

  defp avatar("user"), do: "U"
  defp avatar("system"), do: "S"
  defp avatar(_role), do: "A"

  defp format_arguments(nil), do: nil
  defp format_arguments(arguments) when is_binary(arguments), do: arguments

  defp format_arguments(arguments) when is_map(arguments) do
    if map_size(arguments) == 0 do
      nil
    else
      case Jason.encode(arguments, pretty: true) do
        {:ok, json} -> json
        _ -> inspect(arguments)
      end
    end
  end

  defp format_arguments(other), do: inspect(other)

  defp truncate(text, limit) when is_binary(text) and byte_size(text) > limit do
    binary_part(text, 0, limit) <> "\n… (truncated)"
  end

  defp truncate(text, _limit) when is_binary(text), do: text
  defp truncate(other, _limit), do: inspect(other)

  defp escape(nil), do: ""

  defp escape(value) when is_binary(value) do
    value
    |> String.replace("&", "&amp;")
    |> String.replace("<", "&lt;")
    |> String.replace(">", "&gt;")
    |> String.replace("\"", "&quot;")
    |> String.replace("'", "&#39;")
  end

  defp escape(value), do: value |> to_string() |> escape()
end
