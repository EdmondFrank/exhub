defmodule Exhub.Router.AgentHubChatView do
  @moduledoc """
  HTML view for the Agent Hub chat page.

  Provides a real-time chat interface for a specific agent
  using Server-Sent Events (SSE) for streaming responses.
  """

  @doc """
  Returns the complete HTML content for the chat page.

  ## Parameters
    - `agent_name` — the name of the agent to chat with
  """
  @spec render(String.t()) :: String.t()
  def render(agent_name) do
    """
    <!DOCTYPE html>
    <html lang="en">
    <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>Chat with #{agent_name} — Agent Hub</title>
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
          max-width: 900px;
          margin: 0 auto;
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 12px;
        }
        .header-left {
          display: flex;
          align-items: center;
          gap: 12px;
        }
        .back-btn {
          color: #58a6ff;
          text-decoration: none;
          font-size: 13px;
          padding: 6px 12px;
          border: 1px solid #30363d;
          border-radius: 6px;
          transition: background 0.15s;
        }
        .back-btn:hover {
          background: #21262d;
        }
        h1 {
          color: #58a6ff;
          font-size: 18px;
        }
        .agent-status {
          display: inline-flex;
          align-items: center;
          gap: 6px;
          font-size: 12px;
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
        .header-right {
          display: flex;
          gap: 8px;
        }
        .btn {
          padding: 6px 12px;
          border-radius: 6px;
          font-size: 12px;
          cursor: pointer;
          border: none;
          transition: background 0.15s;
        }
        .btn-reset {
          background: #30363d;
          color: #c9d1d9;
          border: 1px solid #484f58;
        }
        .btn-reset:hover {
          background: #484f58;
        }
        .btn-stop {
          background: #da3633;
          color: white;
        }
        .btn-stop:hover {
          background: #f85149;
        }
        .chat-container {
          flex: 1;
          overflow: hidden;
          display: flex;
          flex-direction: column;
          max-width: 900px;
          width: 100%;
          margin: 0 auto;
          padding: 0 16px;
        }
        .messages {
          flex: 1;
          overflow-y: auto;
          padding: 16px 0;
        }
        .message {
          margin-bottom: 16px;
          display: flex;
          gap: 12px;
        }
        .message.user {
          flex-direction: row-reverse;
        }
        .message-avatar {
          width: 32px;
          height: 32px;
          border-radius: 50%;
          display: flex;
          align-items: center;
          justify-content: center;
          font-size: 14px;
          font-weight: 600;
          flex-shrink: 0;
        }
        .message.user .message-avatar {
          background: #1f6feb;
          color: white;
        }
        .message.assistant .message-avatar {
          background: #238636;
          color: white;
        }
        .message-content {
          max-width: 75%;
          padding: 10px 14px;
          border-radius: 12px;
          font-size: 14px;
          line-height: 1.5;
        }
        .message.user .message-content {
          background: #1f6feb;
          color: white;
          border-bottom-right-radius: 4px;
        }
        .message.assistant .message-content {
          background: #21262d;
          color: #c9d1d9;
          border-bottom-left-radius: 4px;
        }
        .message-content pre {
          background: #161b22;
          border: 1px solid #30363d;
          border-radius: 6px;
          padding: 10px;
          margin: 8px 0;
          overflow-x: auto;
          font-size: 13px;
        }
        .message-content code {
          font-family: 'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, monospace;
        }
        .tool-call {
          background: #161b22;
          border: 1px solid #30363d;
          border-radius: 6px;
          padding: 8px 12px;
          margin: 8px 0;
          font-size: 12px;
          color: #8b949e;
        }
        .tool-call .tool-name {
          color: #58a6ff;
          font-weight: 500;
        }
        .typing-indicator {
          display: inline-flex;
          gap: 4px;
          padding: 8px 12px;
        }
        .typing-indicator span {
          width: 6px;
          height: 6px;
          background: #8b949e;
          border-radius: 50%;
          animation: typing 1.4s infinite;
        }
        .typing-indicator span:nth-child(2) {
          animation-delay: 0.2s;
        }
        .typing-indicator span:nth-child(3) {
          animation-delay: 0.4s;
        }
        @keyframes typing {
          0%, 100% { opacity: 0.3; transform: translateY(0); }
          50% { opacity: 1; transform: translateY(-4px); }
        }
        .input-area {
          padding: 16px 0;
          border-top: 1px solid #30363d;
          flex-shrink: 0;
        }
        .input-wrapper {
          display: flex;
          gap: 8px;
        }
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
        .input-wrapper textarea:focus {
          outline: none;
          border-color: #1f6feb;
        }
        .input-wrapper textarea::placeholder {
          color: #484f58;
        }
        .send-btn {
          background: #238636;
          color: white;
          border: none;
          border-radius: 8px;
          padding: 10px 20px;
          font-size: 14px;
          font-weight: 500;
          cursor: pointer;
          transition: background 0.15s;
          align-self: flex-end;
        }
        .send-btn:hover:not(:disabled) {
          background: #2ea043;
        }
        .send-btn:disabled {
          opacity: 0.5;
          cursor: not-allowed;
        }
        .error-toast {
          position: fixed;
          bottom: 20px;
          right: 20px;
          background: #da3633;
          color: white;
          padding: 12px 20px;
          border-radius: 8px;
          font-size: 13px;
          transform: translateY(100px);
          opacity: 0;
          transition: transform 0.3s, opacity 0.3s;
        }
        .error-toast.show {
          transform: translateY(0);
          opacity: 1;
        }
        .welcome-message {
          text-align: center;
          color: #8b949e;
          padding: 40px 20px;
          font-size: 14px;
        }
        @media (max-width: 768px) {
          .message-content {
            max-width: 85%;
          }
        }
      </style>
    </head>
    <body>
      <header>
        <div class="header-content">
          <div class="header-left">
            <a href="/agent-hub" class="back-btn">← Back</a>
            <h1>Chat with #{agent_name}</h1>
            <span class="agent-status">
              <span class="status-dot" id="status-dot"></span>
              <span id="status-text">Checking...</span>
            </span>
          </div>
          <div class="header-right">
            <button class="btn btn-reset" onclick="resetChat()">Reset</button>
            <button class="btn btn-stop" onclick="stopAgent()">Stop</button>
          </div>
        </div>
      </header>

      <div class="chat-container">
        <div class="messages" id="messages">
          <div class="welcome-message">
            Send a message to start chatting with #{agent_name}
          </div>
        </div>

        <div class="input-area">
          <div class="input-wrapper">
            <textarea
              id="message-input"
              placeholder="Type your message..."
              rows="1"
              onkeydown="handleKeyDown(event)"
              oninput="autoResize(this)"
            ></textarea>
            <button class="send-btn" id="send-btn" onclick="sendMessage()">Send</button>
          </div>
        </div>
      </div>

      <div class="error-toast" id="error-toast"></div>

      <script>
        var agentName = '#{agent_name}';
        var isStreaming = false;
        var currentAssistantMessage = null;

        // Check agent status on load
        checkAgentStatus();

        function showError(message) {
          var toast = document.getElementById('error-toast');
          toast.textContent = message;
          toast.className = 'error-toast show';
          setTimeout(function() {
            toast.className = 'error-toast';
          }, 5000);
        }

        async function checkAgentStatus() {
          try {
            var response = await fetch('/agent-hub/agents/' + encodeURIComponent(agentName) + '/status');
            var status = await response.json();

            var dot = document.getElementById('status-dot');
            var text = document.getElementById('status-text');

            if (status.status === 'not_running') {
              dot.className = 'status-dot stopped';
              text.textContent = 'Stopped';
            } else {
              dot.className = 'status-dot running';
              text.textContent = 'Running';
            }
          } catch (err) {
            console.error('Failed to check status:', err);
          }
        }

        function addMessage(role, content) {
          var messagesDiv = document.getElementById('messages');
          var welcome = messagesDiv.querySelector('.welcome-message');
          if (welcome) {
            welcome.remove();
          }

          var messageDiv = document.createElement('div');
          messageDiv.className = 'message ' + role;

          var avatar = document.createElement('div');
          avatar.className = 'message-avatar';
          avatar.textContent = role === 'user' ? 'U' : 'A';

          var contentDiv = document.createElement('div');
          contentDiv.className = 'message-content';
          contentDiv.innerHTML = formatContent(content);

          messageDiv.appendChild(avatar);
          messageDiv.appendChild(contentDiv);
          messagesDiv.appendChild(messageDiv);
          messagesDiv.scrollTop = messagesDiv.scrollHeight;

          return contentDiv;
        }

        function addTypingIndicator() {
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
          messagesDiv.scrollTop = messagesDiv.scrollHeight;
        }

        function removeTypingIndicator() {
          var indicator = document.getElementById('typing-indicator');
          if (indicator) {
            indicator.remove();
          }
        }

        function formatContent(text) {
          // Simple markdown-like formatting
          text = text.replace(/\\`\\`\\`([\\s\\S]*?)\\`\\`\\`/g, '<pre><code>$1</code></pre>');
          text = text.replace(/\\`([^\\`]+)\\`/g, '<code>$1</code>');
          text = text.replace(/\\n/g, '<br>');
          return text;
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

        async function sendMessage() {
          var input = document.getElementById('message-input');
          var message = input.value.trim();

          if (!message || isStreaming) return;

          // Add user message
          addMessage('user', message);
          input.value = '';
          input.style.height = 'auto';

          // Show typing indicator
          isStreaming = true;
          document.getElementById('send-btn').disabled = true;
          addTypingIndicator();

          try {
            var response = await fetch('/agent-hub/agents/' + encodeURIComponent(agentName) + '/chat', {
              method: 'POST',
              headers: {
                'Content-Type': 'application/json'
              },
              body: JSON.stringify({ message: message })
            });

            if (!response.ok) {
              throw new Error('HTTP ' + response.status);
            }

            // Remove typing indicator and prepare assistant message
            removeTypingIndicator();
            currentAssistantMessage = addMessage('assistant', '');

            // Read SSE stream
            var reader = response.body.getReader();
            var decoder = new TextDecoder();
            var buffer = '';

            while (true) {
              var result = await reader.read();
              if (result.done) break;

              buffer += decoder.decode(result.value, { stream: true });
              var lines = buffer.split('\\n');
              buffer = lines.pop() || '';

              for (var i = 0; i < lines.length; i++) {
                var line = lines[i].trim();
                if (line.startsWith('data: ')) {
                  var data = line.slice(6);
                  if (data === '[DONE]') {
                    continue;
                  }

                  try {
                    var event = JSON.parse(data);
                    handleStreamEvent(event);
                  } catch (e) {
                    console.error('Failed to parse event:', e);
                  }
                }
              }
            }
          } catch (err) {
            removeTypingIndicator();
            showError('Chat failed: ' + err.message);
            console.error('Chat error:', err);
          } finally {
            isStreaming = false;
            document.getElementById('send-btn').disabled = false;
            currentAssistantMessage = null;
            checkAgentStatus();
          }
        }

        function handleStreamEvent(event) {
          if (!currentAssistantMessage) return;

          switch (event.type) {
            case 'delta':
              if (event.text) {
                currentAssistantMessage.innerHTML += escapeHtml(event.text);
                scrollToBottom();
              }
              break;

            case 'tool_call':
              var toolDiv = document.createElement('div');
              toolDiv.className = 'tool-call';
              toolDiv.innerHTML = '🔧 Calling <span class="tool-name">' + escapeHtml(event.tool) + '</span>...';
              currentAssistantMessage.appendChild(toolDiv);
              scrollToBottom();
              break;

            case 'tool_result':
              // Could show tool result if needed
              break;

            case 'complete':
              // Final message already accumulated
              break;

            case 'error':
              showError('Agent error: ' + event.error);
              break;
          }
        }

        function scrollToBottom() {
          var messages = document.getElementById('messages');
          messages.scrollTop = messages.scrollHeight;
        }

        function escapeHtml(text) {
          var div = document.createElement('div');
          div.appendChild(document.createTextNode(text));
          return div.innerHTML;
        }

        async function resetChat() {
          if (!confirm('Reset conversation with ' + agentName + '?')) return;

          try {
            await fetch('/agent-hub/agents/' + encodeURIComponent(agentName) + '/reset', {
              method: 'POST'
            });

            // Clear messages
            document.getElementById('messages').innerHTML =
              '<div class="welcome-message">Conversation reset. Send a message to start chatting.</div>';
          } catch (err) {
            showError('Failed to reset: ' + err.message);
          }
        }

        async function stopAgent() {
          if (!confirm('Stop agent ' + agentName + '?')) return;

          try {
            await fetch('/agent-hub/agents/' + encodeURIComponent(agentName) + '/stop', {
              method: 'POST'
            });
            checkAgentStatus();
          } catch (err) {
            showError('Failed to stop agent: ' + err.message);
          }
        }

        // Auto-resize textarea on load
        var textarea = document.getElementById('message-input');
        autoResize(textarea);
      </script>
    </body>
    </html>
    """
  end
end
