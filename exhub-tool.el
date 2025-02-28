;;; exhub-tool.el --- MCP Tools integration for Exhub

;;; Commentary:

;; This package provides integration with MCP Tools using Exhub.

;;; Code:

(require 'exhub)
(require 'exhub-chat)

(defvar exhub-mcp-server-all-name "mcp-server-all" "All MCP servers")
(defvar exhub-mcp-server-git-name "mcp-server-git" "Git MCP server name.")
(defvar exhub-mcp-server-file-name "mcp-server-file" "File MCP server name.")
(defcustom exhubt-mcp-default-allowed-dir (expand-file-name "~") "Default allowed dir for Tools access." :type 'string :group 'exhub-tool)

(defun exhub-start-git-mcp-server ()
  "Start the Git MCP server"
  (interactive)
  (exhub-tool-call "start-server" "message" exhub-mcp-server-git-name "python" "-m" "mcp_server_git"))

(defun exhub-start-file-mcp-server ()
  "Start the File MCP server"
  (interactive)
  (exhub-tool-call "start-server" "message" exhub-mcp-server-file-name "npx" "-y" "@modelcontextprotocol/server-filesystem" exhubt-mcp-default-allowed-dir))

(defun exhub-chat-with-git ()
  "Chat with Exhub using a registered Git server."
  (interactive)
  (exhub-chat-with-server exhub-mcp-server-git-name "You are a software developer, can use git tools to help user to finish tasks"))

(defun exhub-chat-with-tools ()
  "Chat with Exhub using all tools."
  (interactive)
  (exhub-chat-with-server exhub-mcp-server-all-name "You are a software developer, can use tools to help user to finish tasks"))

(defun exhub-chat-with-server (server-name system-message)
  "Chat with Exhub using a specified server."
  (let* ((buffer-file (buffer-file-name))
         (prompt (read-string "Chat with Exhub: ")))
    (if (string-empty-p (string-trim prompt))
        (message "Please do not enter an empty prompt.")
      (let ((buffer (exhub-chat--get-chat-buffer)))
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-max))
            (insert "## User:\n")
            (insert (format "%s\n" prompt)))

          (message "[Exhub-Chat] Please wait for Exhub ...")
          (exhub-tool-call
           "chat-with-tool"
           buffer-file
           server-name
           system-message
           prompt
           (or exhub-chat-buffer-name
               (buffer-name))))))))

(defun exhub-tool-call (action callback &rest args)
  "Call a Tools Server or Client API function using Exhub."
  (exhub-call "exhub-tool" action callback args))

;;;

(provide 'exhub-tool)

;;; exhub-tool.el ends here
