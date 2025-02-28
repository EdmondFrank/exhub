;;; exhub-tool.el --- MCP Tools integration for Exhub

;;; Commentary:

;; This package provides integration with MCP Tools using Exhub.

;;; Code:

(require 'exhub)
(require 'exhub-chat)

(defvar exhub-mcp-server-git-name "mcp-server-git"
  "Git MCP server name.")

(defun exhub-start-git-mcp-server ()
  "Start the Git MCP server"
  (interactive)
  (exhub-tool-call "start-server" "message" exhub-mcp-server-git-name "python" "-m" "mcp_server_git"))

(defun exhub-chat-with-git ()
  "Chat with Exhub using a registered Git server."
  (interactive)
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
           exhub-mcp-server-git-name
           "You are a software developer, can use git tools to help user to finish tasks"
           prompt
           (or exhub-chat-buffer-name
               (buffer-name))))))))



(defun exhub-tool-call (action callback &rest args)
  "Call a Tools Server or Client API function using Exhub."
  (exhub-call "exhub-tool" action callback args))

;;;

(provide 'exhub-tool)

;;; exhub-tool.el ends here
