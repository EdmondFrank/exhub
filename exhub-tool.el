;;; exhub-tool.el --- MCP Tools integration for Exhub

;;; Commentary:

;; This package provides integration with MCP Tools using Exhub.

;;; Code:

(require 'exhub)
(require 'exhub-chat)

(defvar exhub-mcp-server-all-name "mcp-server-all" "All MCP servers")
(defvar exhub-mcp-server-git-name "mcp-server-git" "Git MCP server name.")
(defvar exhub-mcp-server-file-name "mcp-server-file" "File MCP server name.")
(defvar exhub-mcp-server-github-name "mcp-server-github" "Github MCP server name.")
(defvar exhub-mcp-server-gitee-name "mcp-server-gitee" "Gitee MCP server name.")
(defvar exhub-mcp-server-gitee-ent-name "mcp-server-gitee-ent" "Gitee Ent MCP server name.")
(defvar exhub-mcp-server-k8s-name "mcp-server-k8s" "Kubernetes MCP server name.")

(defcustom exhubt-mcp-default-allowed-dir (expand-file-name "~") "Default allowed dir for Tools access." :type 'string :group 'exhub-tool)

(defun exhub-start-git-mcp-server ()
  "Start the Git MCP server"
  (interactive)
  (exhub-tool-call "start-server" "message" exhub-mcp-server-git-name "python" "-m" "mcp_server_git"))

(defun exhub-start-gitee-mcp-server ()
  "Start the Gitee MCP server"
  (interactive)
  (let ((gitee-token (read-passwd "Please enter your Gitee personal access token: ")))
    (if (string-empty-p (string-trim gitee-token))
        (message "Please enter a valid Gitee personal access token.")
      (setenv "GITEE_ACCESS_TOKEN" gitee-token)
      (message "Starting the Gitee MCP server...")
      (exhub-tool-call "start-server" "message" exhub-mcp-server-gitee-name "mcp-gitee"))))

(defun exhub-start-gitee-mcp-ent-server ()
  "Start the Gitee MCP server"
  (interactive)
  (let ((gitee-token (read-passwd "Please enter your Gitee enterprise access token: ")))
    (if (string-empty-p (string-trim gitee-token))
        (message "Please enter a valid Gitee personal access token.")
      (setenv "GITEE_MCP_ENT_ACCESS_TOKEN" gitee-token)
      (message "Starting the Gitee MCP Ent server...")
      (exhub-tool-call "start-server" "message" exhub-mcp-server-gitee-ent-name "mcp-gitee-ent"))))

(defun exhub-stop-gitee-mcp-ent-server ()
  "Stop the Git MCP server"
  (interactive)
  (exhub-tool-call "stop-server" "message" exhub-mcp-server-gitee-ent-name))

(defun exhub-start-file-mcp-server ()
  "Start the File MCP server"
  (interactive)
  (exhub-tool-call "start-server" "message" exhub-mcp-server-file-name "npx" "-y" "@modelcontextprotocol/server-filesystem" exhubt-mcp-default-allowed-dir))

(defun exhub-start-github-mcp-server ()
  "Start the GitHub MCP server"
  (interactive)
  (let ((github-token (read-passwd "Please enter your GitHub personal access token: ")))
    (if (string-empty-p (string-trim github-token))
        (message "Please enter a valid GitHub personal access token.")
      (setenv "GITHUB_PERSONAL_ACCESS_TOKEN" github-token)
      (message "Starting the GitHub MCP server...")
      (exhub-tool-call "start-server" "message" exhub-mcp-server-github-name "npx" "-y" "@modelcontextprotocol/server-github"))))

(defun exhub-start-k8s-mcp-server ()
  "Start the Kubernetes MCP server"
  (interactive)
  (exhub-tool-call "start-server" "message" exhub-mcp-server-k8s-name "npx" "-y" "mcp-server-kubernetes"))

(defun exhub-stop-git-mcp-server ()
  "Stop the Git MCP server"
  (interactive)
  (exhub-tool-call "stop-server" "message" exhub-mcp-server-git-name))

(defun exhub-stop-gitee-mcp-server ()
  "Stop the Gitee MCP server"
  (interactive)
  (exhub-tool-call "stop-server" "message" exhub-mcp-server-gitee-name))

(defun exhub-stop-file-mcp-server ()
  "Stop the File MCP server"
  (interactive)
  (exhub-tool-call "stop-server" "message" exhub-mcp-server-file-name))

(defun exhub-stop-github-mcp-server ()
  "Stop the GitHub MCP server"
  (interactive)
  (exhub-tool-call "stop-server" "message" exhub-mcp-server-github-name))

(defun exhub-stop-k8s-mcp-server ()
  "Stop the Kubernetes MCP server"
  (interactive)
  (exhub-tool-call "stop-server" "message" exhub-mcp-server-k8s-name))

(defun exhub-stop-all-mcp-servers ()
  "Stop all MCP servers"
  (interactive)
  (exhub-stop-git-mcp-server)
  (exhub-stop-gitee-mcp-server)
  (exhub-stop-file-mcp-server)
  (exhub-stop-github-mcp-server)
  (exhub-stop-k8s-mcp-server))

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
