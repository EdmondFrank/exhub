;;; exhub-agent.el --- Agents integration for Exhub  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  EdmondFrank

;; Author: EdmondFrank <edmondfrank@hotmail.com>
;; Keywords: Extensions
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides integration for interacting with agents in the Exhub
;; environment. It allows users to chat with existing or new agents, reply to
;; agents using the output of shell commands, initialize tools with agents, and
;; kill agents.

;;; Code:

(require 'exhub)
(require 'exhub-chat)

(defvar exhub-agent-version "0.1.0"
  "Current version of exhub-agent.")

(defvar exhub-agent-system-message "You are a helpful assistant"
  "The default system message for Exhub agents.")

(defvar exhub-agent-json-object-type 'hash-table
  "The JSON object type for Exhub agents.")

(defvar exhub-agent-json-array-type 'list
  "The JSON array type for Exhub agents.")

(defvar exhub-agent-json-key-type 'string
  "The JSON key type for Exhub agents.")

(defun exhub-agent-chat ()
  "Chat with existing or new agents in the Exhub world."
  (interactive)
  (exhub-agent-call "list-agents" "exhub--chat-select-or-create-agent"))

(defun exhub-agent-tool-reply ()
  "Reply to an existing agent using the output of a shell command as the prompt."
  (interactive)
  (exhub-agent-call "list-agents" "exhub--tool-reply-an-agent"))

(defun exhub-agent-init-tools ()
  "Initialize tools with existing or new agents in the Exhub world."
  (interactive)
  (exhub-agent-call "list-agents" "exhub--init-tools-select-or-create-agent"))

(defun exhub-agent-kill ()
  "Kill an existing agent in the Exhub world."
  (interactive)
  (exhub-agent-call "list-agents" "exhub--chat-kill-agent"))

(defun exhub--init-tools-select-or-create-agent (response)
  "Initialize tools with Exhub by using an existing agent or creating a new one.
RESPONSE is the response from the server, containing the list of agents."
  (let* ((agent-names (json-read-from-string response))
         (choices (mapcar (lambda (name) (cons name name)) agent-names))
         (agent-name (completing-read "Select an agent to initialize tools with: " choices nil t))
         (buffer-file (buffer-file-name))
         (prompt (shell-command-to-string "mcpm-aider toolprompt")))
    (if (string-empty-p (string-trim prompt))
        (message "No tools are available.")
      (let ((buffer (exhub-chat--get-chat-buffer)))
        (with-current-buffer buffer
          (exhub-agent-call
           "chat-with-agent"
           buffer-file
           agent-name
           exhub-agent-system-message
           prompt
           (or exhub-chat-buffer-name
               (buffer-name))))))))

(defun exhub--chat-select-or-create-agent (response)
  "Chat with Exhub by using an existing agent or creating a new one.
RESPONSE is the response from the server, containing the list of agents."
  (let* ((agent-names (json-read-from-string response))
         (choices (mapcar (lambda (name) (cons name name)) agent-names))
         (agent-name (completing-read "Select an agent to chat to: " choices nil t))
         (buffer-file (buffer-file-name))
         (prompt (read-string "Chat with Exhub Agent: ")))
    (if (string-empty-p (string-trim prompt))
        (message "Please do not enter an empty prompt.")
      (let ((buffer (exhub-chat--get-chat-buffer)))
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-max))
            (insert "## User:\n")
            (insert (format "%s\n" prompt)))
          (message "[Exhub-Chat] Please wait for Exhub ...")
          (exhub-agent-call
           "chat-with-agent"
           buffer-file
           agent-name
           exhub-agent-system-message
           prompt
           (or exhub-chat-buffer-name
               (buffer-name))))))))

(defun exhub--tool-reply-an-agent (response)
  "Reply to an existing agent using the output of a shell command as the prompt.
RESPONSE is the response from the server, containing the list of agents."
  (let* ((agent-names (json-read-from-string response))
         (choices (mapcar (lambda (name) (cons name name)) agent-names))
         (agent-name (completing-read "Select an agent to reply to: " choices nil t))
         (shell-command (read-string "Enter shell command: "))
         (prompt (shell-command-to-string shell-command)))
    (if (string-empty-p (string-trim prompt))
        (message "Shell command output is empty.")
      (let ((buffer (exhub-chat--get-chat-buffer)))
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-max))
            (insert "## User:\n")
            (insert (format "%s\n" prompt)))
          (message "[Exhub-Chat] Please wait for Exhub ...")
          (exhub-agent-call
           "chat-with-agent"
           (buffer-file-name)
           agent-name
           exhub-agent-system-message
           prompt
           (or exhub-chat-buffer-name
               (buffer-name))))))))

(defun exhub--chat-kill-agent (response)
  "Kill an agent in the Exhub world.
RESPONSE is the response from the server, containing the list of agents."
  (let* ((agent-names (json-read-from-string response))
         (choices (mapcar (lambda (name) (cons name name)) agent-names))
         (agent-name (completing-read "Select an agent to kill: " choices nil t)))
    (if (string-empty-p (string-trim agent-name))
        (message "Please select a valid agent to kill.")
      (exhub-agent-call "kill-agent" "exhub--chat-kill-agent-callback" agent-name))))

(defun exhub-agent-call (action callback &rest args)
  "Call an Agent Server or Client API function using Exhub.
ACTION is the action name.
CALLBACK is the function to call with the response.
ARGS are the arguments to pass to the action."
  (exhub-call "exhub-agent" action callback args))

(defun exhub--chat-kill-agent-callback (response)
  "Callback function to handle the response after killing an agent.
RESPONSE is the response from the server."
  (message "Agent killed successfully: %s" response))

(provide 'exhub-agent)

;;; exhub-agent.el ends here
