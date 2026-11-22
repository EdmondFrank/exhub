;;; exhub-agent.el --- Agent Hub integration for Exhub  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  EdmondFrank

;; Author: EdmondFrank <edmondfrank@hotmail.com>
;; Keywords: Extensions
;; Version: 0.2.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Emacs client for the ExHub Agent Hub.  Every registered Sagents agent
;; profile ("assistant", "coder", "researcher", ...) owns one conversation
;; persisted in SQLite, so a thread survives Emacs restarts and is shared with
;; the browser UI at `http://localhost:9069/agent-hub'.
;;
;; The server side is `Exhub.ResponseHandlers.ExhubAgent'.

;;; Code:

(require 'exhub)
(require 'exhub-chat)

(defvar exhub-agent-version "0.2.0"
  "Current version of exhub-agent.")

(defvar exhub-agent-json-object-type 'hash-table
  "The JSON object type for Exhub agents.")

(defvar exhub-agent-json-array-type 'list
  "The JSON array type for Exhub agents.")

(defvar exhub-agent-json-key-type 'string
  "The JSON key type for Exhub agents.")

(defvar exhub-agent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c") 'exhub-agent-chat)
    (define-key map (kbd "C-c r") 'exhub-agent-tool-reply)
    (define-key map (kbd "C-c s") 'exhub-agent-start)
    (define-key map (kbd "C-c k") 'exhub-agent-kill)
    map))

(define-derived-mode exhub-agent-mode markdown-mode "exhub-agent"
  "The major mode for Exhub Agent.")

(add-to-list 'auto-mode-alist '("\\.chat$" . exhub-agent-mode))

;;; Agent Hub protocol helpers

(defun exhub-agent-call (action callback &rest args)
  "Call the Exhub Agent Hub ACTION, then CALLBACK with ARGS appended."
  (exhub-call "exhub-agent" action callback args))

(defun exhub--agent-names (response)
  "Return the list of registered agent names parsed from RESPONSE."
  (let ((json-object-type exhub-agent-json-object-type)
        (json-array-type exhub-agent-json-array-type)
        (json-key-type exhub-agent-json-key-type))
    (json-read-from-string response)))

(defun exhub--select-agent (prompt response)
  "Prompt for an agent with PROMPT, choosing from RESPONSE's names."
  (let* ((names (exhub--agent-names response))
         (choices (mapcar (lambda (name) (cons name name)) names)))
    (if (null choices)
        (progn
          (message "[Exhub-Agent] No registered agents.")
          nil)
      (completing-read prompt choices nil t))))

(defun exhub--agent-send (agent-name prompt)
  "Send PROMPT to the Agent Hub agent AGENT-NAME and render its reply."
  (let ((buffer (exhub-chat--get-chat-buffer)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (insert "## User:\n")
        (insert (format "%s\n" prompt)))
      (message "[Exhub-Chat] Waiting for agent '%s' ..." agent-name)
      (exhub-agent-call
       "chat-with-agent"
       (buffer-file-name)
       agent-name
       prompt
       (or exhub-chat-buffer-name
           (buffer-name))))))

(defun exhub--agent-notice (response)
  "Display the server RESPONSE in the minibuffer."
  (message "%s" response))

;;; Commands

;;;###autoload
(defun exhub-agent-chat ()
  "Chat with one of the registered Agent Hub agents."
  (interactive)
  (exhub-agent-call "list-agents" 'exhub--agent-chat-select))

(defun exhub--agent-chat-select (response)
  "Prompt for an agent from RESPONSE, then chat with it."
  (when-let ((agent-name (exhub--select-agent "Chat with an agent: " response)))
    (let ((prompt (read-string "Chat with Exhub Agent: ")))
      (if (string-empty-p (string-trim prompt))
          (message "Please do not enter an empty prompt.")
        (exhub--agent-send agent-name prompt)))))

;;;###autoload
(defun exhub-agent-tool-reply ()
  "Reply to an agent with the output of a shell command as the prompt."
  (interactive)
  (exhub-agent-call "list-agents" 'exhub--agent-tool-reply-select))

(defun exhub--agent-tool-reply-select (response)
  "Prompt for an agent from RESPONSE and send it a shell command's output."
  (when-let ((agent-name (exhub--select-agent "Reply to an agent: " response)))
    (let ((prompt (shell-command-to-string (read-string "Enter shell command: "))))
      (if (string-empty-p (string-trim prompt))
          (message "Shell command output is empty.")
        (exhub--agent-send agent-name prompt)))))

;;;###autoload
(defun exhub-agent-start ()
  "Start the agent of one of the registered profiles."
  (interactive)
  (exhub-agent-call "list-agents" 'exhub--agent-start-select))

(defun exhub--agent-start-select (response)
  "Prompt for an agent from RESPONSE and start it."
  (when-let ((agent-name (exhub--select-agent "Start an agent: " response)))
    (exhub-agent-call "start-agent" 'exhub--agent-notice agent-name)))

;;;###autoload
(defun exhub-agent-kill ()
  "Stop every running conversation of a registered agent."
  (interactive)
  (exhub-agent-call "list-agents" 'exhub--agent-kill-select))

(defun exhub--agent-kill-select (response)
  "Prompt for an agent from RESPONSE and stop it."
  (when-let ((agent-name (exhub--select-agent "Stop an agent: " response)))
    (exhub-agent-call "kill-agent" 'exhub--agent-notice agent-name)))

(provide 'exhub-agent)

;;; exhub-agent.el ends here
