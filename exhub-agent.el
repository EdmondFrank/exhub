;;; exhub-agent.el --- Agents integration for Exhub  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  EdmondFrank

;; Author: EdmondFrank Edmondfrank@hotmail.com
;; Keywords: Extensions
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides agents integration for Exhub.

;;; Code:

(require 'exhub)
(require 'exhub-chat)

(defvar exhub-agent-version "0.1.0"
  "Current version of exhub-agent.")

(defun exhub-chat-with-agent ()
  "Chat with Existing or New Agents in Exhub World."
  (interactive)
  (exhub-agent-call "list-agents" "exhub--chat-select-or-create-agent"))

(defun exhub-chat-kill-agent ()
  "Kill an Existing agent in Exhub World."
  (interactive)
  (exhub-agent-call "list-agents" "exhub--chat-kill-agent"))

(defun exhub--chat-select-or-create-agent (response)
  "Chat with Exhub by using an existing agent or creating a new one.
RESPONSE is the response from the server, containing the list of agents."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (agent-names (json-read-from-string response))
         (system-message "You are a helpful assistant")
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
           system-message
           prompt
           (or exhub-chat-buffer-name
               (buffer-name))))))))

(defun exhub--chat-kill-agent (response)
  "Kill an agent in Exhub World.
RESPONSE is the response from the server, containing the list of agents."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (agent-names (json-read-from-string response))
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
