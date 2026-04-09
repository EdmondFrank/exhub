;;; exhub.el --- Exhub (Elixir Plugin for Emacs)  -*- lexical-binding:t; -*-

;; Copyright (C) 2023 EdmondFrank

;; Author: Edmond Frank <EdmondFrank>
;; Version: 0.1
;; Package-Requires: ((websocket "1.10"))
;; Keywords: websocket elixir
;; URL: https://github.com/edmondfrank/exhub.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a client for connecting to the Exhub Elixir WebSocket server.

;;; Code:

(require 'json)
(require 'websocket)


(defvar exhub-endpoint "ws://localhost:9069/exhub"
  "Endpoint of exhub elixir application.")

(defvar exhub--websocket nil
  "Websocket connection to Exhub.")

(defvar exhub--ping-timer nil
  "Timer for sending ping messages to the WebSocket.")

(defvar exhub--elixir-process nil
  "Elixir process.")

(defcustom exhub-backend-path (expand-file-name "_build/prod/rel/exhub/bin/exhub"
                                                (if load-file-name
                                                    (file-name-directory load-file-name)
                                                  default-directory))
  "The Exhub backend used to run exhub."
  :type 'string)

(defcustom exhub-mix-env "prod"
  "The MIX_ENV environment variable for the Elixir application."
  :type 'string)

(defcustom exhub-secret-vault-password nil
  "The SECRET_VAULT_PASSWORD environment variable for SecretVault.
If nil or empty, the environment variable will not be set."
  :type '(choice (const :tag "Not set" nil) string))

(defun exhub-start ()
  "Start webserver connection to Exhub (Elixir WS server)."
  (interactive)
  (let ((retry-count 0)
        (max-retries 3)
        (success nil))
    (while (and (< retry-count max-retries) (not success))
      (condition-case err
          (progn
            (setq exhub--websocket
                  (websocket-open
                   exhub-endpoint
                   :on-message
                   (lambda (_websocket frame)
                     (let ((body (websocket-frame-text frame)))
                       (condition-case err
                           (exhub-eval body)
                         (error
                          (message "Error evaluating WebSocket message: %s" err)))))
                   :on-close (lambda (_websocket)
                               (message "websocket closed")
                               (exhub--stop-ping-timer))))
            (setq success t)
            (exhub--start-ping-timer))  ; Start the ping timer
        (error
         (message "Attempt %d to connect to Exhub failed: %s" (1+ retry-count) err)
         (sleep-for (+ 1 (random 2)))  ; Sleep for 1 to 2 seconds
         (setq retry-count (1+ retry-count)))))
    (unless success
      (message "Failed to connect to Exhub after %d attempts" max-retries))))

(defun exhub-restart-websocket ()
  "Restart the websocket connection."
  (interactive)
  (when (exhub-open-connection)
    (websocket-close exhub--websocket)
    (exhub--stop-ping-timer))  ; Stop the ping timer
  (exhub-start))

(defun exhub-send (payload)
  "Send a PAYLOAD message to the websocket."
  (interactive "sMessage: ")
  (unless (exhub-open-connection)
    (exhub-start))
  (websocket-send-text exhub--websocket payload))

(defun exhub-call (&rest func-args)
  "Call Exhub function from Emacs."
  (if (exhub-open-connection)
      (websocket-send-text exhub--websocket
                           (json-encode (list "func" func-args)))
    (message "[Exhub] Application has exited.")))

(defun exhub-open-connection ()
  "Check if the websocket connection is open."
  (websocket-openp exhub--websocket))

(defun exhub-eval (string)
  "Evaluate elisp code stored in STRING."
  (eval (car (read-from-string string))))

(defun exhub-pong ()
  "A no-operation function that does nothing."
  nil)

(defun exhub--pong ()
  "Send a pong message to the websocket."
  (exhub-send "exhub-pong"))

(defun exhub--ping ()
  "Send a pong message to the websocket."
  (exhub-send "exhub-ping"))

(defun exhub--start-ping-timer ()
  "Start the ping timer to send ping messages to the WebSocket."
  (when (exhub-open-connection)
    (setq exhub--ping-timer
          (run-with-timer 30 30 'exhub--ping))))

(defun exhub--stop-ping-timer ()
  "Stop the ping timer."
  (when exhub--ping-timer
    (cancel-timer exhub--ping-timer)
    (setq exhub--ping-timer nil)))

(defun exhub--switch-model-callback (response)
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (llm-names (json-read-from-string response))
         (choices (mapcar (lambda (name) (cons name name)) llm-names)))
    (let ((selected-llm (completing-read "Select a model to switch to: " choices)))
      (exhub-call "exhub-config" "set-model" selected-llm))))

(defun exhub--display-current-model-callback (model-name)
  "Display the current model name in the minibuffer and copy it to the clipboard."
  (message "Current Model: %s" model-name)
  (kill-new model-name))

(defun exhub-switch-model ()
  "Switch the model by calling the Exhub configuration to list available models and prompt the user to select one."
  (interactive)

  ;; Call the Exhub configuration to list available models and handle the callback to prompt the user for selection
  (exhub-call "exhub-config" "switch-model" "exhub--switch-model-callback"))

(defun exhub-model ()
  (interactive)
  ;; Call the Exhub configuration to get the current model name
  (exhub-call "exhub-config" "current-model" "exhub--display-current-model-callback"))

(defun exhub-start-elixir ()
  "Start the Elixir application."
  (interactive)
  ;; Check if the Elixir process is already running
  (unless (and exhub--elixir-process (process-live-p exhub--elixir-process))
    (message "Starting Exhub backend with MIX_ENV=%s" exhub-mix-env)
    ;; Bind process-environment locally so env vars only affect this process
    (let ((process-environment
           (append
            (list (format "MIX_ENV=%s" exhub-mix-env))
            (when (and exhub-secret-vault-password
                       (not (string-empty-p exhub-secret-vault-password)))
              (list (format "SECRET_VAULT_PASSWORD=%s" exhub-secret-vault-password)))
            process-environment)))
      (setq exhub--elixir-process
            (start-process "exhub" "*exhub*" exhub-backend-path "start")))))

(defun exhub-restart-elixir ()
  "Restart the Elixir application."
  (interactive)
  (when exhub--elixir-process
    (when (process-live-p exhub--elixir-process)
      (kill-process exhub--elixir-process)
      (setq exhub--elixir-process nil)))
  (exhub-start-elixir))

(defun exhub-reload ()
  "Hot-reload all BEAM modules in the running Exhub server.

Uses two strategies:
1. RPC via the Exhub backend binary (preferred) — calls into the
   running BEAM VM and hot-swaps all modules without restarting
   the server or interrupting in-flight HTTP requests.
2. WebSocket fallback — sends a reload message over the WebSocket
   connection when the RPC binary is unavailable."
  (interactive)
  (message "[Exhub] Triggering hot reload via RPC...")
  (if (file-executable-p exhub-backend-path)
      (let ((buffer (get-buffer-create "*exhub-reload*")))
        (with-current-buffer buffer
          (erase-buffer))
        (let ((proc (start-process "exhub-reload" buffer exhub-backend-path "rpc" "Exhub.HotReload.reload_and_summarize()")))
          (set-process-sentinel proc
                                (lambda (proc _event)
                                  (when (memq (process-status proc) '(exit signal))
                                    (with-current-buffer (process-buffer proc)
                                      (message "%s" (buffer-string))))))))
    (message "[Exhub] Backend not executable, falling back to WebSocket reload")
    (exhub-send "exhub-reload")))

(provide 'exhub)
;;; exhub.el ends here
