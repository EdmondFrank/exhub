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
    ;; Start the Elixir application process in the specified directory
    (message exhub-backend-path)
    (setq exhub--elixir-process
          (start-process "exhub" "*exhub*" exhub-backend-path "start"))))

(defun exhub-restart-elixir ()
  "Restart the Elixir application."
  (interactive)
  (when exhub--elixir-process
    (when (process-live-p exhub--elixir-process)
      (kill-process exhub--elixir-process)
      (setq exhub--elixir-process nil)))
  (exhub-start-elixir))

(defun exhub-restart ()
  "Restart the websocket connection and the Elixir application."
  (interactive)
  (exhub-restart-elixir)
  (exhub-restart-websocket))

(provide 'exhub)
;;; exhub.el ends here
