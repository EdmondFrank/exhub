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

(require 'websocket)

(defvar exhub-endpoint "ws://localhost:9069/exhub"
  "Endpoint of exhub elixir application.")

(defvar exhub--websocket nil
  "Websocket connection to Exhub.")

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
                       (exhub-eval body)))
                   :on-close (lambda (_websocket) (message "websocket closed"))))
            (setq success t))
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
    (websocket-close exhub--websocket))
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

(defun exhub--pong ()
  "Send a pong message to the websocket."
  (exhub-send "exhub-pong"))

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
      (kill-process exhub--elixir-process)))
  (exhub-start-elixir))

(provide 'exhub)
;;; exhub.el ends here
