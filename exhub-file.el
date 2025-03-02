;;; exhub-file.el --- Exhub File integration for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2023 EdmondFrank

;;; Commentary:

;; This package provides integration with Exhub File using Exhub.

;;; Code:

(require 'exhub)

(defgroup exhub-file nil
  "Exhub File group.
This group provides functions to interact with Exhub File API.
The functions in this group are used to perform various file operations
such as reading, writing, and deleting files using the Exhub File API."
  :group 'applications)

(defun exhub-preview-markdown ()
  "Preview the current buffer with markdown using Exhub"
  (interactive)
  (let ((content (buffer-substring-no-properties (point-min) (point-max)))
        (buffer-name (buffer-name)))
    (if (string-empty-p content)
        (message "The current buffer is empty. Nothing to preview.")
      (exhub-file-call "markdown-render" "exhub-file-show-with-eww-casual" content buffer-name))))

(defun exhub-file-call (action callback &rest args)
  "Call a API function using Exhub File.
ACTION is the action to perform.
CALLBACK is the callback function to call when the action is complete.
ARGS are the arguments to pass to the action.
This function is used to interact with the Exhub File API to perform
various file operations such as reading, writing, and deleting files."
  (exhub-call "exhub-file" action callback args))

(defun exhub-file-show-with-eww-casual (html-content buffer-name)
  "Display HTML content in EWW buffer.
HTML-CONTENT is the HTML content to be displayed.
BUFFER-NAME is the name of the buffer where the content will be displayed."
  (let ((eww-buffer (get-buffer (format "*eww-%s*" buffer-name))))
    (when eww-buffer
      (kill-buffer eww-buffer))
    (setq eww-buffer (get-buffer-create (format "*eww-%s*" buffer-name)))
    (with-current-buffer eww-buffer
      (erase-buffer)
      (insert html-content)
      (goto-char (point-min))
      (shr-render-region (point-min) (point-max))
      (eww-mode))
    (switch-to-buffer eww-buffer)))

;; (defun exhub-live-preview-markdown ()
;;   "Preview the current buffer with markdown using Exhub in real-time"
;;   (interactive)
;;   (let ((buffer-name (buffer-name)))
;;     (add-hook 'after-save-hook
;;               (lambda ()
;;                 (let ((buffer-name (buffer-name))
;;                       (content (buffer-substring-no-properties (point-min) (point-max))))
;;                   (if (string-empty-p content)
;;                       (message "The current buffer is empty. Nothing to preview.")
;;                     (exhub-file-call "markdown-render" "exhub-file-show-with-eww-casual" content buffer-name))))
;;               'local)))

(provide 'exhub-file)

;;; exhub-file.el ends here
