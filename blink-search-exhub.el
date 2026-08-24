;;; blink-search-exhub.el --- Blink Search via ExHub WebSocket  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 EdmondFrank

;; Author: Edmond Frank
;; Version: 0.1
;; Package-Requires: ((emacs "28") (websocket "1.10"))
;; Keywords: search convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:
;;
;; Elixir-powered blink-search using the ExHub WebSocket infrastructure.
;; Replaces the Python/EPC backend with Elixir/OTP for true concurrency.
;;
;; Requires: exhub.el (WebSocket connection to ExHub server)

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'recentf)
(require 'imenu)
(require 'pulse)

(recentf-mode 1)

;; ===========================================================================
;; Variables
;; ===========================================================================

(defvar blink-search-exhub-window-configuration nil)
(defvar blink-search-exhub-start-buffer nil)
(defvar blink-search-exhub-start-buffer-name nil)
(defvar blink-search-exhub-start-path-name nil)
(defvar blink-search-exhub-start-buffer-directory nil)
(defvar blink-search-exhub-continue-directory nil)
(defvar blink-search-exhub-start-keyword nil)

(defvar blink-search-exhub-input-buffer " *blink search exhub input*")
(defvar blink-search-exhub-tooltip-buffer " *blink search exhub tooltip*")
(defvar blink-search-exhub-candidate-buffer " *blink search exhub candidate*")
(defvar blink-search-exhub-backend-buffer " *blink search exhub backend*")

(defvar blink-search-exhub-candidate-items nil)
(defvar blink-search-exhub-candidate-select-index nil)
(defvar blink-search-exhub-backend-items nil)
(defvar blink-search-exhub-backend-select-index nil)
(defvar blink-search-exhub-backend-name nil)
(defvar blink-search-exhub-item-index nil)
(defvar blink-search-exhub-items-number nil)
(defvar blink-search-exhub-backend-number nil)

(defvar blink-search-exhub-idle-update-list nil)
(defvar blink-search-exhub-start-update-list nil)
(defvar blink-search-exhub-idle-timer nil)
(defvar blink-search-exhub-elisp-symbol-size 0)
(defvar blink-search-exhub-preview-window nil)
(defvar blink-search-exhub-focus-timer nil)

;; ===========================================================================
;; Customization
;; ===========================================================================

(defgroup blink-search-exhub nil
  "Blink Search via ExHub."
  :group 'applications)

(defcustom blink-search-exhub-search-backends nil
  "Default backends for blink search.  Nil means all default backends."
  :type '(repeat string)
  :group 'blink-search-exhub)

(defcustom blink-search-exhub-common-directory '(("HOME" "~/"))
  "Common directories to search and open.
Each entry is a list of (ALIAS DIRECTORY), pushed to the Elixir
Common Directory backend on start."
  :type '(repeat (list string string))
  :group 'blink-search-exhub)

(defcustom blink-search-exhub-grep-pdf-search-paths nil
  "Directories to search PDF files with rga.
Nil means search the current directory only."
  :type '(repeat string)
  :group 'blink-search-exhub)

(defcustom blink-search-exhub-elisp-symbol-update-idle 5
  "Idle seconds between elisp symbol synchronization to ExHub."
  :type 'number
  :group 'blink-search-exhub)

(defcustom blink-search-exhub-history-path
  (expand-file-name (concat user-emacs-directory "blink-search" "/history.txt"))
  "Path to store search history."
  :type 'string
  :group 'blink-search-exhub)

(defcustom blink-search-exhub-kv-db-path
  (expand-file-name (concat user-emacs-directory "blink-search-kv.db"))
  "Path to the SQLite database for the Key Value Store backend."
  :type 'string
  :group 'blink-search-exhub)

(defcustom blink-search-exhub-kv-db-table
  "blink_search_kv"
  "Table name for the Key Value Store backend."
  :type 'string
  :group 'blink-search-exhub)

(defcustom blink-search-exhub-flash-line-delay 0.3
  "Seconds to flash the current line after navigation."
  :type 'number
  :group 'blink-search-exhub)

;; ===========================================================================
;; Faces
;; ===========================================================================

(defface blink-search-exhub-select-face
  '()
  "Face for the currently selected candidate.")

(defface blink-search-exhub-flash-face
  '((t (:inherit highlight)))
  "Face to flash the current line."
  :group 'blink-search-exhub)

;; ===========================================================================
;; Quick Keys
;; ===========================================================================

(defvar blink-search-exhub-quick-keys
  '("h" "l" "u" "i" "y"
    "," "." ";" "/" "'"
    "r" "v" "g" "t" "c"
    "7" "8" "9" "0"
    "H" "L" "U" "I" "Y"
    "s" "a" "e" "q"
    "1" "2" "3" "4"
    "[" "]")
  "Quick access keys for candidates, prefixed with Alt.")

;; ===========================================================================
;; Keymap
;; ===========================================================================

(defvar blink-search-exhub-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") 'blink-search-exhub-quit)
    (define-key map (kbd "ESC ESC ESC") 'blink-search-exhub-quit)
    (define-key map (kbd "C-n") 'blink-search-exhub-candidate-select-next)
    (define-key map (kbd "C-p") 'blink-search-exhub-candidate-select-prev)
    (define-key map (kbd "M-n") 'blink-search-exhub-backend-select-next)
    (define-key map (kbd "M-p") 'blink-search-exhub-backend-select-prev)
    (define-key map (kbd "M-j") 'blink-search-exhub-candidate-group-select-next)
    (define-key map (kbd "M-k") 'blink-search-exhub-candidate-group-select-prev)
    (define-key map (kbd "C-m") 'blink-search-exhub-do)
    (define-key map (kbd "C-M-m") 'blink-search-exhub-preview)
    (define-key map (kbd "C-M-n") 'blink-search-exhub-preview-next)
    (define-key map (kbd "C-M-p") 'blink-search-exhub-preview-prev)
    (define-key map (kbd "C-j") 'blink-search-exhub-parent)
    (define-key map (kbd "C-l") 'blink-search-exhub-continue)
    (define-key map (kbd "M-w") 'blink-search-exhub-copy)
    (dolist (key blink-search-exhub-quick-keys)
      (define-key map (kbd (format "M-%s" key)) 'blink-search-exhub-quick-do))
    map)
  "Keymap for `blink-search-exhub-mode'.")

(define-derived-mode blink-search-exhub-mode text-mode "blink-search-exhub"
  (kill-all-local-variables)
  (setq major-mode 'blink-search-exhub-mode)
  (setq mode-name "blink-search-exhub")
  (use-local-map blink-search-exhub-mode-map)
  (when (featurep 'evil)
    (evil-set-initial-state 'blink-search-exhub-mode 'emacs)))

;; ===========================================================================
;; ExHub Communication
;; ===========================================================================

(defun blink-search-exhub-call-flat (&rest args)
  "Send a blink-search command with flat args to ExHub."
  (apply #'exhub-call "blink-search" args))

;; ===========================================================================
;; Colors
;; ===========================================================================

(defun blink-search-exhub-init-colors (&optional force)
  "Initialize colors for blink-search buffers."
  (with-current-buffer blink-search-exhub-input-buffer
    (face-remap-add-relative 'hl-line :background (face-background 'default)))

  (let* ((is-dark (string-equal (prin1-to-string (frame-parameter nil 'background-mode)) "dark"))
         (blend-bg (if is-dark "#000000" "#AAAAAA"))
         (default-bg (face-attribute 'default :background)))
    (unless (ignore-errors
              (blink-search-exhub-color-blend default-bg blend-bg 0.6))
      (setq default-bg (if is-dark "#000000" "#AAAAAA")))
    (when (or force (equal (face-attribute 'blink-search-exhub-select-face :background) 'unspecified))
      (set-face-background 'blink-search-exhub-select-face
                           (blink-search-exhub-color-blend default-bg blend-bg 0.6)))
    (when (or force (equal (face-attribute 'blink-search-exhub-select-face :foreground) 'unspecified))
      (set-face-foreground 'blink-search-exhub-select-face
                           (face-attribute 'font-lock-function-name-face :foreground)))))

(defun blink-search-exhub-color-blend (c1 c2 alpha)
  "Blend colors C1 and C2 with ALPHA."
  (apply (lambda (r g b)
           (format "#%02x%02x%02x" (ash r -8) (ash g -8) (ash b -8)))
         (cl-mapcar (lambda (x y) (round (+ (* x alpha) (* y (- 1 alpha)))))
                    (color-values c1) (color-values c2))))

;; ===========================================================================
;; Window Layout
;; ===========================================================================

(defun blink-search-exhub-get-window-allocation (&optional window)
  "Get WINDOW allocation as (x y w h)."
  (let* ((edges (window-pixel-edges window))
         (x (nth 0 edges))
         (y (+ (nth 1 edges) (window-tab-line-height window)))
         (w (- (nth 2 edges) x))
         (h (- (nth 3 edges) (window-mode-line-height window) y)))
    (list x y w h)))

(defun blink-search-exhub-get-row-number ()
  "Return the number of visible text rows in the candidate window."
  (let* ((win (get-buffer-window blink-search-exhub-candidate-buffer))
         (height (when win (nth 3 (blink-search-exhub-get-window-allocation win))))
         (line-height (line-pixel-height)))
    (if (and height line-height (> height 0) (> line-height 0))
        (/ height line-height)
      20)))

(defun blink-search-exhub-init-bottom-layout ()
  "Initialize the bottom split window layout."
  (let* ((input-height 3)
         (tooltip-height 1)
         (cand-win (selected-window)))
    ;; Split: top (original) / candidate / tooltip+input
    (delete-other-windows)
    (switch-to-buffer blink-search-exhub-candidate-buffer)

    ;; Create tooltip window at bottom
    (let ((tooltip-win (split-window nil (- (+ tooltip-height input-height)) 'below)))
      (set-window-buffer tooltip-win blink-search-exhub-tooltip-buffer)
      ;; Create input window below tooltip
      (let ((input-win (split-window tooltip-win (- input-height) 'below)))
        (set-window-buffer input-win blink-search-exhub-input-buffer)
        (select-window input-win)
        ;; Dedicate all session windows so `display-buffer' (warnings,
        ;; completions, help popups...) can never replace their buffers:
        ;; a replaced input window silently kills all navigation and
        ;; every render guard afterwards.
        (dolist (win (list cand-win tooltip-win input-win))
          (set-window-dedicated-p win t))))))

;; ===========================================================================
;; Disable options for search buffers
;; ===========================================================================

(defun blink-search-exhub-disable-options (&optional disable-cursor)
  "Disable UI options for blink-search buffers."
  (when display-line-numbers
    (setq-local display-line-numbers nil))
  (when (version< "27.0" emacs-version)
    (setq-local tab-line-format nil))
  (setq-local header-line-format nil)
  (setq-local mode-line-format nil)
  (when disable-cursor
    (setq-local cursor-type nil)))

;; ===========================================================================
;; Main Entry Point
;; ===========================================================================

;;;###autoload
(defun blink-search-exhub (&optional arg)
  "Start blink-search via ExHub.
With prefix ARG, search current symbol."
  (interactive "P")

  (setq blink-search-exhub-start-buffer (current-buffer))
  (setq blink-search-exhub-start-buffer-name (buffer-name (current-buffer)))
  (setq blink-search-exhub-start-path-name (buffer-file-name (current-buffer)))
  (setq blink-search-exhub-start-buffer-directory
        (with-current-buffer (current-buffer)
          (expand-file-name default-directory)))

  (setq blink-search-exhub-start-keyword
        (cond
         ((region-active-p)
          (buffer-substring-no-properties (region-beginning) (region-end)))
         (t
          (if arg (or (thing-at-point 'symbol t) "") ""))))

  ;; Save window configuration
  (unless blink-search-exhub-window-configuration
    (setq blink-search-exhub-window-configuration (current-window-configuration)))

  ;; Create buffers
  (with-current-buffer (get-buffer-create blink-search-exhub-input-buffer)
    (erase-buffer)
    (blink-search-exhub-mode)
    (add-hook 'after-change-functions 'blink-search-exhub-monitor-input nil t)
    (blink-search-exhub-disable-options nil)
    (setq-local left-margin-width 1)
    (setq-local right-margin-width 1))

  (with-current-buffer (get-buffer-create blink-search-exhub-tooltip-buffer)
    (erase-buffer)
    (blink-search-exhub-disable-options t)
    (face-remap-add-relative 'hl-line :background (face-background 'default))
    (setq-local left-margin-width 1)
    (setq-local right-margin-width 1))

  (with-current-buffer (get-buffer-create blink-search-exhub-candidate-buffer)
    (erase-buffer)
    (blink-search-exhub-disable-options t)
    (setq-local truncate-lines t))

  (with-current-buffer (get-buffer-create blink-search-exhub-backend-buffer)
    (erase-buffer)
    (blink-search-exhub-disable-options t)
    (setq-local truncate-lines t))

  ;; Init colors
  (blink-search-exhub-init-colors)

  ;; Layout
  (blink-search-exhub-init-bottom-layout)

  ;; Run start update functions
  (dolist (update-func blink-search-exhub-start-update-list)
    (funcall update-func))

  ;; Initialize search directory and current buffer on Elixir side
  (blink-search-exhub-call-flat "init_search_dir" blink-search-exhub-start-buffer-directory)

  ;; Push Emacs-side configuration to the Elixir backends
  (blink-search-exhub-call-flat "init_common_directory" blink-search-exhub-common-directory)
  (when blink-search-exhub-grep-pdf-search-paths
    (blink-search-exhub-call-flat "init_grep_pdf_paths"
                                  blink-search-exhub-grep-pdf-search-paths))

  ;; Push Key Value Store config
  (blink-search-exhub-call-flat "update" "Key Value"
                                (list blink-search-exhub-kv-db-path
                                      blink-search-exhub-kv-db-table))

  ;; Send current buffer content for Current Buffer backend
  (blink-search-exhub-call-flat "init_current_buffer"
                                (buffer-name blink-search-exhub-start-buffer)
                                (base64-encode-string
                                 (encode-coding-string
                                  (with-current-buffer blink-search-exhub-start-buffer
                                    (buffer-string))
                                  'utf-8)))

  ;; Start initial search
  (when (and (exhub-open-connection)
             (not (string-empty-p blink-search-exhub-start-keyword)))
    (message "[blink-search-exhub] Search symbol '%s'" blink-search-exhub-start-keyword))
  (blink-search-exhub-call-flat "search"
                                blink-search-exhub-start-keyword
                                (blink-search-exhub-get-row-number)
                                blink-search-exhub-search-backends)

  ;; Start idle synchronization timer (elisp symbols etc.)
  (unless blink-search-exhub-idle-timer
    (setq blink-search-exhub-idle-timer
          (run-with-idle-timer blink-search-exhub-elisp-symbol-update-idle
                               t
                               #'blink-search-exhub-run-idle-updates)))

  ;; Focus guard: re-anchor focus stolen by async popups while open
  (unless blink-search-exhub-focus-timer
    (setq blink-search-exhub-focus-timer
          (run-with-timer 0.4 0.4 #'blink-search-exhub-focus-guard))))

;; ===========================================================================
;; Input Monitoring
;; ===========================================================================

(defun blink-search-exhub-monitor-input (_begin _end _length)
  "Monitor input changes and dispatch search to ExHub."
  (when (string-equal (buffer-name) blink-search-exhub-input-buffer)
    (let* ((input (string-trim
                   (with-current-buffer blink-search-exhub-input-buffer
                     (buffer-substring-no-properties (point-min) (point-max))))))
      (cond ((or (string-prefix-p "#" input)
                 (string-prefix-p "#" input))
             (blink-search-exhub-call-flat "search" (substring input 1)
                                           (blink-search-exhub-get-row-number)
                                           '("Current Buffer")))
            ((or (string-prefix-p "!" input)
                 (string-prefix-p "！" input))
             (blink-search-exhub-call-flat "search" (substring input 1)
                                           (blink-search-exhub-get-row-number)
                                           '("Grep File")))
            ((or (string-prefix-p ";" input)
                 (string-prefix-p "；" input))
             (blink-search-exhub-call-flat "search" (substring input 1)
                                           (blink-search-exhub-get-row-number)
                                           '("Grep PDF")))
            ((or (string-prefix-p ":" input)
                 (string-prefix-p "：" input))
             (blink-search-exhub-call-flat "search" (substring input 1)
                                           (blink-search-exhub-get-row-number)
                                           '("PDF")))
            (t
             (blink-search-exhub-call-flat "search" input
                                           (blink-search-exhub-get-row-number)
                                           blink-search-exhub-search-backends))))))

;; ===========================================================================
;; Navigation Commands
;; ===========================================================================

(defun blink-search-exhub-candidate-select-next ()
  (interactive)
  (blink-search-exhub-call-flat "select_next_candidate"))

(defun blink-search-exhub-candidate-select-prev ()
  (interactive)
  (blink-search-exhub-call-flat "select_prev_candidate"))

(defun blink-search-exhub-backend-select-next ()
  (interactive)
  (blink-search-exhub-call-flat "select_next_backend"))

(defun blink-search-exhub-backend-select-prev ()
  (interactive)
  (blink-search-exhub-call-flat "select_prev_backend"))

(defun blink-search-exhub-candidate-group-select-next ()
  (interactive)
  (blink-search-exhub-call-flat "select_next_group"))

(defun blink-search-exhub-candidate-group-select-prev ()
  (interactive)
  (blink-search-exhub-call-flat "select_prev_group"))

;; ===========================================================================
;; Action Commands
;; ===========================================================================

(defun blink-search-exhub-get-candidate-text (candidate-info)
  "Extract text from CANDIDATE-INFO (string or plist)."
  (replace-regexp-in-string
   "\r+$" ""
   (format "%s"
           (if (stringp candidate-info)
               candidate-info
             (plist-get candidate-info :text)))))

(defun blink-search-exhub-do ()
  "Execute action for selected candidate."
  (interactive)
  (let* ((item (nth blink-search-exhub-candidate-select-index
                    blink-search-exhub-candidate-items))
         (backend-name (plist-get item :backend))
         (candidate-info (plist-get item :candidate))
         (candidate (blink-search-exhub-get-candidate-text candidate-info)))
    (blink-search-exhub-quit)
    (blink-search-exhub-call-flat "do" backend-name candidate)))

(defun blink-search-exhub-preview ()
  "Preview selected candidate."
  (interactive)
  (let* ((item (nth blink-search-exhub-candidate-select-index
                    blink-search-exhub-candidate-items))
         (backend-name (plist-get item :backend))
         (candidate-info (plist-get item :candidate))
         (candidate (blink-search-exhub-get-candidate-text candidate-info)))
    (blink-search-exhub-call-flat "select" backend-name candidate)))

(defun blink-search-exhub-preview-next ()
  "Preview next candidate."
  (interactive)
  (blink-search-exhub-candidate-select-next)
  (blink-search-exhub-preview))

(defun blink-search-exhub-preview-prev ()
  "Preview previous candidate."
  (interactive)
  (blink-search-exhub-candidate-select-prev)
  (blink-search-exhub-preview))

(defun blink-search-exhub-parent ()
  "Navigate to parent of selected candidate."
  (interactive)
  (let* ((item (nth blink-search-exhub-candidate-select-index
                    blink-search-exhub-candidate-items))
         (backend-name (plist-get item :backend))
         (candidate-info (plist-get item :candidate))
         (candidate (blink-search-exhub-get-candidate-text candidate-info)))
    (blink-search-exhub-quit)
    (blink-search-exhub-call-flat "parent" backend-name candidate)))

(defun blink-search-exhub-continue ()
  "Continue search in subdirectory of selected candidate."
  (interactive)
  (let* ((item (nth blink-search-exhub-candidate-select-index
                    blink-search-exhub-candidate-items))
         (backend-name (plist-get item :backend))
         (candidate-info (plist-get item :candidate))
         (candidate (blink-search-exhub-get-candidate-text candidate-info)))
    (blink-search-exhub-call-flat "continue" backend-name candidate)))

(defun blink-search-exhub-copy ()
  "Copy selected candidate text."
  (interactive)
  (let* ((item (nth blink-search-exhub-candidate-select-index
                    blink-search-exhub-candidate-items))
         (backend-name (plist-get item :backend))
         (candidate-info (plist-get item :candidate))
         (candidate (blink-search-exhub-get-candidate-text candidate-info)))
    (blink-search-exhub-call-flat "copy" backend-name candidate)))

(defun blink-search-exhub-quick-do ()
  "Execute action for candidate matching quick key."
  (interactive)
  (let* ((event-type (event-basic-type last-command-event))
         (event-string (if (characterp event-type)
                           (string event-type)
                         (error "Unexpected input")))
         (candidate-index (cl-position event-string blink-search-exhub-quick-keys :test 'equal)))
    (when (< candidate-index (length blink-search-exhub-candidate-items))
      (let* ((item (nth candidate-index blink-search-exhub-candidate-items))
             (backend-name (plist-get item :backend))
             (candidate-info (plist-get item :candidate))
             (candidate (blink-search-exhub-get-candidate-text candidate-info)))
        (blink-search-exhub-quit)
        (blink-search-exhub-call-flat "do" backend-name candidate)))))

;; ===========================================================================
;; Quit
;; ===========================================================================

(defun blink-search-exhub-focus-guard ()
  "Re-select the input window when a foreign popup stole focus.
Async popups (flymake/lsp logs, warnings) can select their own window
between renders even though session windows are dedicated; left alone
this breaks repeated C-n/M-n navigation because the keymap lives in the
input buffer only.  Session windows are left untouched so deliberate
navigation inside the layout still works."
  (when (and blink-search-exhub-window-configuration
             (not (active-minibuffer-window)))
    (let ((input-win (get-buffer-window blink-search-exhub-input-buffer)))
      (when (and input-win
                 (not (memq (selected-window)
                            (list input-win
                                  (get-buffer-window blink-search-exhub-candidate-buffer)
                                  (get-buffer-window blink-search-exhub-backend-buffer)
                                  (get-buffer-window blink-search-exhub-tooltip-buffer)
                                  blink-search-exhub-preview-window))))
        (select-window input-win)))))

(defun blink-search-exhub-quit ()
  "Quit blink-search and restore window configuration."
  (interactive)
  (blink-search-exhub-call-flat "clean")

  ;; Stop idle synchronization timer
  (when blink-search-exhub-idle-timer
    (cancel-timer blink-search-exhub-idle-timer)
    (setq blink-search-exhub-idle-timer nil))
  (when blink-search-exhub-focus-timer
    (cancel-timer blink-search-exhub-focus-timer)
    (setq blink-search-exhub-focus-timer nil))
  (setq blink-search-exhub-elisp-symbol-size 0)
  (setq blink-search-exhub-preview-window nil)

  (when blink-search-exhub-window-configuration
    (set-window-configuration blink-search-exhub-window-configuration)
    (setq blink-search-exhub-window-configuration nil)
    (setq blink-search-exhub-start-buffer nil)
    (setq blink-search-exhub-start-buffer-name nil)
    (setq blink-search-exhub-start-path-name nil)
    (setq blink-search-exhub-start-buffer-directory nil)
    (setq blink-search-exhub-continue-directory nil)))

;; ===========================================================================
;; Rendering (called from Elixir via WebSocket)
;; ===========================================================================

(defun blink-search-exhub-select-preview-window ()
  "Select a persistent preview window, creating one if needed.
The preview window is split off the candidate window so previews never
replace the buffer of the input or candidate windows.  A newly split
preview window is dedicated so `display-buffer' cannot steal or replace
it either."
  (if (and (window-live-p blink-search-exhub-preview-window)
           (not (memq blink-search-exhub-preview-window
                      (list (get-buffer-window blink-search-exhub-input-buffer)
                            (get-buffer-window blink-search-exhub-candidate-buffer)
                            (get-buffer-window blink-search-exhub-backend-buffer)))))
      (select-window blink-search-exhub-preview-window)
    (let* ((cand-win (get-buffer-window blink-search-exhub-candidate-buffer))
           (session-windows
            (list cand-win
                  (get-buffer-window blink-search-exhub-tooltip-buffer)
                  (get-buffer-window blink-search-exhub-input-buffer)
                  (get-buffer-window blink-search-exhub-backend-buffer)))
           (fresh-win
            (and cand-win
                 (ignore-errors
                   (select-window cand-win)
                   (split-window (selected-window) nil 'right t))))
           ;; Fallbacks, in order: the window showing the user's original
           ;; buffer (any window outside the session layout), then — as a
           ;; last resort — the candidate window.  Never run previews in
           ;; the input/candidate/backend window when avoidable: opening a
           ;; file there replaces its buffer and kills rendering and focus
           ;; restoration for the rest of the session.
           (fallback-win
            (let ((found nil))
              (dolist (win (window-list) found)
                (when (and (not found) (not (memq win session-windows)))
                  (setq found win)))))
           (preview-win (or fresh-win fallback-win cand-win)))
      (when fresh-win
        (set-window-dedicated-p fresh-win t))
      (setq blink-search-exhub-preview-window preview-win)
      (select-window preview-win))))

(defmacro blink-search-exhub-select-input-window (&rest body)
  "Evaluate BODY in the preview window, then re-select the input window.
BODY typically opens a file/buffer for preview.  It must not run with
the input window selected: `find-file' would replace the input window's
buffer, after which `(get-buffer-window ...input-buffer)' returns nil,
focus is lost and repeated C-n/M-n navigation stops working."
  (declare (indent 0))
  `(let ((input-window (get-buffer-window blink-search-exhub-input-buffer)))
     (when input-window
       (unwind-protect
           (progn
             (blink-search-exhub-select-preview-window)
             ,@body)
         (when (window-live-p input-window)
           (select-window input-window))))))

(defun blink-search-exhub-goto-column (column)
  "Move point to COLUMN, handling mixed CJK/ASCII correctly."
  (let ((current-column 0))
    (while (and (< current-column column) (not (eolp)))
      (setq current-column (+ current-column (char-width (char-after))))
      (forward-char 1))))

(defun blink-search-exhub-flash-locate ()
  "Flash the current line to indicate navigation target."
  (when (> blink-search-exhub-flash-line-delay 0)
    (pulse-momentary-highlight-one-line (point) 'blink-search-exhub-flash-face)))

(defun blink-search-exhub-update-items (candidate-items
                                        candidate-select-index
                                        backend-items
                                        backend-select-index
                                        backend-name
                                        search-items-index
                                        search-items-number
                                        backend-number)
  "Update search results from Elixir backend.
Called via WebSocket by the ExHub BlinkSearch server."
  (when (get-buffer-window blink-search-exhub-input-buffer)
    (setq blink-search-exhub-candidate-items candidate-items)
    (setq blink-search-exhub-candidate-select-index candidate-select-index)
    (setq blink-search-exhub-backend-items backend-items)
    (setq blink-search-exhub-backend-select-index backend-select-index)
    (setq blink-search-exhub-backend-name backend-name)
    (setq blink-search-exhub-item-index search-items-index)
    (setq blink-search-exhub-items-number search-items-number)
    (setq blink-search-exhub-backend-number backend-number)

    ;; Render with the window selection frozen: backend-window juggling
    ;; below must never leak focus, otherwise repeated C-n/M-n navigation
    ;; silently breaks (keymap lives in the input buffer only).
    (save-selected-window
      (if (> backend-number 1)
          (blink-search-exhub-show-backend-window)
        (blink-search-exhub-hide-backend-window))
      (blink-search-exhub-render))

    ;; Self-heal focus: async popups (flymake log, lsp dialogs, warnings)
    ;; can steal the selected window between renders even though session
    ;; windows are dedicated.  Every render re-anchors selection to the
    ;; input window so repeated C-n/M-n navigation never breaks.
    (let ((input-win (get-buffer-window blink-search-exhub-input-buffer)))
      (when (and input-win (not (eq (selected-window) input-win)))
        (select-window input-win)))))

(defun blink-search-exhub-show-backend-window ()
  "Show the backend detail window."
  (unless (get-buffer-window blink-search-exhub-backend-buffer)
    (save-excursion
      (blink-search-exhub-select-window-safe (get-buffer-window blink-search-exhub-candidate-buffer))
      (let ((backend-win (ignore-errors
                           (split-window (selected-window) nil 'right t))))
        (if backend-win
            (progn
              (set-window-buffer backend-win blink-search-exhub-backend-buffer)
              (set-window-dedicated-p backend-win t))
          ;; No room to split: degrade by reusing the candidate window.
          (set-window-buffer (selected-window) blink-search-exhub-backend-buffer)))
      (blink-search-exhub-select-window-safe (get-buffer-window blink-search-exhub-input-buffer)))))

(defun blink-search-exhub-hide-backend-window ()
  "Hide the backend detail window."
  (when (get-buffer-window blink-search-exhub-backend-buffer)
    (save-excursion
      (delete-window (get-buffer-window blink-search-exhub-backend-buffer))
      (blink-search-exhub-select-window-safe (get-buffer-window blink-search-exhub-input-buffer)))))

(defun blink-search-exhub-select-window-safe (window)
  "Select WINDOW if it is live."
  (when (window-live-p window)
    (select-window window)))

(defun blink-search-exhub-render ()
  "Render candidate and backend items in their respective buffers."
  (let ((candidate-items blink-search-exhub-candidate-items)
        (candidate-select-index blink-search-exhub-candidate-select-index)
        (backend-items blink-search-exhub-backend-items)
        (backend-select-index blink-search-exhub-backend-select-index)
        (backend-name blink-search-exhub-backend-name)
        (search-items-index blink-search-exhub-item-index)
        (search-items-number blink-search-exhub-items-number)
        (backend-number blink-search-exhub-backend-number))
    (save-excursion
      (let* ((win-alloc (blink-search-exhub-get-window-allocation
                         (get-buffer-window blink-search-exhub-candidate-buffer)))
             (window-width (nth 2 win-alloc)))

        ;; Tooltip
        (with-current-buffer blink-search-exhub-tooltip-buffer
          (let* ((tooltip-alloc (blink-search-exhub-get-window-allocation
                                 (get-buffer-window blink-search-exhub-input-buffer)))
                 (tooltip-width (nth 2 tooltip-alloc)))
            (erase-buffer)
            (insert
             (concat
              (propertize (format "%s [%s/%s]" backend-name search-items-index search-items-number)
                          'face 'font-lock-constant-face)
              (propertize " search prefix: " 'face 'font-lock-type-face)
              (propertize "#" 'face 'font-lock-type-face)
              (propertize " buffer " 'face 'font-lock-keyword-face)
              (propertize "!" 'face 'font-lock-type-face)
              (propertize " directory " 'face 'font-lock-keyword-face)
              (propertize ";" 'face 'font-lock-type-face)
              (propertize " pdfs " 'face 'font-lock-keyword-face)
              (propertize ":" 'face 'font-lock-type-face)
              (propertize " pdf " 'face 'font-lock-keyword-face)))))

        ;; Candidate buffer
        (with-current-buffer blink-search-exhub-candidate-buffer
          (let* ((candidate-max-length (ceiling (* (/ window-width (frame-char-width)) 0.6)))
                 (candidate-index 0))
            (erase-buffer)
            (when candidate-items
              (dolist (item candidate-items)
                (let* ((candidate-info (plist-get item :candidate))
                       (candidate (blink-search-exhub-get-candidate-text candidate-info))
                       (matches (unless (stringp candidate-info)
                                  (plist-get candidate-info :matches)))
                       (backend (plist-get item :backend))
                       (display-candidate
                        (if (<= (length candidate) candidate-max-length)
                            candidate
                          (concat (substring candidate 0 (/ candidate-max-length 2))
                                  "..."
                                  (substring candidate (- (length candidate)
                                                          (/ candidate-max-length 2))))))
                       candidate-line)

                  (setq candidate-line
                        (concat
                         (propertize (format "%s " (nth candidate-index blink-search-exhub-quick-keys))
                                     'face 'font-lock-type-face)
                         (if (> backend-number 1)
                             (format "%s " display-candidate)
                           (format "%s " candidate))
                         (when (> backend-number 1)
                           (propertize (format "%s " backend)
                                       'face (if (equal candidate-index candidate-select-index)
                                                 'blink-search-exhub-select-face
                                               'font-lock-doc-face)))
                         "\n"))

                  ;; Highlight matches
                  (when (and matches (equal backend-number 1))
                    (dolist (match matches)
                      (let ((match-start (nth 0 match))
                            (match-end (nth 1 match)))
                        (when (and match-start match-end
                                   (< match-end (length candidate-line)))
                          (add-face-text-property match-start match-end
                                                  'font-lock-type-face 'append candidate-line)))))

                  ;; Highlight selected line
                  (when (equal candidate-index candidate-select-index)
                    (add-face-text-property 0 (length candidate-line)
                                            'blink-search-exhub-select-face 'append candidate-line))

                  (insert candidate-line)
                  (setq candidate-index (1+ candidate-index)))))))

        ;; Backend buffer
        (with-current-buffer blink-search-exhub-backend-buffer
          (erase-buffer)
          (when (> backend-number 1)
            (let ((backend-index 0))
              (when backend-items
                (dolist (candidate-info backend-items)
                  (let* ((candidate (if (stringp candidate-info)
                                        candidate-info
                                      (plist-get candidate-info :text)))
                         (backend-line
                          (concat
                           (propertize (format " %s " candidate)
                                       'face (if (equal backend-index backend-select-index)
                                                 'blink-search-exhub-select-face
                                               'font-lock-doc-face))
                           "\n")))
                    (when (equal backend-index backend-select-index)
                      (add-face-text-property 0 (length backend-line)
                                              'blink-search-exhub-select-face 'append backend-line))
                    (insert backend-line)
                    (setq backend-index (1+ backend-index))))))))))))

;; ===========================================================================
;; Backend Data Sync (pushed from Emacs to Elixir)
;; ===========================================================================

(defun blink-search-exhub-buffer-list-update ()
  "Sync buffer list to Elixir backend."
  (when (exhub-open-connection)
    (blink-search-exhub-call-flat "update" "Buffer List"
                                  (mapcar #'buffer-name (buffer-list)))))

(defun blink-search-exhub-recent-file-update ()
  "Sync recent files to Elixir backend."
  (when (exhub-open-connection)
    (blink-search-exhub-call-flat "update" "Recent File"
                                  (mapcar #'substring-no-properties recentf-list))))

(defun blink-search-exhub-imenu-update ()
  "Sync imenu candidates to Elixir backend."
  (when (and (exhub-open-connection) blink-search-exhub-start-buffer)
    (with-current-buffer blink-search-exhub-start-buffer
      (let ((candidates (blink-search-exhub-imenu-get-candidates)))
        (when candidates
          (blink-search-exhub-call-flat "update" "IMenu" candidates))))))

(defun blink-search-exhub-imenu-get-candidates ()
  "Get imenu candidates as (name position) pairs."
  (ignore-errors
    (mapcar (lambda (info) (list (car info) (marker-position (cdr info))))
            (let* ((index (ignore-errors (imenu--make-index-alist t))))
              (when index
                (blink-search-exhub-imenu-build-candidates
                 (delete (assoc "*Rescan*" index) index)))))))

(defun blink-search-exhub-imenu-build-candidates (alist)
  "Build flat candidate list from imenu ALIST."
  (cl-remove-if
   (lambda (c)
     (or (string-equal (car c) "Types")
         (string-equal (car c) "Variables")))
   (cl-loop for elm in alist
            nconc (cond
                   ((imenu--subalist-p elm)
                    (blink-search-exhub-imenu-build-candidates
                     (cl-loop for (e . v) in (cdr elm) collect
                              (cons e (if (integerp v) (copy-marker v) v)))))
                   ((listp (cdr elm))
                    (and elm (list elm)))
                   (t
                    (and (cdr elm)
                         (setcdr elm (pcase (cdr elm)
                                       ((and ov (pred overlayp)) (copy-overlay ov))
                                       ((and mk (or (pred markerp) (pred integerp)))
                                        (copy-marker mk))))
                         (list elm)))))))

;; Register start update functions
(add-to-list 'blink-search-exhub-start-update-list #'blink-search-exhub-buffer-list-update t)
(add-to-list 'blink-search-exhub-start-update-list #'blink-search-exhub-recent-file-update t)
(add-to-list 'blink-search-exhub-start-update-list #'blink-search-exhub-imenu-update t)

;; ---------------------------------------------------------------------------
;; Elisp Symbol sync (pushed from Emacs to Elixir on idle)
;; ---------------------------------------------------------------------------

(defconst blink-search-exhub-elisp-parse-depth 100)
(defconst blink-search-exhub-elisp-parse-limit 30)

(defun blink-search-exhub-elisp-global-symbols ()
  "All globally interned symbols bound as function/variable/feature/face."
  (all-completions ""
                   obarray
                   (lambda (symbol)
                     (or (fboundp symbol)
                         (boundp symbol)
                         (featurep symbol)
                         (facep symbol)))))

(defun blink-search-exhub-elisp-local-symbols ()
  "Symbols bound by enclosing binding forms in elisp buffers."
  (when (or (derived-mode-p 'emacs-lisp-mode)
            (derived-mode-p 'inferior-emacs-lisp-mode)
            (derived-mode-p 'lisp-interaction-mode))
    (let ((regexp "[ \t\n]*\\(\\_<\\(?:\\sw\\|\\s_\\)*\\_>\\)")
          (pos (point))
          res)
      (condition-case nil
          (save-excursion
            (dotimes (_ blink-search-exhub-elisp-parse-depth)
              (up-list -1)
              (save-excursion
                (when (eq (char-after) ?\()
                  (forward-char 1)
                  (when (ignore-errors
                          (save-excursion (forward-list)
                                          (<= (point) pos)))
                    (skip-chars-forward " \t\n")
                    (cond
                     ((looking-at "\\_<\\(?:cl-\\)?\\(?:def\\(?:macro\\|subst\\|un\\)\\|l\\(?:ambda\\|e\\(?:\\(?:xical-le\\)?t\\)\\)\\)\\*?\\_>")
                      (down-list 1)
                      (condition-case nil
                          (dotimes (_ blink-search-exhub-elisp-parse-limit)
                            (save-excursion
                              (when (looking-at "[ \t\n]*(")
                                (down-list 1))
                              (when (looking-at regexp)
                                (cl-pushnew (match-string-no-properties 1) res)))
                            (forward-sexp))
                        (scan-error nil)))
                     ((looking-at "\\_<\\(?:cl-\\)?\\(?:do\\(?:list\\|times\\)\\)\\*?\\_>")
                      (down-list 1)
                      (when (looking-at regexp)
                        (cl-pushnew (match-string-no-properties 1) res)))))))))
        (scan-error nil))
      res)))

(defun blink-search-exhub-elisp-symbol-update ()
  "Synchronize elisp symbols to the ExHub Elisp Symbol backend.
Only pushes when the symbol count changed (mirrors upstream)."
  (when (exhub-open-connection)
    (let* ((symbols (append (blink-search-exhub-elisp-local-symbols)
                            (blink-search-exhub-elisp-global-symbols)))
           (symbols-size (length symbols)))
      (unless (equal blink-search-exhub-elisp-symbol-size symbols-size)
        (blink-search-exhub-call-flat "update" "Elisp Symbol" symbols)
        (setq blink-search-exhub-elisp-symbol-size symbols-size)))))

(defun blink-search-exhub-run-idle-updates ()
  "Run registered idle update functions."
  (dolist (update-func blink-search-exhub-idle-update-list)
    (ignore-errors (funcall update-func))))

(add-to-list 'blink-search-exhub-idle-update-list
             #'blink-search-exhub-elisp-symbol-update t)

;; ===========================================================================
;; Action callbacks (called from Elixir via WebSocket)
;; ===========================================================================

(defun blink-search-open-file (path)
  "Open file or directory at PATH."
  (if (file-directory-p path)
      (dired path)
    (find-file path)))

(defun blink-search-grep-file-do (file line column)
  "Open FILE at LINE and COLUMN."
  (find-file file)
  (ignore-errors
    (goto-line line)
    (blink-search-exhub-goto-column column))
  (blink-search-exhub-flash-locate))

(defun blink-search-grep-file-preview (file line column)
  "Preview FILE at LINE and COLUMN."
  (blink-search-exhub-select-input-window
   (blink-search-grep-file-do file line column)))

(defun blink-search-grep-file-clean ()
  "Clean up grep file temp buffers.")

(defun blink-search-current-buffer-do (buffer line column)
  "Navigate to LINE COLUMN in BUFFER."
  (switch-to-buffer buffer)
  (goto-line line)
  (blink-search-exhub-goto-column column)
  (blink-search-exhub-flash-locate))

(defun blink-search-current-buffer-preview (buffer line column)
  "Preview LINE COLUMN in BUFFER."
  (blink-search-exhub-select-input-window
   (blink-search-current-buffer-do buffer line column)))

(defun blink-search-imenu-do (point)
  "Navigate to POINT in current buffer."
  (goto-char point)
  (blink-search-exhub-flash-locate))

(defun blink-search-elisp-symbol-do (candidate)
  "Execute action for elisp symbol CANDIDATE."
  (let* ((symbol (intern candidate)))
    (cond ((commandp symbol) (call-interactively symbol))
          ((or (functionp symbol) (macrop symbol)) (describe-function symbol))
          ((facep symbol) (customize-face symbol))
          ((custom-variable-p symbol) (customize-option symbol))
          (t (describe-variable symbol)))))

(defun blink-search-continue-search (path)
  "Continue search in directory PATH."
  (setq blink-search-exhub-continue-directory path)
  (blink-search-exhub-call-flat "init_search_dir" path)
  ;; Re-trigger search with current input
  (with-current-buffer blink-search-exhub-input-buffer
    (let ((input (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
      (blink-search-exhub-call-flat "search" input
                                    (blink-search-exhub-get-row-number)
                                    blink-search-exhub-search-backends))))

(defun blink-search-exhub-pdf-goto (file page match-text)
  "Open PDF FILE at PAGE and highlight MATCH-TEXT occurrences.
Uses pdf-tools when available, falls back to doc-view."
  (find-file file)
  (cond
   ((and (bound-and-true-p pdf-view-mode) (fboundp 'pdf-view-goto-page))
    (pdf-view-goto-page (max 1 (string-to-number page)))
    (when (and (fboundp 'pdf-isearch-search-page)
               match-text (not (string-empty-p match-text)))
      (ignore-errors
        (let ((matches (pdf-isearch-search-page match-text)))
          (when matches
            (pdf-isearch-hl-matches (nth 0 matches) matches t))))))
   ((and (bound-and-true-p doc-view-mode) (fboundp 'doc-view-goto-page))
    (doc-view-goto-page (max 1 (string-to-number page)))
    (when (and (fboundp 'doc-view-search)
               match-text (not (string-empty-p match-text)))
      (ignore-errors (doc-view-search match-text))))
   (t
    (message "[blink-search-exhub] No PDF viewer for %s" file))))

(defun blink-search-grep-pdf-do (file line match-text)
  "Open PDF FILE at LINE (rga reports pages as lines) highlighting MATCH-TEXT."
  (blink-search-exhub-pdf-goto file line match-text))

(defun blink-search-grep-pdf-preview (file line match-text)
  "Preview PDF FILE at LINE."
  (blink-search-exhub-select-input-window
   (blink-search-grep-pdf-do file line match-text)))

(defun blink-search-grep-pdf-clean ()
  "Clean up grep PDF resources.")

(defun blink-search-pdf-do (file line match-text)
  "Open PDF FILE at LINE highlighting MATCH-TEXT."
  (blink-search-exhub-pdf-goto file line match-text))

(defun blink-search-pdf-preview (file line match-text)
  "Preview PDF FILE at LINE."
  (blink-search-exhub-select-input-window
   (blink-search-pdf-do file line match-text)))

(defun blink-search-pdf-clean ()
  "Clean up PDF resources.")

(provide 'blink-search-exhub)
;;; blink-search-exhub.el ends here
