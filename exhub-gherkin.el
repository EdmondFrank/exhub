;;; exhub-gherkin.el --- Preview Gherkin .feature files as Org -*- lexical-binding: t; -*-

;; Copyright (C) 2025 EdmondFrank

;; Author: EdmondFrank
;; Keywords: tools, gherkin, bdd, org

;;; Commentary:

;; `exhub-gherkin-preview' renders the current `.feature' buffer, or an
;; arbitrary feature file, into a read-only Org buffer: Feature / Rule /
;; Scenario become headings, tags become Org tags, steps become list
;; items, and doc strings and data tables keep their structure.  Press
;; `RET' on a heading or step to jump back to the source line, `g' to
;; refresh and `q' to dismiss the preview.
;;
;; Enable `exhub-gherkin-mode' in `.feature' buffers for a `C-c C-v'
;; preview key and refresh-on-save:
;;
;;   (add-hook 'find-file-hook
;;             (lambda ()
;;               (when (and (buffer-file-name)
;;                          (string-match-p "\\.feature\\'" (buffer-file-name)))
;;                 (exhub-gherkin-mode 1))))
;;
;; The parser lives in `exhub-gherkin-parse.el' and the Org renderer in
;; `exhub-gherkin-render.el'.

;;; Code:

(require 'org)
(require 'subr-x)

(require 'exhub-gherkin-parse)
(require 'exhub-gherkin-render)

(defcustom exhub-gherkin-preview-display 'side-window
  "How `exhub-gherkin-preview' displays the preview buffer.
`side-window' shows it in a right side window, `bottom' in a full-width
window at the bottom, `popup' in a window split below the selected one,
and `switch' switches the selected window to the preview."
  :type '(choice (const :tag "Side window" side-window)
                 (const :tag "Bottom window" bottom)
                 (const :tag "Pop-up window" popup)
                 (const :tag "Switch to buffer" switch)
                 (const :tag "No display (testing)" none))
  :group 'exhub-gherkin)

(defcustom exhub-gherkin-preview-width 0.45
  "Width of the right side window used by the Gherkin preview.
A float is a ratio of the frame width, an integer a column count."
  :type '(choice (float :tag "Ratio of frame width")
                 (integer :tag "Columns"))
  :group 'exhub-gherkin)

(defcustom exhub-gherkin-auto-refresh t
  "Whether saving a `.feature' buffer refreshes its open previews.
Only applies in buffers where `exhub-gherkin-mode' is enabled."
  :type 'boolean
  :group 'exhub-gherkin)

(defvar-local exhub-gherkin--source-file nil
  "File the current Gherkin preview was rendered from, if any.")

(defvar-local exhub-gherkin--source-buffer nil
  "Live buffer the current Gherkin preview was rendered from, if any.")

;;;; Preview mode

(defvar exhub-gherkin-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    map)
  "Keymap for `exhub-gherkin-preview-mode'.")

(define-derived-mode exhub-gherkin-preview-mode org-mode "Gherkin"
  "Major mode for a read-only Org rendering of a Gherkin feature.
\\{exhub-gherkin-preview-mode-map}"
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil)
  (setq-local header-line-format " Gherkin preview"))

(define-key exhub-gherkin-preview-mode-map (kbd "g")
  #'exhub-gherkin-preview-refresh)
(define-key exhub-gherkin-preview-mode-map (kbd "q") #'quit-window)
(define-key exhub-gherkin-preview-mode-map (kbd "RET")
  #'exhub-gherkin-goto-source)
(define-key exhub-gherkin-preview-mode-map (kbd "C-c C-o")
  #'exhub-gherkin-goto-source)

;;;; Rendering into the preview buffer

(defun exhub-gherkin--render-buffer (buffer feature source-file source-buffer)
  "Render FEATURE into BUFFER.
SOURCE-FILE and SOURCE-BUFFER are remembered for refresh and for
`exhub-gherkin-goto-source'."
  (with-current-buffer buffer
    (let ((preview-p (derived-mode-p 'exhub-gherkin-preview-mode))
          (inhibit-read-only t)
          (inhibit-modification-hooks t))
      (erase-buffer)
      (insert (exhub-gherkin-render feature source-file))
      (unless preview-p
        (exhub-gherkin-preview-mode))
      (setq-local exhub-gherkin--source-file source-file)
      (setq-local exhub-gherkin--source-buffer source-buffer)
      (setq-local header-line-format
                  (format " Gherkin | %s"
                          (or (and source-file (file-name-nondirectory source-file))
                              (buffer-name buffer))))
      (goto-char (point-min))
      (when (fboundp 'org-set-startup-visibility)
        (ignore-errors (org-set-startup-visibility)))
      (when (fboundp 'font-lock-ensure)
        (font-lock-ensure)))))

(defun exhub-gherkin--buffer-name (name source-file)
  "Return the preview buffer name for NAME and SOURCE-FILE."
  (format "*gherkin: %s*"
          (if source-file (abbreviate-file-name source-file) name)))

(defun exhub-gherkin--display (buffer)
  "Display preview BUFFER according to `exhub-gherkin-preview-display'."
  (pcase exhub-gherkin-preview-display
    ('none nil)
    ('switch (switch-to-buffer buffer))
    ('bottom
     (display-buffer buffer
                     '((display-buffer-reuse-window display-buffer-at-bottom)
                       (window-height . 0.4)
                       (preserve-size . (nil . t)))))
    ('popup
     (display-buffer buffer
                     '((display-buffer-reuse-window
                        display-buffer-pop-up-window
                        display-buffer-below-selected)
                       (window-height . 0.4))))
    (_
     (display-buffer buffer
                     `((display-buffer-reuse-window display-buffer-in-side-window)
                       (side . right)
                       (slot . 1)
                       (window-width . ,exhub-gherkin-preview-width)
                       (preserve-size . (t . t)))))))

(defun exhub-gherkin--show (text name source-file source-buffer)
  "Render TEXT and show it; return the preview buffer."
  (let* ((feature (exhub-gherkin-parse text (or source-file name)))
         (buffer (get-buffer-create
                  (exhub-gherkin--buffer-name name source-file))))
    (exhub-gherkin--render-buffer buffer feature source-file source-buffer)
    (exhub-gherkin--display buffer)
    buffer))

;;;; Commands

(defun exhub-gherkin-preview (&optional buffer)
  "Preview BUFFER, or the current buffer, as Org.
The preview buffer is read-only; `g' refreshes it, `RET' jumps to the
source line and `q' dismisses it."
  (interactive)
  (let* ((source (or buffer (current-buffer)))
         (file (buffer-file-name source))
         (name (or (and file (file-name-nondirectory file))
                   (buffer-name source)))
         (text (with-current-buffer source
                 (buffer-substring-no-properties (point-min) (point-max)))))
    (exhub-gherkin--show text name file source)))

(defun exhub-gherkin-preview-file (file)
  "Preview the Gherkin FILE."
  (interactive "fGherkin feature file: ")
  (let ((file (expand-file-name file)))
    (unless (file-readable-p file)
      (user-error "Cannot read %s" file))
    (let ((text (with-temp-buffer
                  (insert-file-contents file)
                  (buffer-string))))
      (exhub-gherkin--show text (file-name-nondirectory file) file nil))))

(defun exhub-gherkin--refresh-preview ()
  "Re-render the current preview buffer from its source."
  (let* ((source exhub-gherkin--source-buffer)
         (file exhub-gherkin--source-file)
         (text (cond
                ((buffer-live-p source)
                 (with-current-buffer source
                   (buffer-substring-no-properties (point-min) (point-max))))
                ((and file (file-readable-p file))
                 (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string)))
                (t
                 (user-error "The Gherkin source is no longer available"))))
         (name (or (and file (file-name-nondirectory file))
                   (and (buffer-live-p source) (buffer-name source))
                   (buffer-name)))
         (feature (exhub-gherkin-parse text (or file name))))
    (exhub-gherkin--render-buffer (current-buffer) feature file source)))

(defun exhub-gherkin-preview-refresh ()
  "Re-render the current Gherkin preview from its source."
  (interactive)
  (unless (derived-mode-p 'exhub-gherkin-preview-mode)
    (user-error "Not in a Gherkin preview buffer"))
  (exhub-gherkin--refresh-preview)
  (message "Gherkin preview refreshed"))

(defun exhub-gherkin-goto-source ()
  "Jump to the `.feature' line that produced the preview text at point."
  (interactive)
  (let ((line (exhub-gherkin--source-line-at-point)))
    (unless line
      (user-error "No Gherkin source line at point"))
    (let ((source (and (buffer-live-p exhub-gherkin--source-buffer)
                       exhub-gherkin--source-buffer))
          (file exhub-gherkin--source-file))
      (cond
       (source
        (pop-to-buffer source)
        (exhub-gherkin--goto-line line))
       ((and file (file-readable-p file))
        (find-file-other-window file)
        (exhub-gherkin--goto-line line))
       (t
        (user-error "The Gherkin source is no longer available"))))))

(defun exhub-gherkin--source-line-at-point ()
  "Return the `exhub-gherkin-source-line' property at point, or nil."
  (let ((line (get-text-property (point) 'exhub-gherkin-source-line))
        (position (point))
        (end (line-end-position)))
    (while (and (not line) (< position end))
      (setq line (get-text-property position 'exhub-gherkin-source-line))
      (setq position (1+ position)))
    (or line
        (get-text-property (line-beginning-position)
                           'exhub-gherkin-source-line))))

(defun exhub-gherkin--goto-line (line)
  "Move to LINE in the current buffer."
  (goto-char (point-min))
  (forward-line (1- line)))

;;;; Minor mode for .feature buffers

(defvar exhub-gherkin-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-v") #'exhub-gherkin-preview)
    map)
  "Keymap for `exhub-gherkin-mode'.")

(define-minor-mode exhub-gherkin-mode
  "Minor mode for Gherkin `.feature' buffers.
It binds `C-c C-v' to `exhub-gherkin-preview' and, when
`exhub-gherkin-auto-refresh' is non-nil, refreshes open previews after
saving the buffer.
\\{exhub-gherkin-mode-map}"
  :lighter " Gherkin"
  :keymap exhub-gherkin-mode-map
  (if exhub-gherkin-mode
      (add-hook 'after-save-hook #'exhub-gherkin--after-save nil t)
    (remove-hook 'after-save-hook #'exhub-gherkin--after-save t)))

(defun exhub-gherkin--after-save ()
  "Refresh every preview rendered from the current buffer."
  (when exhub-gherkin-auto-refresh
    (let ((source (current-buffer)))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and (derived-mode-p 'exhub-gherkin-preview-mode)
                     (eq exhub-gherkin--source-buffer source))
            (ignore-errors (exhub-gherkin--refresh-preview))))))))

(provide 'exhub-gherkin)

;;; exhub-gherkin.el ends here
