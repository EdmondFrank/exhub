 ;;; exhub-translate.el --- Exhub translation package for Emacs  -*- lexical-binding:t; -*-

;; Copyright (C) 2023 EdmondFrank

;; Author: Edmond Frank <edmondfrank@hotmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (posframe "0.9"))
;; Keywords: convenience, translation
;; URL: https://github.com/edmondfrank/exhub

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

 ;;; Commentary:

;; This package provides translation functionality for Emacs using Exhub.

 ;;; Code:

(require 'json)
(require 'subr-x)
(require 'posframe)
(require 'exhub)

;;; Code:

;;;;;;;;;;;;;;;;;;;;; Customize options ;;;;;;;;;;;;;;;;;;;;;
(defgroup exhub-translate nil
  "Search and refacotry code base on ripgrep."
  :group 'exhub-translate)

(defface exhub-translate-font-lock-mark-word
  '((t (:foreground "White" :background "#007aff" :bold t)))
  "Face for keyword match."
  :group 'exhub-translate)

(defvar exhub-translate-origin-style-mode-list
  '(text-mode erc-mode rcirc-mode))

(defvar exhub-translate-line-style-mode-list
  '(web-mode emacs-lisp-mode inferior-emacs-lisp-mode css-mode))

(defvar exhub-translate-camel-style-mode-list
  '(js-mode go-mode))

(defvar exhub-translate-underline-style-mode-list
  '(ruby-mode))

(defvar exhub-translate-default-style "origin"
  "The default translation style, which can be set to \"origin\", \"line\", \"camel\" or \"underline\".")
;;;;;;;;;;;;;;;;;;;;; Interactive functions ;;;;;;;;;;;;;;;;;;;;;
(defun exhub-translate-insert (arg)
  (interactive "p")
  (if (or
       (equal arg 4)
       (and (boundp 'exhub-translate-original-translation)
            exhub-translate-original-translation)
       (exhub-translate-in-string-p)
       (exhub-translate-in-comment-p)
       (exhub-translate-in-commit-buffer-p)
       (minibuffer-window-active-p (get-buffer-window)))
      (exhub-translate-insert-original-translation)
    (exhub-translate-active
     (cond
      ((exhub-translate-match-modes exhub-translate-origin-style-mode-list)
       "origin")
      ((exhub-translate-match-modes exhub-translate-line-style-mode-list)
       "line")
      ((exhub-translate-match-modes exhub-translate-camel-style-mode-list)
       "camel")
      ((exhub-translate-match-modes exhub-translate-underline-style-mode-list)
       "underline")
      (t
       exhub-translate-default-style)))))

(defun exhub-translate-insert-original-translation ()
  (interactive)
  (exhub-translate-active "comment"))

(defun exhub-translate-insert-with-line ()
  (interactive)
  (exhub-translate-active "line"))

(defun exhub-translate-insert-with-underline ()
  (interactive)
  (exhub-translate-active "underline"))

(defun exhub-translate-insert-with-camel ()
  (interactive)
  (exhub-translate-active "camel"))

(defun exhub-translate-replace ()
  (interactive)
  (exhub-translate-replace-symbol
   (cond ((exhub-translate-match-modes exhub-translate-line-style-mode-list)
          "line")
         ((exhub-translate-match-modes exhub-translate-camel-style-mode-list)
          "camel")
         ((exhub-translate-match-modes exhub-translate-underline-style-mode-list)
          "underline")
         (t
          exhub-translate-default-style))))


(defun exhub-translate-posframe (tolang)
  (interactive "sTo: ")
  (let ((word (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'symbol))))
    (if (use-region-p)
        (kill-ring-save (region-beginning) (region-end))
      (kill-ring-save (beginning-of-thing 'symbol) (end-of-thing 'symbol)))
    (if (string-equal word "")
        (message "Nothing input, cancel translate.")
      (exhub-translate-retrieve-translation
       word exhub-translate-default-style (exhub-translate-generate-uuid) tolang "posframe"))))

(defun exhub-translate-show-translation-posframe (text)
  "Hide the translation posframe."
  (let ((placeholder (exhub-translate-generate-uuid)))
    ;; Query translation.
    (when (posframe-workable-p)
      (let* ((translation-posframe-buffer-name "*Translation Results*")
             (translation-posframe-buffer (get-buffer-create translation-posframe-buffer-name))
             (translation-posframe-content text))
        (posframe-show
         translation-posframe-buffer
         :string translation-posframe-content
         :position (point))
        (run-at-time "5 sec" nil #'exhub-translate-hide-translation-posframe translation-posframe-buffer)))))


(defun exhub-translate-hide-translation-posframe (translation-posframe-buffer)
  "Hide the translation posframe."
  (when (and translation-posframe-buffer
             (buffer-live-p translation-posframe-buffer))
    (posframe-delete translation-posframe-buffer)))

(defun exhub-translate-replace-with-line ()
  (interactive)
  (exhub-translate-replace-symbol "line"))

(defun exhub-translate-replace-with-underline ()
  (interactive)
  (exhub-translate-replace-symbol "underline"))

(defun exhub-translate-replace-with-camel ()
  (interactive)
  (exhub-translate-replace-symbol "camel"))

(defun exhub-translate-replace-zh ()
  "Translate and replace the selected region to Chinese."
  (interactive)
  (let ((region (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (thing-at-point 'symbol))))
    (if (use-region-p)
        (kill-region (region-beginning) (region-end))
      (kill-region (beginning-of-thing 'symbol) (end-of-thing 'symbol)))
    (exhub-translate-query-translation region "origin" "zh")))

;;;;;;;;;;;;;;;;;;;;; Helper functions ;;;;;;;;;;;;;;;;;;;;;
(defun exhub-translate-replace-symbol (style)
  (let ((word (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'symbol))))
    (if (use-region-p)
        (kill-region (region-beginning) (region-end))
      (kill-region (beginning-of-thing 'symbol) (end-of-thing 'symbol)))
    (exhub-translate-query-translation word style "EN")))

(defun exhub-translate-match-modes (mode-list)
  (cl-remove-if 'null (mapcar #'(lambda (mode) (derived-mode-p mode)) mode-list)))

(defun exhub-translate-use-original-translation ()
  (set (make-local-variable 'exhub-translate-original-translation) t))

(defun exhub-translate-active (style)
  ;; Enable input method if user has load it.
  (activate-input-method default-input-method)

  ;; Add monitor hook.
  (add-hook 'after-change-functions 'exhub-translate-monitor-after-change nil t)

  ;; Make sure build hash to contain placeholder.
  (unless (boundp 'exhub-translate-placeholder-hash)
    (set (make-local-variable 'exhub-translate-placeholder-hash) (make-hash-table :test 'equal)))

  ;; Make sure clean active overlay first.
  (when (and (boundp 'exhub-translate-active-overlay)
             exhub-translate-active-overlay)
    (delete-overlay exhub-translate-active-overlay))

  ;; Reset active local variables
  (set (make-local-variable 'exhub-translate-active-point) (point))
  (set (make-local-variable 'exhub-translate-active-style) style)
  (set (make-local-variable 'exhub-translate-active-overlay) (make-overlay (point) (point)))

  ;; Active new overlay from current point.
  (overlay-put exhub-translate-active-overlay 'face 'exhub-translate-font-lock-mark-word)

  ;; Print play hint.
  (unless (minibuffer-window-active-p (get-buffer-window))
    (message "Type Chinese and press SPACE to translate.")))

(defun exhub-translate-inactive (&optional keep-style)
  (interactive)
  ;; Disable input method if user has load it.
  (deactivate-input-method)

  ;; Delete active overlay.
  (when (and (boundp 'exhub-translate-active-overlay)
             exhub-translate-active-overlay)
    (delete-overlay exhub-translate-active-overlay))

  ;; Clean active local variables.
  (set (make-local-variable 'exhub-translate-active-point) nil)
  (when (and (boundp 'exhub-translate-active-overlay)
             exhub-translate-active-overlay)
    (set (make-local-variable 'exhub-translate-active-overlay) nil))

  ;; Clean style.
  (unless keep-style
    (set (make-local-variable 'exhub-translate-active-style) nil)))

(defun exhub-translate-monitor-after-change (start end len)
  (when (and (boundp 'exhub-translate-active-point))
    (if exhub-translate-active-point
        (let ((translate-start exhub-translate-active-point)
              (translate-end (point)))
          (cond
           ;; Translate current Chinese words after press SPACE.
           ((string-equal (buffer-substring-no-properties start end) " ")
            (let ((word (buffer-substring-no-properties translate-start translate-end)))
              ;; Delete Chinese words.
              (kill-region translate-start translate-end)

              ;; Query translation.
              (exhub-translate-query-translation word exhub-translate-active-style "EN")

              ;; Inactive.
              (exhub-translate-inactive nil)))
           ;; Update active overlay bound if user press any other non-SPACE character.
           (t
            (move-overlay exhub-translate-active-overlay translate-start translate-end)))))))

(defun exhub-translate-current-parse-state ()
  "Return parse state of point from beginning of defun."
  (let ((point (point)))
    (beginning-of-defun)
    (when (equal point (point))
      (beginning-of-line))
    (parse-partial-sexp (point) point)))

(defun exhub-translate-in-commit-buffer-p ()
  (and (string-equal (buffer-name) "COMMIT_EDITMSG")
       (save-excursion
         (goto-char (point-min))
         (search-forward-regexp "#\\s-Please\\s-enter\\s-the\\s-commit\\s-message\\s-for\\s-your\\s-changes." nil t))))

(defun exhub-translate-in-string-p (&optional state)
  (or (nth 3 (or state (exhub-translate-current-parse-state)))
      (and
       (eq (get-text-property (point) 'face) 'font-lock-string-face)
       (eq (get-text-property (- (point) 1) 'face) 'font-lock-string-face))
      (and
       (eq (get-text-property (point) 'face) 'font-lock-doc-face)
       (eq (get-text-property (- (point) 1) 'face) 'font-lock-doc-face))
      ))

(defun exhub-translate-in-comment-p (&optional state)
  (or (nth 4 (or state (exhub-translate-current-parse-state)))
      (eq (get-text-property (point) 'face) 'font-lock-comment-face)))

(defun exhub-translate-convert-translation (translation style)
  (let ((words (split-string translation " ")))
    (cond ((string-equal style "line")
           (string-join (mapcar 'downcase words) "-"))
          ((string-equal style "underline")
           (string-join (mapcar 'downcase words) "_"))
          ((string-equal style "camel")
           (concat (downcase (car words)) (string-join (mapcar 'capitalize (cdr words)))))
          ((or
            (string-equal style "comment")
            (string-equal style "origin"))
           translation))))

(defun exhub-translate-update-translation-in-buffer (word style translation insert-buffer placeholder)
  (let ((result (exhub-translate-convert-translation translation style)))
    (save-excursion
      (with-current-buffer insert-buffer
        (let ((placeholder-point (gethash placeholder exhub-translate-placeholder-hash)))
          (if placeholder-point
              (progn
                ;; Insert result at placeholder point .
                (goto-char placeholder-point)
                (insert result)

                ;; Remove placeholder from hash.
                (remhash placeholder exhub-translate-placeholder-hash))
            (message (format "Something wrong that we can't found placeholder for %s: %s" word translation))))))))

(defun exhub-translate-generate-uuid ()
  "Generate a 32 character UUID."
  (md5 (number-to-string (float-time))))

(defun exhub-translate-query-translation (word style lang)
  (if (string-equal word "")
      (message "Nothing input, cancel translate.")
    (let ((placeholder (exhub-translate-generate-uuid)))
      ;; Store placeholder in hash.
      ;; bug: `exhub-translate-placeholder-hash' is not initialized
      ;; thus I add such fix

      (unless (boundp 'exhub-translate-placeholder-hash)
        (set (make-local-variable 'exhub-translate-placeholder-hash) (make-hash-table :test 'equal)))

      (puthash placeholder (point) exhub-translate-placeholder-hash)

      ;; Query translation.
      (exhub-translate-retrieve-translation word style placeholder lang "replace")
      )))

(defun exhub-translate-retrieve-translation (word style placeholder lang action)
  (exhub-call "exhub-translate" word style (buffer-name) placeholder lang action))

(provide 'exhub-translate)

 ;;; exhub-translate.el ends here
