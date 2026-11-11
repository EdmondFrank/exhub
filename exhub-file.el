;;; exhub-file.el --- Exhub File integration for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2023 EdmondFrank

;;; Commentary:

;; This package provides integration with Exhub File using Exhub.
;;
;; `exhub-preview-markdown' renders Markdown with Exhub and shows the
;; result near point.  ```mermaid fences are turned into SVG images
;; server-side, so diagrams display without any client-side JavaScript.
;;
;; The preview renders the active region when there is one, and the whole
;; buffer otherwise.

;;; Code:

(require 'exhub)
(require 'subr-x)

(declare-function posframe-delete "posframe" (buffer-or-name))
(declare-function posframe-show "posframe" (&rest args))
(declare-function posframe-workable-p "posframe" ())
(declare-function shr-render-region "shr" (start end &optional fontify))
(defvar markdown-mode-map)

(defgroup exhub-file nil
  "Exhub File group.
This group provides functions to interact with Exhub File API.
The functions in this group are used to perform various file operations
such as reading, writing, and deleting files using the Exhub File API."
  :group 'applications)

(defcustom exhub-markdown-preview-use-posframe t
  "Whether `exhub-preview-markdown' shows the preview in a posframe.
When nil, or when posframe cannot work (terminal Emacs), the preview
opens in a dedicated EWW buffer instead."
  :type 'boolean
  :group 'exhub-file)

(defcustom exhub-markdown-preview-posframe-width 84
  "Width of the Markdown preview posframe, in characters.
Markdown is re-wrapped to this width before rendering so text and
Mermaid diagrams stay inside the frame."
  :type 'integer
  :group 'exhub-file)

(defcustom exhub-markdown-preview-posframe-max-height 0.7
  "Maximum height of the Markdown preview posframe.
A float is a ratio of the selected frame's height, an integer is a
number of lines."
  :type '(choice (float :tag "Ratio of frame height")
                 (integer :tag "Lines"))
  :group 'exhub-file)

(defconst exhub-markdown-preview-buffer-name "*exhub-markdown-preview*"
  "Buffer holding the rendered Markdown shown in the preview posframe.")

(defvar exhub-markdown-preview-posframe-buffer nil
  "Buffer currently shown in the Markdown preview posframe, or nil.")

(defun exhub-preview-markdown ()
  "Preview the current buffer's Markdown, or the active region, using Exhub.
The region takes precedence over the whole buffer.  The rendered preview
is shown in a posframe near point when possible; with a prefix argument,
or where posframe cannot work, it opens in a dedicated EWW buffer
instead."
  (interactive)
  (let ((content (exhub-markdown-preview--content)))
    (if (string-empty-p (string-trim content))
        (message "Nothing to preview: the %s is empty."
                 (if (use-region-p) "region" "buffer"))
      (exhub-file-call "markdown-render"
                       (if (or current-prefix-arg
                               (not (exhub-markdown-preview-posframe-workable-p)))
                           "exhub-file-show-with-eww-casual"
                         "exhub-file-show-with-posframe")
                       content (buffer-name)))))

(defun exhub-preview-markdown-in-eww ()
  "Preview the buffer or active region Markdown in a dedicated EWW buffer.
Same as `exhub-preview-markdown' with a prefix argument."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (exhub-preview-markdown)))

;; `C-c C-v' is free in `markdown-mode-map'; gfm-mode and the other derived
;; modes inherit it.
(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-v") #'exhub-preview-markdown-in-eww))

(defun exhub-markdown-preview--content ()
  "Return the Markdown source to preview.
The active region wins over the whole buffer, so a snippet can be
previewed without copying it elsewhere."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun exhub-markdown-preview-posframe-workable-p ()
  "Return non-nil when the Markdown preview can use a posframe."
  (and exhub-markdown-preview-use-posframe
       (require 'posframe nil t)
       (posframe-workable-p)))

(defun exhub-markdown-preview--posframe-max-height ()
  "Return the preview posframe height limit, in lines."
  (max 3
       (if (floatp exhub-markdown-preview-posframe-max-height)
           (truncate (* (frame-height (selected-frame))
                        exhub-markdown-preview-posframe-max-height))
         exhub-markdown-preview-posframe-max-height)))

(defun exhub-markdown-preview--image-limits ()
  "Return the (MAX-WIDTH . MAX-HEIGHT) pixel limits for preview images."
  (let ((frame (selected-frame))
        (max-height (exhub-markdown-preview--posframe-max-height)))
    (cons (* exhub-markdown-preview-posframe-width
             (max 1 (frame-char-width frame)))
          (* max-height (max 1 (frame-char-height frame))))))

(defun exhub-markdown-preview--clamp-images (max-width max-height)
  "Shrink images in the current buffer to fit MAX-WIDTH and MAX-HEIGHT pixels.
`shr-rescale-image' only scales images when their buffer is displayed in
a window, so a buffer rendered ahead of the posframe would otherwise keep
the natural — often very wide — SVG size of a Mermaid diagram."
  (let ((pos (point-min)))
    (while (< pos (point-max))
      (let* ((next (or (next-single-property-change pos 'display) (point-max)))
             (display (get-text-property pos 'display))
             (data (and (eq (car-safe display) 'image)
                        (plist-get (cdr display) :data))))
        (when (stringp data)
          (let ((resized (ignore-errors
                           (create-image data nil t
                                         :ascent 100
                                         :max-width max-width
                                         :max-height max-height))))
            (when resized
              (put-text-property pos next 'display (cons 'image resized)))))
        (setq pos next)))))

(defun exhub-markdown-preview--line-metrics ()
  "Return (MAX-LINE-COLS . TOTAL-LINES) for the current rendered buffer.
Images contribute their pixel size converted to the frame's character
grid, so a Mermaid diagram gets the rows and columns it needs instead of
being clipped by a frame sized for text alone."
  (let ((char-width (max 1 (frame-char-width (selected-frame))))
        (char-height (max 1 (frame-char-height (selected-frame))))
        (max-cols 0)
        (lines 0))
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((beg (line-beginning-position))
             (end (line-end-position))
             (image-cols 0)
             (image-lines 1)
             (pos beg))
        (while (< pos end)
          (let ((next (or (next-single-property-change pos 'display nil end) end))
                (display (get-text-property pos 'display)))
            (when (eq (car-safe display) 'image)
              (let ((size (ignore-errors (image-size (cdr display) t))))
                (when size
                  (setq image-cols (max image-cols
                                        (ceiling (/ (float (car size)) char-width))))
                  (setq image-lines (max image-lines
                                         (ceiling (/ (float (cdr size)) char-height)))))))
            (setq pos next)))
        (setq max-cols (max max-cols
                            (string-width (buffer-substring-no-properties beg end))
                            image-cols))
        (setq lines (+ lines image-lines)))
      (forward-line 1))
    (cons max-cols (max 1 lines))))

(defun exhub-markdown-preview--render (html-content)
  "Render HTML-CONTENT into the preview buffer and return that buffer.
The text is re-wrapped to `exhub-markdown-preview-posframe-width' and
images are clamped to the same box, because shr measures images against
the selected window rather than the posframe."
  (require 'shr)
  (let ((buffer (get-buffer-create exhub-markdown-preview-buffer-name))
        (limits (exhub-markdown-preview--image-limits)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert html-content)
      (goto-char (point-min))
      (let ((shr-width (max 20 (- exhub-markdown-preview-posframe-width 4))))
        (shr-render-region (point-min) (point-max)))
      (exhub-markdown-preview--clamp-images (car limits) (cdr limits))
      (goto-char (point-min)))
    buffer))

(defun exhub-file-show-with-posframe (html-content buffer-name)
  "Show rendered HTML-CONTENT from BUFFER-NAME in a transient posframe.
The posframe is dismissed by the next key press.  Falls back to
`exhub-file-show-with-eww-casual' where posframe cannot work."
  (if (not (exhub-markdown-preview-posframe-workable-p))
      (exhub-file-show-with-eww-casual html-content buffer-name)
    (remove-hook 'pre-command-hook #'exhub-markdown-preview-quit-on-key)
    (exhub-markdown-preview-hide-posframe)
    (let* ((buffer (exhub-markdown-preview--render html-content))
           (max-width exhub-markdown-preview-posframe-width)
           (max-height (exhub-markdown-preview--posframe-max-height))
           (metrics (with-current-buffer buffer
                      (exhub-markdown-preview--line-metrics)))
           ;; A frame sized from the content itself: posframe's own fitting
           ;; under-measures images, which would clip Mermaid diagrams.
           (width (min max-width (max 20 (+ 2 (car metrics)))))
           (height (min max-height (max 3 (cdr metrics)))))
      (setq exhub-markdown-preview-posframe-buffer buffer)
      (posframe-show buffer
                     :position (point)
                     :width width
                     :height height
                     :internal-border-width 1
                     :lines-truncate t)
      (add-hook 'pre-command-hook #'exhub-markdown-preview-quit-on-key))))

(defun exhub-markdown-preview-quit-on-key ()
  "Hide the Markdown preview posframe before the next command runs."
  (remove-hook 'pre-command-hook #'exhub-markdown-preview-quit-on-key)
  (exhub-markdown-preview-hide-posframe))

(defun exhub-markdown-preview-hide-posframe ()
  "Hide the Markdown preview posframe, if one is showing."
  (when (buffer-live-p exhub-markdown-preview-posframe-buffer)
    (posframe-delete exhub-markdown-preview-posframe-buffer))
  (setq exhub-markdown-preview-posframe-buffer nil))

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
  (require 'shr)
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