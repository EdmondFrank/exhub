;;; exhub-fim-menu.el --- Dropdown candidate menu for exhub-fim -*- lexical-binding: t; -*-

;;; Commentary:
;; A dropdown candidate menu for Exhub-fim completions, modelled on the
;; completion menu of lsp-bridge (its `acm-frame.el' / `acm.el').  Like acm,
;; the menu is a borderless child frame that is attached to the frame that
;; displayed it and never takes the input focus, so the cursor stays in the
;; code buffer while candidates are browsed.
;;
;; Candidate state (list, selected index, scrolled offset) lives in the
;; buffer the menu was requested from.  Navigation, acceptance and mouse
;; selection are driven by `exhub-fim-menu-mode', a lightweight minor mode
;; that is enabled while the menu is visible.
;;
;; The menu is generic: it knows nothing about completion providers.  It
;; announces selection changes and acceptance through
;; `exhub-fim-menu-selection-hook' and `exhub-fim-menu-accept-hook', both run
;; in the buffer owning the menu.

;;; Code:

(require 'cl-lib)
(require 'frame) ;; `fit-frame-to-buffer'

(defgroup exhub-fim-menu nil
  "Dropdown candidate menu for Exhub-fim."
  :group 'exhub-fim)

(defcustom exhub-fim-menu-max-items 10
  "Maximum number of candidates displayed at once in the dropdown.
When there are more candidates the menu scrolls as the selection
moves, like the completion menu of lsp-bridge."
  :type 'integer)

(defcustom exhub-fim-menu-max-width 60
  "Maximum display width, in characters, of a candidate in the dropdown.
Longer candidates are truncated with an ellipsis; the full text is
still inserted when the candidate is accepted."
  :type 'integer)

(defface exhub-fim-menu-default-face
  '()
  "Default face, foreground and background colors used for the dropdown.")

(defface exhub-fim-menu-border-face
  '((((class color) (min-colors 88) (background dark)) :background "#323232")
    (((class color) (min-colors 88) (background light)) :background "#d7d7d7")
    (t :background "gray"))
  "Background color used for the thin border of the dropdown.")

(defface exhub-fim-menu-select-face
  '()
  "Face used to highlight the currently selected candidate.")

(defface exhub-fim-menu-annotation-face
  '((t :inherit font-lock-doc-face))
  "Face used for the annotation on the right of each candidate.")

(defvar exhub-fim-menu-frame nil
  "Child frame displaying the dropdown.")

(defvar exhub-fim-menu-buffer " *exhub-fim-menu*"
  "Buffer displayed in `exhub-fim-menu-frame'.")

(defvar exhub-fim-menu--emacs-frame nil
  "Frame the dropdown was created for.")

(defvar exhub-fim-menu--source-buffer nil
  "Buffer the currently displayed dropdown belongs to.")

(defvar exhub-fim-menu--popup-point nil
  "Point the dropdown was popped up at.")

(defvar exhub-fim-menu--popup-position nil
  "Pixel position (x . y) the dropdown was popped up at.")

(defvar exhub-fim-menu--max-length-cache 0
  "Pixel width of the previously rendered menu, to avoid resizing.")

(defvar exhub-fim-menu--number-cache 0
  "Number of previously rendered candidates, to avoid resizing.")

(defvar-local exhub-fim-menu-candidates nil
  "Candidates currently offered by the dropdown of this buffer.")

(defvar-local exhub-fim-menu-index 0
  "Index of the selected candidate in `exhub-fim-menu-candidates'.")

(defvar-local exhub-fim-menu-offset 0
  "Index of the first candidate displayed in the dropdown.")

(defvar exhub-fim-menu-selection-hook nil
  "Hook run in the buffer owning the dropdown after the selection changed.
The selected candidate is available through
`exhub-fim-menu-current-candidate'.")

(defvar exhub-fim-menu-accept-hook nil
  "Hook run in the buffer owning the dropdown when a candidate is accepted.
The menu is still visible while the hook runs; the buffer owning the
menu is responsible for hiding it, see `exhub-fim-menu-hide'.")

(defvar exhub-fim-menu-cancel-hook nil
  "Hook run in the buffer owning the dropdown when it is dismissed.
The menu hides itself afterwards.")

(defvar exhub-fim-menu--mouse-ignore-map
  (let ((map (make-sparse-keymap)))
    (dotimes (i 7)
      (dolist (k '(mouse down-mouse drag-mouse double-mouse triple-mouse))
        (define-key map (vector (intern (format "%s-%s" k (1+ i)))) #'ignore)))
    map)
  "Ignore all mouse clicks in the menu buffer.
Candidate lines override this with their own `mouse-1' binding.")

(defvar exhub-fim-menu--line-map
  (let ((map (make-sparse-keymap)))
    ;; Accept when the button goes down, and swallow the release event so a
    ;; single click cannot accept twice.
    (define-key map [down-mouse-1] #'exhub-fim-menu-mouse-accept)
    (define-key map [mouse-1] #'ignore)
    (define-key map [mouse-2] #'ignore)
    map)
  "Keymap installed on candidate lines, mapping clicks to acceptance.")

(defvar exhub-fim-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") #'exhub-fim-menu-select-next)
    (define-key map (kbd "M-p") #'exhub-fim-menu-select-previous)
    (define-key map [down] #'exhub-fim-menu-select-next)
    (define-key map [up] #'exhub-fim-menu-select-previous)
    (define-key map (kbd "M->") #'exhub-fim-menu-select-last)
    (define-key map (kbd "M-<") #'exhub-fim-menu-select-first)
    (define-key map (kbd "C-v") #'exhub-fim-menu-next-page)
    (define-key map (kbd "M-v") #'exhub-fim-menu-previous-page)
    (define-key map (kbd "<tab>") #'exhub-fim-menu-accept)
    (define-key map (kbd "M-<return>") #'exhub-fim-menu-accept)
    (define-key map (kbd "M-l") #'exhub-fim-menu-cancel)
    (define-key map (kbd "C-g") #'exhub-fim-menu-cancel)
    map)
  "Keymap active while the dropdown is visible.")

(define-minor-mode exhub-fim-menu-mode
  "Minor mode active while the Exhub-fim dropdown is displayed."
  :init-value nil
  :keymap exhub-fim-menu-mode-map
  :lighter nil)

;;;; Frame plumbing, adapted from lsp-bridge's acm-frame.el.

(defun exhub-fim-menu--border-face ()
  "Face used to draw the border of the child frame."
  (if (facep 'child-frame-border) 'child-frame-border 'internal-border))

(defun exhub-fim-menu--set-frame-colors (frame)
  "Set border and background color of FRAME from the menu faces."
  (let ((face (exhub-fim-menu--border-face))
        (border (face-attribute 'exhub-fim-menu-border-face :background nil t)))
    (unless (equal (face-attribute face :background frame t) border)
      (set-face-background face border frame)))
  (let ((background (face-attribute 'exhub-fim-menu-default-face :background nil t)))
    (unless (equal (frame-parameter frame 'background-color) background)
      (set-frame-parameter frame 'background-color background))))

(defun exhub-fim-menu--make-frame (frame-name)
  "Create the borderless child frame named FRAME-NAME.
The frame is attached to the selected frame and never takes focus, so
typing keeps going into the buffer the menu was requested from."
  (let* ((after-make-frame-functions nil)
         (parent (selected-frame))
         (parent-font (with-selected-frame parent (face-attribute 'default :font)))
         frame)
    (setq frame
          (make-frame
           `((name . ,frame-name)
             (parent-frame . ,parent)
             (no-accept-focus . t)
             (no-focus-on-map . t)
             (minibuffer . nil)
             (min-width . t)
             (min-height . t)
             (width . 0)
             (height . 0)
             (border-width . 0)
             (internal-border-width . 1)
             (child-frame-border-width . 1)
             (left-fringe . 0)
             (right-fringe . 0)
             (vertical-scroll-bars . nil)
             (horizontal-scroll-bars . nil)
             (menu-bar-lines . 0)
             (tool-bar-lines . 0)
             (tab-bar-lines . 0)
             (no-other-frame . t)
             (no-other-window . t)
             (no-delete-other-windows . t)
             (unsplittable . t)
             (undecorated . t)
             (cursor-type . nil)
             (visibility . nil)
             (no-special-glyphs . t)
             (desktop-dont-save . t)
             (mode-line-format . nil))))
    ;; Keep the menu's font in sync with the frame it belongs to.
    (with-selected-frame frame
      (set-frame-font parent-font))
    (exhub-fim-menu--set-frame-colors frame)
    (setq exhub-fim-menu--emacs-frame parent)
    (redirect-frame-focus frame parent)
    frame))

(defun exhub-fim-menu--create-frame-if-needed ()
  "Create the dropdown frame and configure the menu buffer."
  (unless (frame-live-p exhub-fim-menu-frame)
    (setq exhub-fim-menu-frame (exhub-fim-menu--make-frame "exhub-fim menu frame"))
    (with-current-buffer (get-buffer-create exhub-fim-menu-buffer)
      (use-local-map exhub-fim-menu--mouse-ignore-map)
      (dolist (var '((mode-line-format . nil)
                     (header-line-format . nil)
                     (tab-line-format . nil)
                     (tab-bar-format . nil)
                     (frame-title-format . "")
                     (truncate-lines . t)
                     (cursor-in-non-selected-windows . nil)
                     (cursor-type . nil)
                     (show-trailing-whitespace . nil)
                     (display-line-numbers . nil)
                     (left-fringe-width . nil)
                     (right-fringe-width . nil)
                     (left-margin-width . 0)
                     (right-margin-width . 0)
                     (fringes-outside-margins . 0)))
        (set (make-local-variable (car var)) (cdr var))))
    (let ((win (frame-root-window exhub-fim-menu-frame)))
      (set-window-buffer win (get-buffer-create exhub-fim-menu-buffer))
      ;; Mark the window dedicated so the frame is never reused.
      (set-window-dedicated-p win t))))

(defun exhub-fim-menu--fit-frame (frame)
  "Resize FRAME to the smallest size that fits its content."
  (let ((window-min-height 0)
        (window-min-width 0))
    (fit-frame-to-buffer frame)))

(defun exhub-fim-menu--set-frame-position (frame x y)
  "Move FRAME to pixel position (X, Y), making it visible first."
  (unless (frame-visible-p frame)
    ;; Force a redisplay, otherwise the popup sometimes shows no content.
    (redisplay 'force)
    (make-frame-visible frame))
  (set-frame-position frame x y))

(defun exhub-fim-menu--popup-position (point &optional line-bias)
  "Return the pixel position below which the dropdown pops up at POINT.
LINE-BIAS moves the position down by that many lines, used to step past
the ghost preview of a multi-line candidate."
  (let* ((edges (window-pixel-edges))
         (window-left (+ (nth 0 edges)
                         (/ (- (window-pixel-width) (window-body-width nil t)) 2)))
         (window-top (nth 1 edges))
         (pos (posn-x-y (posn-at-point point)))
         (line-height (line-pixel-height))
         (offset-y (if (version< emacs-version "27.0")
                       (window-header-line-height)
                     (+ (window-tab-line-height) (window-header-line-height)))))
    (cons (+ (car pos) window-left)
          (+ (cdr pos) window-top offset-y line-height
             (* (or line-bias 0) line-height)))))

(defun exhub-fim-menu--adjust-frame-pos (frame &optional margin)
  "Keep FRAME on screen, MARGIN pixels away from the frame edges."
  (let* ((margin (or margin 50))
         (emacs-frame (or exhub-fim-menu--emacs-frame (selected-frame)))
         (main-x (car (frame-position emacs-frame)))
         (main-y (cdr (frame-position emacs-frame)))
         (main-width (frame-pixel-width emacs-frame))
         (main-height (frame-pixel-height emacs-frame))
         (frame-x (car (frame-position frame)))
         (frame-y (cdr (frame-position frame)))
         (frame-width (frame-pixel-width frame))
         (frame-height (frame-pixel-height frame)))
    (when (> (+ frame-x frame-width) (- (+ main-x main-width) margin))
      (set-frame-position frame (- (+ main-x main-width) frame-width margin) frame-y))
    ;; Flip the menu above the cursor when there is not enough room below.
    (when (> (+ frame-y frame-height) (- (+ main-y main-height) margin))
      (set-frame-position
       frame frame-x
       (if (> (- (cdr exhub-fim-menu--popup-position) main-y)
              (+ frame-height (* 2 (line-pixel-height))))
           (- (cdr exhub-fim-menu--popup-position) frame-height (* 2 (line-pixel-height)))
         (- (+ main-y main-height) frame-height margin))))))

(defun exhub-fim-menu-color-blend (c1 c2 alpha)
  "Blend color C1 into C2 with weight ALPHA."
  (apply (lambda (r g b) (format "#%02x%02x%02x" (ash r -8) (ash g -8) (ash b -8)))
         (cl-mapcar (lambda (x y) (round (+ (* x alpha) (* y (- 1 alpha)))))
                    (color-values c1) (color-values c2))))

(defun exhub-fim-menu-init-colors (&optional force)
  "Derive the menu colors from the theme, overriding faces when FORCE."
  (let* ((dark (eq (frame-parameter nil 'background-mode) 'dark))
         (blend (if dark "#000000" "#AAAAAA"))
         (default-background
          (if (or force (equal (face-attribute 'exhub-fim-menu-default-face :background)
                               'unspecified))
              (face-attribute 'default :background)
            (face-attribute 'exhub-fim-menu-default-face :background))))
    (unless (ignore-errors (exhub-fim-menu-color-blend default-background blend 0.6))
      (setq default-background (if dark "#000000" "#AAAAAA")))
    (when (or force (equal (face-attribute 'exhub-fim-menu-default-face :background)
                           'unspecified))
      (set-face-background 'exhub-fim-menu-default-face
                           (exhub-fim-menu-color-blend default-background blend
                                                       (if dark 0.8 0.9))))
    (when (or force (equal (face-attribute 'exhub-fim-menu-select-face :background)
                           'unspecified))
      (set-face-background 'exhub-fim-menu-select-face
                           (exhub-fim-menu-color-blend default-background blend 0.6)))
    (when (or force (equal (face-attribute 'exhub-fim-menu-select-face :foreground)
                           'unspecified))
      (set-face-foreground 'exhub-fim-menu-select-face
                           (face-attribute 'font-lock-function-name-face :foreground)))))

;;;; Rendering.
;;;; Rendering.

(defsubst exhub-fim-menu--indent-pixel (xpos)
  "Return a display property aligning a candidate to XPOS pixels."
  `(space :align-to (,xpos)))

(defun exhub-fim-menu--width (string)
  "Pixel width of STRING."
  (if (fboundp 'string-pixel-width)
      (string-pixel-width string)
    (* (string-width string) (frame-char-width))))

(defun exhub-fim-menu--blank-line-p (line)
  "Return non-nil when LINE holds nothing but whitespace."
  (string-match-p "\\`[ \t\r]*\\'" line))

(defun exhub-fim-menu--display-label (candidate)
  "Return the one line label displayed for completion CANDIDATE.
Candidates that start with a newline are labelled with their first
non-empty line, prefixed by an enter symbol, so the menu never shows a
blank row.  A cross-buffer word candidate is labelled with the whole word
rather than the tail inserted after point, as lsp-bridge's menu labels its
search-word candidates."
  (let* ((candidate (or (get-text-property 0 'exhub-fim-word candidate)
                        candidate))
         (lines (split-string candidate "\n"))
         (label (or (car lines) ""))
         (blank (exhub-fim-menu--blank-line-p label))
         (multi (> (length lines) 1)))
    (when (and blank multi)
      (setq label (or (seq-find (lambda (l) (not (exhub-fim-menu--blank-line-p l)))
                                (cdr lines))
                      "")))
    (setq label (replace-regexp-in-string "[\t\r]+" " " label))
    (when blank
      (setq label (concat "↵" (string-trim-left label))))
    (truncate-string-to-width label exhub-fim-menu-max-width nil nil "…")))

(defun exhub-fim-menu--annotation (candidate)
  "Return the annotation displayed for completion CANDIDATE.
A cross-buffer word candidate is annotated with where it came from, the way
lsp-bridge's menu reads \"Search Word\"; other candidates show their line
count."
  (or (get-text-property 0 'exhub-fim-annotation candidate)
      (let ((lines (length (split-string candidate "\n" t))))
        (if (> lines 1) (format "%d lines" lines) ""))))

(defun exhub-fim-menu--build-items ()
  "Return the visible window of candidates as display items.
Each item is (INDEX LABEL ANNOTATION LABEL-WIDTH ANNOTATION-WIDTH),
where INDEX is the absolute position in `exhub-fim-menu-candidates'."
  (let ((window (cl-subseq exhub-fim-menu-candidates
                           exhub-fim-menu-offset
                           (min (length exhub-fim-menu-candidates)
                                (+ exhub-fim-menu-offset exhub-fim-menu-max-items))))
        (items nil)
        (index exhub-fim-menu-offset))
    (dolist (candidate window (nreverse items))
      (let ((label (exhub-fim-menu--display-label candidate))
            (annotation (exhub-fim-menu--annotation candidate)))
        (push (list index label annotation
                    (exhub-fim-menu--width label)
                    (exhub-fim-menu--width annotation))
              items)
        (setq index (1+ index))))))

(defun exhub-fim-menu--row-width (item)
  "Pixel width of the label plus annotation of ITEM."
  (+ (nth 3 item) (nth 4 item)))

(defun exhub-fim-menu--render-items (items max-width selected-index)
  "Insert display ITEMS of width MAX-WIDTH into the menu buffer.
Mark the item whose absolute position in `exhub-fim-menu-candidates'
equals SELECTED-INDEX as the selected one.  Annotations are right
aligned at MAX-WIDTH, as in lsp-bridge's menu, so the whole list stays
readable.  Candidate state is buffer-local to the buffer owning the
menu, so it cannot be read from here, the menu buffer."
  (let* ((gutter-width (* 4 (frame-char-width)))
         (show-annotations (cl-some (lambda (item) (not (string-empty-p (nth 2 item))))
                                    items)))
    (dolist (item items)
      (let* ((index (nth 0 item))
             (label (nth 1 item))
             (annotation (nth 2 item))
             (selected (equal index selected-index))
             (line
              (concat
               (propertize (format "%2d " (1+ index))
                           'face (if selected 'exhub-fim-menu-select-face 'shadow))
               label
               (when show-annotations
                 (propertize
                  " " 'display
                  (exhub-fim-menu--indent-pixel
                   (+ gutter-width (- max-width (nth 4 item))))))
               (when show-annotations
                 (propertize (concat " " annotation)
                             'face (if selected 'exhub-fim-menu-select-face
                                     'exhub-fim-menu-annotation-face)))
               "\n")))
        (when selected
          (add-face-text-property 0 (length line) 'exhub-fim-menu-select-face
                                  'append line))
        ;; Make the whole row clickable, not only the label.
        (let ((start (point)))
          (insert line)
          (put-text-property start (point) 'mouse-face 'exhub-fim-menu-select-face)
          (put-text-property start (point) 'exhub-fim-menu-index index)
          (put-text-property start (point) 'keymap exhub-fim-menu--line-map)
          (put-text-property start (point) 'help-echo
                             "mouse-1: accept this completion"))))
    ;; Drop the trailing newline so the frame hugs the last line.
    (goto-char (point-max))
    (delete-char -1)))

(defun exhub-fim-menu--render ()
  "Render the candidates into the menu buffer, resizing the frame if needed.
Called from the buffer owning the menu: read the candidate state here,
before switching to the menu buffer, where the buffer-local variables
hold their default values."
  (let* ((items (exhub-fim-menu--build-items))
         (selected-index exhub-fim-menu-index)
         (max-width (if items (cl-reduce #'max items :key #'exhub-fim-menu--row-width) 0))
         (resize-p (or (/= exhub-fim-menu--max-length-cache max-width)
                       (/= exhub-fim-menu--number-cache (length items)))))
    (setq exhub-fim-menu--max-length-cache max-width
          exhub-fim-menu--number-cache (length items))
    (with-current-buffer (get-buffer-create exhub-fim-menu-buffer)
      (erase-buffer)
      (if items
          (exhub-fim-menu--render-items items max-width selected-index)
        (insert " ")))
    (when resize-p
      (exhub-fim-menu--fit-frame exhub-fim-menu-frame))
    (exhub-fim-menu--adjust-frame-pos exhub-fim-menu-frame)))

;;;; Public API.

(defun exhub-fim-menu-can-display-p ()
  "Return non-nil when a child frame dropdown can be displayed."
  (and (not (or noninteractive emacs-basic-display))
       (display-graphic-p)
       ;; Child frames need a window system able to parent frames.
       (not (memq window-system '(tty term)))))

(defun exhub-fim-menu-visible-p ()
  "Return non-nil when the dropdown is currently displayed."
  (and (frame-live-p exhub-fim-menu-frame)
       (frame-visible-p exhub-fim-menu-frame)))

(defun exhub-fim-menu-current-candidate ()
  "Return the selected candidate, or nil when no menu is displayed."
  (when exhub-fim-menu-candidates
    (nth exhub-fim-menu-index exhub-fim-menu-candidates)))

(defun exhub-fim-menu-show (candidates &optional index)
  "Display CANDIDATES in a dropdown at point, initially selecting INDEX.
When the dropdown is already displayed for the current buffer, replace
its candidates in place, keeping the selected candidate unless INDEX is
non-nil; streamed responses call this repeatedly.
Return nil when the dropdown cannot be displayed, in which case
nothing changes."
  (when (and candidates (exhub-fim-menu-can-display-p) (posn-at-point))
    (if (and (exhub-fim-menu-visible-p)
             (eq exhub-fim-menu--source-buffer (current-buffer)))
        ;; Refresh the visible menu in place, without tearing down the
        ;; frame.  `exhub-fim-menu--set-index' clamps the index and the
        ;; scrolled offset, re-renders, and runs
        ;; `exhub-fim-menu-selection-hook' to update the preview.
        (progn
          (setq exhub-fim-menu-candidates candidates)
          (exhub-fim-menu--set-index (or index exhub-fim-menu-index))
          t)
      ;; Drop a menu that may still be displayed for another buffer.
      (exhub-fim-menu-hide)
      (setq exhub-fim-menu-candidates candidates
            exhub-fim-menu-index (max 0 (min (or index 0) (1- (length candidates))))
            exhub-fim-menu-offset 0
            exhub-fim-menu--max-length-cache 0
            exhub-fim-menu--number-cache 0
            exhub-fim-menu--source-buffer (current-buffer)
            exhub-fim-menu--popup-point (point))
      ;; Drop the menu when the owning buffer goes away.
      (add-hook 'kill-buffer-hook #'exhub-fim-menu-hide nil t)
      (add-hook 'window-scroll-functions #'exhub-fim-menu--hide-on-scroll nil t)
      (exhub-fim-menu-init-colors)
      ;; A menu created for another frame cannot be reused.
      (when (and (frame-live-p exhub-fim-menu-frame)
                 (not (eq (frame-parent exhub-fim-menu-frame) (selected-frame))))
        (exhub-fim-menu--delete-frame))
      (exhub-fim-menu--create-frame-if-needed)
      (setq exhub-fim-menu--popup-position
            (exhub-fim-menu--popup-position exhub-fim-menu--popup-point))
      (exhub-fim-menu--set-frame-position exhub-fim-menu-frame
                                          (car exhub-fim-menu--popup-position)
                                          (cdr exhub-fim-menu--popup-position))
      (exhub-fim-menu--render)
      (exhub-fim-menu-mode 1)
      ;; Let the owner preview the initially selected candidate.
      (run-hooks 'exhub-fim-menu-selection-hook)
      t)))

(defun exhub-fim-menu--hide-on-scroll (&rest _)
  "Hide the dropdown of the buffer being scrolled."
  ;; `window-scroll-functions' runs for the buffer being scrolled.
  (when (and (exhub-fim-menu-visible-p)
             (eq (current-buffer) exhub-fim-menu--source-buffer))
    (exhub-fim-menu-hide)))

(defun exhub-fim-menu--delete-frame ()
  "Delete the dropdown frame."
  (when (frame-live-p exhub-fim-menu-frame)
    (delete-frame exhub-fim-menu-frame)
    (setq exhub-fim-menu-frame nil)))

(defun exhub-fim-menu-hide ()
  "Hide the dropdown and disable `exhub-fim-menu-mode'.
Safe to call when no dropdown is displayed."
  (when (and exhub-fim-menu--source-buffer
             (buffer-live-p exhub-fim-menu--source-buffer))
    ;; The candidate state is buffer-local to the buffer owning the
    ;; menu, so reset it there even when hiding was triggered from
    ;; another buffer.
    (with-current-buffer exhub-fim-menu--source-buffer
      (exhub-fim-menu-mode -1)
      (remove-hook 'kill-buffer-hook #'exhub-fim-menu-hide t)
      (remove-hook 'window-scroll-functions #'exhub-fim-menu--hide-on-scroll t)
      (setq exhub-fim-menu-candidates nil
            exhub-fim-menu-index 0
            exhub-fim-menu-offset 0)))
  (when (frame-live-p exhub-fim-menu-frame)
    (make-frame-invisible exhub-fim-menu-frame))
  (setq exhub-fim-menu--max-length-cache 0
        exhub-fim-menu--number-cache 0
        exhub-fim-menu--source-buffer nil
        exhub-fim-menu--popup-point nil
        exhub-fim-menu--popup-position nil))

(defun exhub-fim-menu-move (&optional line-bias)
  "Move the dropdown LINE-BIAS lines below the point it was popped up at.
Used to keep the menu clear of a multi-line ghost preview."
  (when (and (exhub-fim-menu-visible-p) exhub-fim-menu--popup-point
             (buffer-live-p exhub-fim-menu--source-buffer)
             ;; Positioning is relative to the window showing the buffer.
             (eq (window-buffer (selected-window)) exhub-fim-menu--source-buffer))
    (with-current-buffer exhub-fim-menu--source-buffer
      (when-let* ((pos (and (posn-at-point exhub-fim-menu--popup-point)
                            (exhub-fim-menu--popup-position
                             exhub-fim-menu--popup-point line-bias))))
        (setq exhub-fim-menu--popup-position pos)
        (set-frame-position exhub-fim-menu-frame (car pos) (cdr pos))
        (exhub-fim-menu--adjust-frame-pos exhub-fim-menu-frame)))))

(defun exhub-fim-menu--set-index (index)
  "Select candidate INDEX, scrolling the menu to keep it visible."
  (when exhub-fim-menu-candidates
    (let ((total (length exhub-fim-menu-candidates)))
      (setq index (max 0 (min index (1- total))))
      (cond
       ((< index exhub-fim-menu-offset)
        (setq exhub-fim-menu-offset index))
       ((>= index (+ exhub-fim-menu-offset exhub-fim-menu-max-items))
        (setq exhub-fim-menu-offset (- index exhub-fim-menu-max-items -1))))
      (setq exhub-fim-menu-index index)
      (exhub-fim-menu--render)
      ;; Keep the popup aligned with the ghost preview of the selection.
      (run-hooks 'exhub-fim-menu-selection-hook))))

;;;###autoload
(defun exhub-fim-menu-select-next (&optional n)
  "Select the next candidate, wrapping around at the end.
With a prefix argument N move that many candidates forward."
  (interactive "p")
  (when exhub-fim-menu-candidates
    (exhub-fim-menu--set-index
     (mod (+ exhub-fim-menu-index (or n 1)) (length exhub-fim-menu-candidates)))))

;;;###autoload
(defun exhub-fim-menu-select-previous (&optional n)
  "Select the previous candidate, wrapping around at the start.
With a prefix argument N move that many candidates backward."
  (interactive "p")
  (when exhub-fim-menu-candidates
    (let ((total (length exhub-fim-menu-candidates)))
      (exhub-fim-menu--set-index
       (mod (- exhub-fim-menu-index (or n 1)) total)))))

;;;###autoload
(defun exhub-fim-menu-select-first ()
  "Select the first candidate."
  (interactive)
  (exhub-fim-menu--set-index 0))

;;;###autoload
(defun exhub-fim-menu-select-last ()
  "Select the last candidate."
  (interactive)
  (when exhub-fim-menu-candidates
    (exhub-fim-menu--set-index (1- (length exhub-fim-menu-candidates)))))

;;;###autoload
(defun exhub-fim-menu-next-page (&optional n)
  "Select the candidate N pages down."
  (interactive "p")
  (when exhub-fim-menu-candidates
    (exhub-fim-menu--set-index
     (+ exhub-fim-menu-index (* (or n 1) exhub-fim-menu-max-items)))))

;;;###autoload
(defun exhub-fim-menu-previous-page (&optional n)
  "Select the candidate N pages up."
  (interactive "p")
  (when exhub-fim-menu-candidates
    (exhub-fim-menu--set-index
     (- exhub-fim-menu-index (* (or n 1) exhub-fim-menu-max-items)))))

;;;###autoload
(defun exhub-fim-menu-accept ()
  "Accept the selected candidate.
Runs `exhub-fim-menu-accept-hook' in the buffer owning the menu; that
buffer is responsible for inserting the candidate and hiding the menu."
  (interactive)
  (when exhub-fim-menu-candidates
    (run-hooks 'exhub-fim-menu-accept-hook)))

;;;###autoload
(defun exhub-fim-menu-cancel ()
  "Dismiss the dropdown without accepting anything."
  (interactive)
  (run-hooks 'exhub-fim-menu-cancel-hook)
  (exhub-fim-menu-hide)
  (message "Exhub-fim: suggestion dismissed"))

;;;###autoload
(defun exhub-fim-menu-mouse-accept (event)
  "Accept the candidate clicked at EVENT."
  (interactive "e")
  ;; The second slot is the click position for both `down-mouse-1' (2 slots)
  ;; and `mouse-1' (3 slots) events, unlike `event-start'.  `elt' because
  ;; events are vectors.
  (let* ((start (and (> (length event) 1) (elt event 1)))
         (point (and (consp start) (window-live-p (posn-window start))
                     (posn-point start)))
         (index (and point
                     (with-selected-window (posn-window start)
                       (get-char-property point 'exhub-fim-menu-index)))))
    (when-let* ((source-buffer (and (integerp index)
                                    (buffer-live-p exhub-fim-menu--source-buffer)
                                    exhub-fim-menu--source-buffer)))
      (with-current-buffer source-buffer
        (exhub-fim-menu--set-index index)
        (exhub-fim-menu-accept)))))

(provide 'exhub-fim-menu)
;;; exhub-fim-menu.el ends here
