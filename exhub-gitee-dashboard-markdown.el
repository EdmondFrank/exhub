;;; exhub-gitee-dashboard-markdown.el --- Gitee Enterprise dashboard: Markdown -*- lexical-binding: t; -*-

;; Copyright (C) 2025 EdmondFrank

;; Author: EdmondFrank
;; Keywords: tools, gitee, dashboard

;;; Commentary:

;; A small, dependency-free Markdown (GFM) to Org converter used by the
;; detail buffer: pull request / issue descriptions, comments and the
;; description bodies carried by "description updated" events.
;;
;; Only the subset Gitee actually emits is handled: ATX and setext headings,
;; fenced code blocks, GFM tables, lists and task lists, block quotes,
;; horizontal rules, inline code, bold / italic / strikethrough, Markdown and
;; HTML links and images, `<details>' blocks and HTML entities.
;;
;; Conversion is deliberately line based so that a malformed construct can
;; never swallow the rest of the document; anything unrecognised is emitted
;; as a plain paragraph.

;;; Code:

(require 'subr-x)

(require 'exhub-gitee-dashboard-vars)

(defcustom exhub-gitee-dashboard-render-markdown t
  "Whether to render Markdown bodies as Org in the detail buffer.
When nil, bodies are inserted verbatim inside an Org quote block."
  :type 'boolean
  :group 'exhub-gitee-dashboard)

;;;; Inline conversion

(defun exhub-gitee-dashboard--md-link-text (text)
  "Sanitize TEXT for use as an Org link description."
  (replace-regexp-in-string "[][\n\r]" " " (or text "")))

(defun exhub-gitee-dashboard--md-abs-url (url)
  "Return an absolute URL for URL, or nil when it is not linkable.
Gitee writes in-page mentions as `href=\"/username\"' and anchors as
`href=\"#note_1\"'."
  (let ((url (string-trim (or url ""))))
    (cond ((string-empty-p url) nil)
          ((string-prefix-p "#" url) nil)
          ((string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" url) url)
          ((string-prefix-p "//" url) (concat "https:" url))
          ((string-prefix-p "/" url) (concat "https://gitee.com" url))
          (t url))))

(defun exhub-gitee-dashboard--md-link (url desc)
  "Return an Org link to URL described by DESC, or just DESC when URL is unusable."
  (let ((abs (exhub-gitee-dashboard--md-abs-url url))
        (text (exhub-gitee-dashboard--md-link-text desc)))
    (cond ((null abs) text)
          ((string-empty-p text) (format "[[%s]]" abs))
          (t (format "[[%s][%s]]" abs text)))))

(defun exhub-gitee-dashboard--md-strip-tags (text)
  "Remove the HTML tags from TEXT."
  (replace-regexp-in-string "<[^>\n]*>" "" (or text "")))

(defun exhub-gitee-dashboard--md-decode-entities (text)
  "Decode the HTML entities in TEXT."
  (let ((pairs '(("&lt;" . "<") ("&gt;" . ">") ("&quot;" . "\"")
                 ("&#39;" . "'") ("&#x27;" . "'") ("&apos;" . "'")
                 ("&nbsp;" . " ") ("&amp;" . "&")))
        (result text))
    (dolist (pair pairs result)
      (setq result (string-replace (car pair) (cdr pair) result)))))

(defun exhub-gitee-dashboard--md-inline (text)
  "Convert inline Markdown TEXT to Org markup.
Inline code is preserved verbatim as `=code=' (or `~code~' when the code
itself contains an equals sign), links and images become Org links, and the
remaining HTML tags are dropped."
  (let ((s (or text ""))
        (codes nil)
        (bolds nil))
    ;; Protect inline code first so emphasis/link rules cannot touch it.
    (setq s (replace-regexp-in-string
             "`\\([^`\n]+\\)`"
             (lambda (m)
               (push (match-string 1 m) codes)
               (format "\x00%d\x00" (1- (length codes))))
             s t))
    (setq codes (nreverse codes))
    ;; Markdown images, then links.
    (setq s (replace-regexp-in-string
             "!\\[\\([^]]*\\)\\](\\([^)[:space:]]+\\)[^)]*)"
             (lambda (m)
               (let ((alt (string-trim (match-string 1 m))))
                 (exhub-gitee-dashboard--md-link
                  (match-string 2 m) (if (string-empty-p alt) "图片" alt))))
             s t))
    (setq s (replace-regexp-in-string
             "\\[\\([^]]+\\)\\](\\([^)[:space:]]+\\)[^)]*)"
             (lambda (m)
               (exhub-gitee-dashboard--md-link (match-string 2 m) (match-string 1 m)))
             s t))
    ;; HTML links, images, breaks and autolinks.
    (setq s (replace-regexp-in-string
             "<a[^>]*href=\"\\([^\"]+\\)\"[^>]*>\\(\\(?:.\\|\n\\)*?\\)</a>"
             (lambda (m)
               (exhub-gitee-dashboard--md-link
                (match-string 1 m)
                (exhub-gitee-dashboard--md-strip-tags (match-string 2 m))))
             s t))
    (setq s (replace-regexp-in-string
             "<img[^>]*src=\"\\([^\"]+\\)\"[^>]*>"
             (lambda (m) (exhub-gitee-dashboard--md-link (match-string 1 m) "图片"))
             s t))
    (setq s (replace-regexp-in-string "<br[ \t]*/?>[ \t]*" "\n" s t))
    (setq s (replace-regexp-in-string
             "<\\(https?://[^>[:space:]]+\\)>"
             (lambda (m) (format "[[%s]]" (match-string 1 m))) s t))
    (setq s (exhub-gitee-dashboard--md-strip-tags s))
    (setq s (exhub-gitee-dashboard--md-decode-entities s))
    ;; Emphasis: protect strong first so the emphasis rules cannot eat it,
    ;; then strikethrough and emphasis, then restore strong as `*text*'.
    (setq s (replace-regexp-in-string
             "\\(\\*\\*\\|__\\)\\([^*_\n]+\\)\\1"
             (lambda (m)
               (push (match-string 2 m) bolds)
               (format "\x01%d\x01" (1- (length bolds))))
             s t))
    (setq bolds (nreverse bolds))
    (setq s (replace-regexp-in-string "~~\\([^~\n]+\\)~~" "+\\1+" s t))
    (setq s (replace-regexp-in-string "\\*\\([^*\n]+\\)\\*" "/\\1/" s t))
    (setq s (replace-regexp-in-string "\\(\\`\\|\\s-\\)_\\([^_\n]+\\)_" "\\1/\\2/" s t))
    (setq s (replace-regexp-in-string
             "\x01\\([0-9]+\\)\x01"
             (lambda (m)
               (format "*%s*" (or (nth (string-to-number (match-string 1 m)) bolds) "")))
             s t))
    ;; Restore the protected inline code.
    (setq s (replace-regexp-in-string
             "\x00\\([0-9]+\\)\x00"
             (lambda (m)
               (let* ((code (or (nth (string-to-number (match-string 1 m)) codes) ""))
                      (equals (string-match-p "=" code))
                      (tilde (string-match-p "~" code)))
                 (cond ((and equals tilde) code)
                       (equals (format "~%s~" code))
                       (t (format "=%s=" code)))))
             s t))
    ;; `<br>' may have split the text into several lines; drop trailing blanks.
    (if (string-match-p "\n" s)
        (string-join (mapcar #'string-trim-right (split-string s "\n")) "\n")
      s)))

;;;; Tables

(defconst exhub-gitee-dashboard--md-table-separator-re
  "\\`[ \t]*|?[ \t:|-]*-[ \t:|-]*\\'"
  "Regexp matching a GFM table delimiter row, e.g. `|---|:--:|'.")

(defun exhub-gitee-dashboard--md-table-p (line)
  "Return non-nil when LINE belongs to a Markdown table."
  (string-match-p "\\`[ \t]*|" line))

(defun exhub-gitee-dashboard--md-table-separator-p (line)
  "Return non-nil when LINE is a GFM table delimiter row."
  (and (string-match-p exhub-gitee-dashboard--md-table-separator-re line)
       (string-match-p "|" line)
       (string-match-p "-" line)))

(defun exhub-gitee-dashboard--md-table-cells (line)
  "Split the GFM table row LINE into trimmed cell strings."
  (let ((row (string-trim line)))
    (setq row (if (string-prefix-p "|" row) (substring row 1) row))
    (setq row (if (string-suffix-p "|" row) (substring row 0 -1) row))
    (mapcar #'string-trim (split-string row "|"))))

(defun exhub-gitee-dashboard--md-table-row (line)
  "Convert the GFM table row LINE into an Org table row."
  (let ((cells (mapcar (lambda (cell)
                         (let ((text (exhub-gitee-dashboard--md-inline cell)))
                           (if (string-empty-p text) "-" text)))
                       (exhub-gitee-dashboard--md-table-cells line))))
    (if cells
        (concat "| " (mapconcat #'identity cells " | ") " |")
      "-")))

(defun exhub-gitee-dashboard--md-table-separator (line)
  "Return the Org separator row matching the GFM delimiter LINE."
  (let* ((count (length (exhub-gitee-dashboard--md-table-cells line)))
         (count (max 1 count)))
    (concat "|" (mapconcat #'identity (make-list count "---") "+") "|")))

;;;; Block conversion

(defun exhub-gitee-dashboard--md-heading-level (marker base)
  "Return the Org level for a Markdown heading MARKER nested below BASE."
  (+ base (length marker)))

(defun exhub-gitee-dashboard--markdown-to-org (text &optional base-level)
  "Convert Markdown TEXT to an Org string.
BASE-LEVEL is the Org level of the enclosing section (default 1), so a
Markdown `#' heading becomes a level BASE-LEVEL+1 heading."
  (let ((base (or base-level 1))
        (lines (split-string (replace-regexp-in-string "\r\n?" "\n" (or text ""))
                             "\n"))
        (out nil))
    (while lines
      (let ((line (car lines)))
        (cond
         ;; Blank line: collapse runs.
         ((string-blank-p line)
          (pop lines)
          (unless (or (null out) (string-empty-p (car out)))
            (push "" out)))
         ;; Fenced code block.
         ((string-match "\\`[ \t]*\\(?:```\\|~~~\\)[ \t]*\\([^`~]*\\)\\'" line)
          (let ((lang (string-trim (match-string 1 line)))
                (body nil))
            (pop lines)
            (while (and lines
                        (not (string-match-p
                              "\\`[ \t]*\\(?:```\\|~~~\\)[ \t]*\\'" (car lines))))
              (push (pop lines) body))
            (when lines (pop lines))
            (push (concat "#+begin_src " (if (string-empty-p lang) "text" lang)) out)
            (dolist (body-line (nreverse body))
              (push body-line out))
            (push "#+end_src" out)))
         ;; ATX heading.
         ((string-match "\\`[ \t]*\\(#\\{1,6\\}\\)[ \t]+\\(.*?\\)[ \t]*#*[ \t]*\\'" line)
          (let ((level (exhub-gitee-dashboard--md-heading-level (match-string 1 line) base))
                (title (exhub-gitee-dashboard--md-inline (match-string 2 line))))
            (pop lines)
            (unless (string-empty-p title)
              (push (concat (make-string level ?*) " " title) out))))
         ;; Setext heading (a paragraph underlined with === or ---).
         ((and (cdr lines)
               (string-match "\\`[ \t]*\\(=\\{2,\\}\\|-\\{2,\\}\\)[ \t]*\\'"
                             (cadr lines))
               (not (string-blank-p line))
               (not (exhub-gitee-dashboard--md-table-p line)))
          (let* ((underline (string-trim (cadr lines)))
                 (level (exhub-gitee-dashboard--md-heading-level
                         (if (string-prefix-p "=" underline) "##" "###") base))
                 (title (exhub-gitee-dashboard--md-inline (string-trim line))))
            (pop lines)
            (pop lines)
            (unless (string-empty-p title)
              (push (concat (make-string level ?*) " " title) out))))
         ;; Horizontal rule.
         ((string-match-p "\\`[ \t]*\\([-*_][ \t]*\\)\\{3,\\}\\'" line)
          (pop lines)
          (push "-----" out))
         ;; Block quote.
         ((string-match "\\`[ \t]*>[ \t]?\\(.*\\)\\'" line)
          (let ((inner nil))
            (while (and lines
                        (string-match "\\`[ \t]*>[ \t]?\\(.*\\)\\'" (car lines)))
              (push (match-string 1 (pop lines)) inner))
            (push "#+begin_quote" out)
            (dolist (inner-line
                     (split-string
                      (exhub-gitee-dashboard--markdown-to-org
                       (string-join (nreverse inner) "\n") base)
                      "\n"))
              (push inner-line out))
            (push "#+end_quote" out)))
         ;; Table.
         ((exhub-gitee-dashboard--md-table-p line)
          (let ((rows nil))
            (while (and lines (exhub-gitee-dashboard--md-table-p (car lines)))
              (push (pop lines) rows))
            (dolist (row (nreverse rows))
              (push (if (exhub-gitee-dashboard--md-table-separator-p row)
                        (exhub-gitee-dashboard--md-table-separator row)
                      (exhub-gitee-dashboard--md-table-row row))
                    out))))
         ;; List item.
         ((string-match "\\`\\([ \t]*\\)\\([-*+]\\|[0-9]+[.)]\\)[ \t]+\\(.*\\)\\'" line)
          (let* ((indent (length (match-string 1 line)))
                 (marker (match-string 2 line))
                 (ordered (string-match-p "\\`[0-9]" marker))
                 (marker (if ordered
                             (concat (string-trim-right marker "[.)]") ".")
                           "-"))
                 (body (exhub-gitee-dashboard--md-inline (match-string 3 line))))
            (pop lines)
            (push (format "%s%s %s" (make-string indent ?\s) marker body) out)))
         ;; Paragraph: keep the leading indentation so that a wrapped
          ;; line stays inside its list item.
         (t
          (pop lines)
          (let ((converted (string-trim-right (exhub-gitee-dashboard--md-inline line))))
            (unless (string-empty-p (string-trim converted))
              (push converted out)))))))
    (string-trim-right (string-join (mapcar #'string-trim-right (nreverse out)) "\n"))))

;;;; Insertion

(defun exhub-gitee-dashboard--md-quote (text)
  "Return TEXT verbatim inside an Org quote block."
  (let ((body (or text "")))
    (concat "#+begin_quote\n" body
            (unless (or (string-empty-p body) (string-suffix-p "\n" body)) "\n")
            "#+end_quote")))

(defun exhub-gitee-dashboard--insert-markdown (text &optional base-level)
  "Insert TEXT converted from Markdown at BASE-LEVEL.
Returns non-nil when something was inserted.  When
`exhub-gitee-dashboard-render-markdown' is nil the text is inserted verbatim
inside an Org quote block instead."
  (let ((converted (if exhub-gitee-dashboard-render-markdown
                       (exhub-gitee-dashboard--markdown-to-org text base-level)
                     (exhub-gitee-dashboard--md-quote text))))
    (unless (string-empty-p converted)
      (insert converted)
      (unless (string-suffix-p "\n" converted)
        (insert "\n"))
      t)))

(provide 'exhub-gitee-dashboard-markdown)

;;; exhub-gitee-dashboard-markdown.el ends here