;;; exhub-probe.el --- ExHub-backed probe search frontend  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 EdmondFrank

;; Author: Edmond Frank <edmondfrank@hotmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, search, semantic, code-navigation, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:
;;
;; WebSocket-backed code search over ExHub's existing `search_files' MCP tool.
;; It replaces probe.el's subprocess frontend while keeping probe.el's UX shape
;; (per-query+dir buffers, file collapse, syntax highlighting, RET to jump).
;;
;; Transport is ExHub's existing WebSocket protocol: the Emacs side sends
;;   (exhub-call "exhub-search" ACTION REQ-ID PARAMS)
;; which the `Exhub.ResponseHandlers.ExhubSearch' handler answers by calling
;; `BuiltInRegistry.call_tool("desktop", "search_files", PARAMS)' in-VM and
;; pushing back
;;   (exhub-probe--receive REQ-ID IS-ERROR JSON)
;;
;; Requires: exhub.el (the ExHub WebSocket client) and an ExHub build with the
;; `exhub-search' response handler.
;;
;; Commands:
;;   M-x exhub-probe-search       semantic search (query + optional purpose)
;;   M-x exhub-probe-at-point     semantic search for the symbol at point
;;   M-x exhub-probe-region       semantic search for the active region
;;   M-x exhub-probe-glob         glob search for file/directory paths
;;   M-x exhub-probe-content      content (ripgrep) search
;;   M-x exhub-probe-dir          change the search directory
;;   M-x exhub-probe-parent-dir   search the parent directory
;;   M-x exhub-probe-rerun        rerun the last search
;;
;; Keys in a results buffer:
;;   RET visit, n/p next/prev, TAB collapse/expand, g rerun, s new search,
;;   t tests, f Smart Decide filter, r reranker, D directory, ^ parent dir,
;;   C config, q quit.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'project)
(require 'subr-x)

(require 'exhub nil t)

(declare-function exhub-call "exhub" (&rest func-args))
(declare-function exhub-open-connection "exhub" ())
(declare-function exhub-start "exhub" ())

;;; Customization

(defgroup exhub-probe nil
  "ExHub-backed probe search."
  :group 'tools)

(defcustom exhub-probe-max-buffers 10
  "Maximum number of live `exhub-probe-mode' result buffers.
Least-recently-used buffers are killed when a new one is created.  A non-number
value disables the limit."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'exhub-probe)

(defcustom exhub-probe-include-tests nil
  "Whether new searches include test files by default."
  :type 'boolean
  :group 'exhub-probe)

(defcustom exhub-probe-filter t
  "Whether new semantic searches run the Smart Decide relevance filter.
This is always sent to the server explicitly: the server-side default is off
in the test environment, so the frontend never relies on it."
  :type 'boolean
  :group 'exhub-probe)

(defcustom exhub-probe-reranker 'bm25
  "Reranker shown in the state summary.
The `search_files' tool exposes no reranker parameter, so this value is
display-only and is not forwarded to the server."
  :type '(choice (const bm25) (const hybrid) (const hybrid2) (const tfidf)
                 (const ms-marco-tinybert) (const ms-marco-minilm-l6)
                 (const ms-marco-minilm-l12))
  :group 'exhub-probe)

(defcustom exhub-probe-max-results nil
  "Optional maximum results (`max_results' param; nil = tool default)."
  :type '(choice (const :tag "Tool default" nil) integer)
  :group 'exhub-probe)

(defcustom exhub-probe-show-config nil
  "Whether new result buffers show the configuration block initially."
  :type 'boolean
  :group 'exhub-probe)

;;; Faces

(defface exhub-probe-header-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the result buffer header line."
  :group 'exhub-probe)

(defface exhub-probe-filename-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for clickable file headers."
  :group 'exhub-probe)

(defface exhub-probe-line-number-face
  '((t :inherit font-lock-comment-face))
  "Face for per-line numbers."
  :group 'exhub-probe)

(defface exhub-probe-separator-face
  '((t :inherit shadow))
  "Face for section separators."
  :group 'exhub-probe)

(defface exhub-probe-meta-face
  '((t :inherit font-lock-comment-face))
  "Face for UI/prose text."
  :group 'exhub-probe)

(defface exhub-probe-match-face
  '((t :inherit highlight))
  "Face for matched lines in content results."
  :group 'exhub-probe)

(defface exhub-probe-code-face
  '((t :inherit default))
  "Face for raw (unparsed) code output."
  :group 'exhub-probe)

;;; State

(defvar exhub-probe--request-counter 0
  "Monotonic per-session request id counter.")

(defvar exhub-probe--inflight (make-hash-table :test 'eql)
  "Hash mapping an in-flight request id to its result buffer.")

(defvar-local exhub-probe--pending-id nil
  "Request id of the newest search issued for this buffer.
Replies for older ids are dropped so a superseded search cannot override a
newer one.")

(defvar-local exhub-probe--last-payload nil
  "Last successfully parsed reply payload (a hash-table).")

(defvar-local exhub-probe--error nil
  "Error string to display inline, or nil.")

(defvar-local exhub-probe--search-type "semantic"
  "One of \"semantic\", \"glob\" or \"content\".")

(defvar-local exhub-probe--query nil
  "Semantic query, or the glob/content pattern.")

(defvar-local exhub-probe--pattern nil
  "Glob/content pattern (mirrors `exhub-probe--query' for those types).")

(defvar-local exhub-probe--purpose nil
  "Optional natural-language purpose for the Smart Decide filter.")

(defvar-local exhub-probe--directory nil
  "Absolute search directory.")

(defvar-local exhub-probe--include-tests nil
  "Buffer-local include-tests state.")

(defvar-local exhub-probe--filter t
  "Buffer-local Smart Decide filter state.")

(defvar-local exhub-probe--reranker 'bm25
  "Buffer-local display-only reranker.")

(defvar-local exhub-probe--show-config nil
  "Whether the configuration block is shown in this buffer.")

(defvar-local exhub-probe--collapsed nil
  "List of collapsed file paths.")

(defvar-local exhub-probe--overlays nil
  "List of collapse overlays currently active.")

(defvar-local exhub-probe--regions nil
  "List of (ID FILE BODY-START-MARKER BODY-END-MARKER) recorded during redraw.")

;;; Transport

(defun exhub-probe--bool (value)
  "Encode VALUE as a JSON boolean, never `null'."
  (if value t :json-false))

(defun exhub-probe--build-params ()
  "Build the `search_files' parameter hash-table for the current buffer."
  (let ((params (make-hash-table :test 'equal)))
    (puthash "path" (expand-file-name exhub-probe--directory) params)

    (pcase exhub-probe--search-type
      ("semantic"
       (puthash "query" (or exhub-probe--query "") params)
       (when (and exhub-probe--purpose (not (string-empty-p exhub-probe--purpose)))
         (puthash "purpose" exhub-probe--purpose params))
       (puthash "allow_tests" (exhub-probe--bool exhub-probe--include-tests) params)
       (puthash "filter" (exhub-probe--bool exhub-probe--filter) params))
      ((or "glob" "content")
       (puthash "pattern" (or exhub-probe--pattern "") params)))

    (when (and (integerp exhub-probe-max-results) (> exhub-probe-max-results 0))
      (puthash "max_results" exhub-probe-max-results params))

    params))

(defun exhub-probe--action ()
  "Return the ExHub search action for the current buffer's search type."
  (pcase exhub-probe--search-type
    ("glob" "glob")
    ("content" "content")
    (_ "search")))

(defun exhub-probe--run ()
  "Start a search in the current buffer using its state variables."
  (let ((params (exhub-probe--build-params))
        (action (exhub-probe--action)))
    (unless (exhub-open-connection)
      (exhub-start))
    (if (exhub-open-connection)
        (let ((req-id (cl-incf exhub-probe--request-counter)))
          (setq exhub-probe--pending-id req-id
                exhub-probe--error nil
                exhub-probe--last-payload nil)
          (exhub-probe--redraw)
          (puthash req-id (current-buffer) exhub-probe--inflight)
          (let ((json-object-type 'hash-table)
                (json-array-type 'list))
            (exhub-call "exhub-search" action req-id params)))
      (setq exhub-probe--error
            "ExHub WebSocket is not connected (exhub-start did not establish it).")
      (exhub-probe--redraw))))

(defun exhub-probe--receive (req-id is-error json)
  "Callback invoked by ExHub over the WebSocket.

REQ-ID is the id echoed from the request; IS-ERROR is non-nil when the server
reports failure; JSON is the single-line JSON payload.  Replies for stale or
superseded requests are dropped."
  (let* ((buf (gethash req-id exhub-probe--inflight))
         (fresh (and buf
                     (buffer-live-p buf)
                     (eql req-id (buffer-local-value 'exhub-probe--pending-id buf)))))
    (remhash req-id exhub-probe--inflight)
    (when fresh
      (condition-case err
          (let ((payload (json-parse-string json
                                            :object-type 'hash-table
                                            :array-type 'list
                                            :null-object nil
                                            :false-object nil)))
            (if (and (not is-error) (gethash "ok" payload))
                (with-current-buffer buf
                  (setq exhub-probe--error nil
                        exhub-probe--last-payload payload)
                  (exhub-probe--redraw))
              (with-current-buffer buf
                (setq exhub-probe--error
                      (or (gethash "error" payload) "Unknown search error")
                      exhub-probe--last-payload nil)
                (exhub-probe--redraw))))
        (error
         (with-current-buffer buf
           (setq exhub-probe--error (format "Malformed reply: %s" err)
                 exhub-probe--last-payload nil)
           (exhub-probe--redraw)))))))

;;; Buffers

(defun exhub-probe--project-root ()
  "Return the search root: project.el, else the enclosing `.git'."
  (let ((start default-directory))
    (or (when-let ((project (project-current nil start)))
          (expand-file-name (project-root project)))
        (locate-dominating-file start ".git")
        (expand-file-name start))))

(defun exhub-probe--buffer-name (query directory)
  "Return the result buffer name for QUERY in DIRECTORY."
  (format "*exhub-probe %s %s*"
          (truncate-string-to-width (or query "") 30 nil nil t)
          (abbreviate-file-name (expand-file-name directory))))

(defun exhub-probe--buffers ()
  "All live result buffers, most recently used first."
  (cl-remove-if-not
   (lambda (buf) (with-current-buffer buf (derived-mode-p 'exhub-probe-mode)))
   (buffer-list)))

(defun exhub-probe--trim-buffers ()
  "Kill least-recently-used result buffers beyond `exhub-probe-max-buffers'."
  (when (and (integerp exhub-probe-max-buffers) (> exhub-probe-max-buffers 0))
    (dolist (buf (nthcdr (1- exhub-probe-max-buffers) (exhub-probe--buffers)))
      (kill-buffer buf))))

(defun exhub-probe--buffer (query directory)
  "Return (creating if needed) the result buffer for QUERY in DIRECTORY."
  (let* ((name (exhub-probe--buffer-name query directory))
         (buf (get-buffer name)))
    (unless buf
      (exhub-probe--trim-buffers)
      (setq buf (get-buffer-create name))
      (with-current-buffer buf
        (setq default-directory (file-name-as-directory (expand-file-name directory)))
        (exhub-probe-mode)))
    buf))

(defun exhub-probe--begin (type query purpose &optional directory)
  "Start a TYPE search for QUERY (with optional PURPOSE) in DIRECTORY."
  (let* ((dir (expand-file-name (or directory (exhub-probe--project-root))))
         (buf (exhub-probe--buffer query dir)))
    (with-current-buffer buf
      (setq exhub-probe--search-type type
            exhub-probe--query query
            exhub-probe--pattern query
            exhub-probe--purpose purpose
            exhub-probe--directory dir
            exhub-probe--include-tests exhub-probe-include-tests
            exhub-probe--filter exhub-probe-filter
            exhub-probe--reranker exhub-probe-reranker
            exhub-probe--show-config exhub-probe-show-config
            exhub-probe--collapsed nil
            exhub-probe--error nil)
      (exhub-probe--run))
    (pop-to-buffer buf)))

;;; Rendering

(defun exhub-probe--state-summary ()
  "One-line description of the current search state."
  (format "%s: %s | dir: %s | type: %s | filter: %s | tests: %s | reranker: %s%s"
          (if (member exhub-probe--search-type '("glob" "content")) "pattern" "query")
          (or exhub-probe--query "")
          (abbreviate-file-name (or exhub-probe--directory default-directory))
          (or exhub-probe--search-type "semantic")
          (if exhub-probe--filter "on" "off")
          (if exhub-probe--include-tests "on" "off")
          exhub-probe--reranker
          (if (member exhub-probe--search-type '("glob" "content"))
              " (n/a)" "")))

(defun exhub-probe--insert-banner ()
  "Insert the in-buffer banner."
  (insert (propertize (format "ExHub Probe — %s\n" (exhub-probe--state-summary))
                      'face 'exhub-probe-header-face))
  (insert (propertize (make-string 72 ?─) 'face 'exhub-probe-separator-face))
  (insert "\n"))

(defun exhub-probe--insert-config ()
  "Insert the configuration block."
  (insert "\n")
  (insert (propertize "Configuration:\n" 'face 'exhub-probe-meta-face))
  (dolist (line (list (format "    Search directory: %s"
                              (abbreviate-file-name (or exhub-probe--directory default-directory)))
                      (format "    Search type:      %s" exhub-probe--search-type)
                      (format "    Reranker:         %s (display-only; search_files has no reranker parameter)"
                              exhub-probe--reranker)
                      (format "    Smart Decide:     %s" (if exhub-probe--filter "on" "off"))
                      (format "    Include tests:    %s" (if exhub-probe--include-tests "on" "off"))
                      (format "    Max results:      %s" (or exhub-probe-max-results "tool default"))))
    (insert (propertize (concat line "\n") 'face 'exhub-probe-meta-face)))
  (insert (propertize (make-string 72 ?─) 'face 'exhub-probe-separator-face))
  (insert "\n"))

(defun exhub-probe--insert-separator ()
  "Insert a section separator."
  (insert (propertize (make-string 72 ?─) 'face 'exhub-probe-separator-face))
  (insert "\n"))

(defun exhub-probe--redraw ()
  "Rebuild the current buffer from its state and last payload."
  (let ((inhibit-read-only t)
        (collapsed exhub-probe--collapsed))
    (erase-buffer)
    (remove-overlays)
    (setq exhub-probe--overlays nil
          exhub-probe--regions nil
          exhub-probe--collapsed collapsed)
    (exhub-probe--insert-banner)
    (when exhub-probe--show-config
      (exhub-probe--insert-config))
    (cond
     (exhub-probe--error
      (insert "\n")
      (insert (propertize (format "%s\n" exhub-probe--error) 'face 'error)))
     ((null exhub-probe--last-payload)
      (insert "\n")
      (insert (propertize "Searching…\n" 'face 'exhub-probe-meta-face)))
     (t
      (insert "\n")
      (exhub-probe--insert-results)))
    (exhub-probe--apply-collapses)
    (goto-char (point-min)))
  (setq header-line-format (concat " " (exhub-probe--state-summary)))
  (force-mode-line-update))

(defun exhub-probe--get (map key)
  "Look up KEY (a string) in MAP, accepting string or symbol keys."
  (cond
   ((hash-table-p map) (or (gethash key map) (gethash (intern key) map)))
   ((listp map) (cdr (assoc (intern key) map)))
   (t nil)))

(defun exhub-probe--insert-results ()
  "Insert the parsed results of `exhub-probe--last-payload'."
  (let* ((payload exhub-probe--last-payload)
         (data (exhub-probe--get payload "data"))
         (text (exhub-probe--get payload "text"))
         (type exhub-probe--search-type))
    (cond
     ((and data (equal type "glob")) (exhub-probe--insert-glob data))
     ((and data (equal type "content")) (exhub-probe--insert-content data))
     ((and text (exhub-probe--semantic-text-p text)) (exhub-probe--insert-semantic text))
     (t (exhub-probe--insert-raw text)))))

(defun exhub-probe--semantic-text-p (text)
  "Non-nil when TEXT is the tool's structured (filtered) semantic render.

The Smart Decide filter is what produces the `File: PATH (symbol, lines A-B)'
headers.  Without it the tool returns probe's own render, whose `File:' lines
carry no line range, so it is kept on the verbatim path rather than half-parsed."
  (and (stringp text)
       (string-match "^Pattern: " text)
       (string-match "^Path: " text)
       (string-match "^File: .* (.*lines [0-9]+-[0-9]+)$" text)))

(defun exhub-probe--insert-raw (text)
  "Insert TEXT verbatim, flagging the unrecognized format."
  (insert (propertize
           "[exhub-probe: unrecognized output format — showing raw text]\n"
           'face 'exhub-probe-meta-face))
  (insert "\n")
  (insert (propertize (if (and (stringp text) (not (string-empty-p text)))
                          (concat text "\n")
                        "(no output)\n")
                      'face 'exhub-probe-code-face)))

;;; Semantic rendering

(defun exhub-probe--split-sections (text)
  "Split TEXT into (PREAMBLE . SECTIONS) on lines that are exactly \"---\".
SECTIONS is a list of line lists, each starting with its `File: ...' header."
  (let ((lines (split-string (or text "") "\n"))
        (preamble nil)
        (sections nil)
        (current nil)
        (seen-sep nil))
    (dolist (line lines)
      (if (string= (string-trim line) "---")
          (progn
            (when seen-sep
              (push (nreverse current) sections))
            (setq seen-sep t
                  current nil))
        (if seen-sep
            (push line current)
          (push line preamble))))
    (when seen-sep
      (push (nreverse current) sections))
    (cons (nreverse preamble) (nreverse sections))))

(defun exhub-probe--parse-file-header (line)
  "Parse a `File: ...' LINE into (FILE SYMBOL START END), or nil."
  (when (and (stringp line) (string-match "\\`File: \\(.+?\\)\\(?: (\\(.*\\))\\)?\\'" line))
    (let* ((file (match-string 1 line))
           (meta (match-string 2 line))
           (start nil)
           (end nil)
           (symbol nil))
      (when (and meta (string-match "\\([0-9]+\\)-\\([0-9]+\\)\\'" meta))
        (setq start (string-to-number (match-string 1 meta))
              end (string-to-number (match-string 2 meta))
              symbol (string-trim
                      (replace-regexp-in-string
                       ",?[[:space:]]*lines[[:space:]]*[0-9]+-[0-9]+" "" meta))))
      (list file (and symbol (not (string-empty-p symbol)) symbol) start end))))

(defun exhub-probe--highlight-code (code filename)
  "Return CODE with font-lock faces applied, based on FILENAME."
  (condition-case nil
      (with-temp-buffer
        (insert code)
        (let ((mode (assoc-default filename auto-mode-alist #'string-match)))
          (when (consp mode) (setq mode (car mode)))
          (when (and (symbolp mode) (not (fboundp mode)))
            (require mode nil t))
          (when (and (symbolp mode) (fboundp mode))
            ;; Bind `delay-mode-hooks' so the mode's (possibly slow, prompting
            ;; or buffer-name-sensitive) hooks are never run in this temp
            ;; buffer; only the mode body's font-lock setup is needed.
            (let ((delay-mode-hooks t))
              (funcall mode))))
        (when font-lock-defaults
          (font-lock-mode 1)
          (font-lock-ensure (point-min) (point-max)))
        (buffer-string))
    (error code)))

(defun exhub-probe--insert-file-header (file symbol start end id)
  "Insert a clickable file header for FILE with ID."
  (let ((label (concat "File: " (abbreviate-file-name file)
                       (cond
                        ((and symbol start end) (format " (%s, lines %d-%d)" symbol start end))
                        ((and start end) (format " (lines %d-%d)" start end))
                        (t "")))))
    (insert (propertize label
                        'face 'exhub-probe-filename-face
                        'mouse-face 'highlight
                        'help-echo file
                        'keymap exhub-probe--visit-keymap
                        'exhub-probe-file file
                        'exhub-probe-line start
                        'exhub-probe-section id))
    (insert "\n")))

(defun exhub-probe--insert-code (code filename start id)
  "Insert CODE (highlighted for FILENAME) numbered from START, tagged ID."
  (let* ((highlighted (if (and filename (stringp filename) (not (string-empty-p code)))
                          (exhub-probe--highlight-code code filename)
                        code))
         (lines (split-string (or highlighted "") "\n"))
         (n start))
    (dolist (line lines)
      (let ((beg (point)))
        (insert (propertize (format "%6s │ " (if (integerp n) (number-to-string n) ""))
                            'face 'exhub-probe-line-number-face))
        (insert line)
        (insert "\n")
        (add-text-properties beg (1- (point))
                             (list 'exhub-probe-file filename
                                   'exhub-probe-line n
                                   'keymap exhub-probe--visit-keymap
                                   'exhub-probe-section id))
        (when (integerp n) (setq n (1+ n)))))))

(defun exhub-probe--insert-section (id file symbol start end code-lines)
  "Insert one semantic section with ID, recording its body region."
  (exhub-probe--insert-file-header file symbol start end id)
  (insert "\n")
  (let ((body-start (point-marker)))
    (exhub-probe--insert-code (string-join code-lines "\n") file start id)
    (push (list id file body-start (point-marker)) exhub-probe--regions)))

(defun exhub-probe--insert-semantic (text)
  "Render the structured semantic TEXT."
  (let* ((sections (cdr (exhub-probe--split-sections text)))
         (id 0)
         (rendered 0))
    (dolist (lines sections)
      (let* ((sanitized (cl-remove-if #'null lines))
             (header (car sanitized))
             (code (cdr sanitized))
             (info (exhub-probe--parse-file-header header)))
        ;; Drop leading blank lines from the code block.
        (while (and code (string-empty-p (string-trim (car code))))
          (setq code (cdr code)))
        (when (or (nth 0 info) code)
          (setq id (1+ id)
                rendered (1+ rendered))
          (pcase-let ((`(,file ,symbol ,start ,end) info))
            (exhub-probe--insert-section id file symbol start end code))
          (exhub-probe--insert-separator))))
    (unless (> rendered 0)
      (exhub-probe--insert-raw text))))

;;; Glob rendering

(defun exhub-probe--insert-glob (data)
  "Render glob DATA (a hash-table with a `results' list of relative paths)."
  (let ((results (exhub-probe--get data "results"))
        (dir (or exhub-probe--directory default-directory))
        (id 0))
    (if (null results)
        (insert (propertize "No results found.\n" 'face 'exhub-probe-meta-face))
      (dolist (rel (append results nil))
        (setq id (1+ id))
        (let* ((abs (expand-file-name rel dir))
               (beg (point)))
          (insert "  " rel "\n")
          (add-text-properties beg (1- (point))
                               (list 'face 'exhub-probe-filename-face
                                     'mouse-face 'highlight
                                     'help-echo abs
                                     'keymap exhub-probe--visit-keymap
                                     'exhub-probe-file abs
                                     'exhub-probe-line nil
                                     'exhub-probe-section id)))))))

;;; Content rendering

(defun exhub-probe--insert-content (data)
  "Render content DATA (a hash-table with `results' grouped per file)."
  (let ((results (exhub-probe--get data "results"))
        (id 0))
    (if (null results)
        (insert (propertize "No results found.\n" 'face 'exhub-probe-meta-face))
      (dolist (entry (append results nil))
        (let ((file (exhub-probe--get entry "path"))
              (matches (exhub-probe--get entry "matches")))
          (setq id (1+ id))
          (exhub-probe--insert-file-header file nil nil nil id)
          (insert "\n")
          (let ((body-start (point-marker)))
            (dolist (match (append matches nil))
              (let ((ctx (exhub-probe--get match "context"))
                    (ln (exhub-probe--get match "line_number")))
                (dolist (line (split-string (or ctx "") "\n"))
                  (when (not (string-empty-p line))
                    (let ((beg (point)))
                      (insert line "\n")
                      (add-text-properties
                       beg (1- (point))
                       (list 'exhub-probe-file file
                             'exhub-probe-line ln
                             'keymap exhub-probe--visit-keymap
                             'exhub-probe-section id
                             'face (if (string-match-p "\\`=>" line)
                                       'exhub-probe-match-face
                                     'exhub-probe-code-face))))))))
            (push (list id file body-start (point-marker)) exhub-probe--regions))
          (exhub-probe--insert-separator))))))

;;; Collapse / expand

(defun exhub-probe--collapse-region (id file body-start body-end)
  "Hide BODY-START..BODY-END with a collapsed indicator for ID/FILE."
  (let ((ov (make-overlay body-start body-end)))
    (overlay-put ov 'invisible t)
    (overlay-put ov 'exhub-probe-id id)
    (overlay-put ov 'exhub-probe-file file)
    (overlay-put ov 'before-string
                 (propertize "    [collapsed — TAB to expand]\n"
                             'face 'exhub-probe-meta-face))
    (push ov exhub-probe--overlays)))

(defun exhub-probe--remove-overlays (file)
  "Delete collapse overlays for FILE."
  (let (keep)
    (dolist (ov exhub-probe--overlays)
      (if (equal (overlay-get ov 'exhub-probe-file) file)
          (delete-overlay ov)
        (push ov keep)))
    (setq exhub-probe--overlays (nreverse keep))))

(defun exhub-probe--apply-collapses ()
  "Re-apply collapse overlays recorded in `exhub-probe--collapsed'."
  (dolist (region exhub-probe--regions)
    (pcase-let ((`(,id ,file ,body-start ,body-end) region))
      (when (member file exhub-probe--collapsed)
        (exhub-probe--collapse-region id file body-start body-end)))))

;;; Visiting

(defvar exhub-probe--visit-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'exhub-probe-mouse-visit)
    map)
  "Keymap for clickable result lines.")

(defun exhub-probe--visit-target ()
  "Return (FILE . LINE) at point, or nil."
  (let ((file (or (get-text-property (line-beginning-position) 'exhub-probe-file)
                  (get-text-property (point) 'exhub-probe-file)))
        (line (or (get-text-property (line-beginning-position) 'exhub-probe-line)
                  (get-text-property (point) 'exhub-probe-line))))
    (when file (cons file line))))

(defun exhub-probe-visit ()
  "Visit the file (and line) at point."
  (interactive)
  (pcase-let ((`(,file . ,line) (exhub-probe--visit-target)))
    (cond
     ((null file) (user-error "No file at point"))
     ((file-directory-p file) (dired-other-window file))
     (t
      (find-file-other-window file)
      (when (integerp line)
        (goto-char (point-min))
        (forward-line (1- line)))))))

(defun exhub-probe-mouse-visit (event)
  "Visit the result clicked by EVENT."
  (interactive "e")
  (let ((posn (event-start event)))
    (with-current-buffer (window-buffer (posn-window posn))
      (goto-char (posn-point posn))
      (exhub-probe-visit))))

;;; Commands

(defun exhub-probe-toggle-section ()
  "Collapse or expand the file section at point."
  (interactive)
  (let* ((id (or (get-text-property (line-beginning-position) 'exhub-probe-section)
                 (get-text-property (point) 'exhub-probe-section)))
         (region (and id (cl-find-if (lambda (r) (eql (car r) id)) exhub-probe--regions))))
    (unless region (user-error "No file section at point"))
    (pcase-let ((`(,sid ,file ,body-start ,body-end) region))
      (if (member file exhub-probe--collapsed)
          (progn
            (setq exhub-probe--collapsed (delete file exhub-probe--collapsed))
            (exhub-probe--remove-overlays file)
            (message "Expanded %s" (file-name-nondirectory file)))
        (push file exhub-probe--collapsed)
        (exhub-probe--collapse-region sid file body-start body-end)
        (message "Collapsed %s" (file-name-nondirectory file))))))

(defun exhub-probe-toggle-tests ()
  "Toggle inclusion of test files, then rerun."
  (interactive)
  (setq exhub-probe--include-tests (not exhub-probe--include-tests))
  (message "Test files %s" (if exhub-probe--include-tests "included" "excluded"))
  (when exhub-probe--query (exhub-probe--run)))

(defun exhub-probe-toggle-filter ()
  "Toggle the Smart Decide relevance filter, then rerun."
  (interactive)
  (if (equal exhub-probe--search-type "semantic")
      (progn
        (setq exhub-probe--filter (not exhub-probe--filter))
        (message "Smart Decide filter %s" (if exhub-probe--filter "on" "off"))
        (when exhub-probe--query (exhub-probe--run)))
    (message "Smart Decide filter only applies to semantic searches")))

(defun exhub-probe-select-reranker (reranker)
  "Set the display-only RERANKER and refresh the header."
  (interactive
   (list (intern (completing-read
                  (format "Reranker (current: %s): " exhub-probe--reranker)
                  '("bm25" "hybrid" "hybrid2" "tfidf" "ms-marco-tinybert"
                    "ms-marco-minilm-l6" "ms-marco-minilm-l12")
                  nil t))))
  (setq exhub-probe--reranker reranker)
  (setq header-line-format (concat " " (exhub-probe--state-summary)))
  (force-mode-line-update)
  (message "Reranker set to %s (display-only: search_files exposes no reranker parameter)"
           reranker))

(defun exhub-probe-toggle-config ()
  "Toggle the configuration block."
  (interactive)
  (setq exhub-probe--show-config (not exhub-probe--show-config))
  (exhub-probe--redraw))

(defun exhub-probe-dir (dir)
  "Change the search directory to DIR, then rerun."
  (interactive
   (list (read-directory-name "Search directory: "
                              (or exhub-probe--directory default-directory))))
  (setq exhub-probe--directory (expand-file-name dir))
  (when exhub-probe--query (exhub-probe--run)))

(defun exhub-probe-parent-dir ()
  "Rerun the search in the parent of the current search directory."
  (interactive)
  (let ((parent (file-name-directory
                 (directory-file-name (or exhub-probe--directory default-directory)))))
    (setq exhub-probe--directory (expand-file-name parent))
    (when exhub-probe--query (exhub-probe--run))))

(defun exhub-probe-rerun ()
  "Rerun the current search."
  (interactive)
  (unless exhub-probe--query (user-error "No search to rerun"))
  (exhub-probe--run))

;;; Entry points

(defun exhub-probe-search (query &optional purpose)
  "Semantic search for QUERY, with optional natural-language PURPOSE.
PURPOSE drives the Smart Decide relevance filter and falls back to QUERY."
  (interactive "sSearch query: \nsPurpose (optional): ")
  (exhub-probe--begin "semantic" query
                      (and purpose (not (string-empty-p purpose)) purpose)))

(defun exhub-probe-at-point ()
  "Semantic search for the symbol at point."
  (interactive)
  (let ((symbol (thing-at-point 'symbol t)))
    (unless symbol (user-error "No symbol at point"))
    (exhub-probe-search (string-trim symbol))))

(defun exhub-probe-region (beg end)
  "Semantic search for the active region."
  (interactive "r")
  (let ((text (string-trim (buffer-substring-no-properties beg end))))
    (when (string-empty-p text) (user-error "Empty region"))
    (exhub-probe-search text)))

(defun exhub-probe-glob (pattern &optional directory)
  "Glob search for PATTERN, optionally rooted at DIRECTORY."
  (interactive "sGlob pattern: ")
  (exhub-probe--begin "glob" pattern nil directory))

(defun exhub-probe-content (pattern &optional directory)
  "Content (ripgrep) search for PATTERN, optionally rooted at DIRECTORY."
  (interactive "sContent pattern: ")
  (exhub-probe--begin "content" pattern nil directory))

;;; Mode

(defvar exhub-probe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'exhub-probe-visit)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "TAB") #'exhub-probe-toggle-section)
    (define-key map (kbd "g") #'exhub-probe-rerun)
    (define-key map (kbd "s") #'exhub-probe-search)
    (define-key map (kbd "t") #'exhub-probe-toggle-tests)
    (define-key map (kbd "f") #'exhub-probe-toggle-filter)
    (define-key map (kbd "r") #'exhub-probe-select-reranker)
    (define-key map (kbd "D") #'exhub-probe-dir)
    (define-key map (kbd "^") #'exhub-probe-parent-dir)
    (define-key map (kbd "C") #'exhub-probe-toggle-config)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `exhub-probe-mode'.")

(define-derived-mode exhub-probe-mode special-mode "ExHub-Probe"
  "Major mode for ExHub probe search results."
  (setq-local truncate-lines nil)
  (setq-local exhub-probe--pending-id nil)
  (setq-local exhub-probe--last-payload nil)
  (setq-local exhub-probe--error nil)
  (setq-local exhub-probe--collapsed nil)
  (setq-local exhub-probe--overlays nil)
  (setq-local exhub-probe--regions nil)
  (setq-local header-line-format nil))

;;; Aliases (only when probe.el does not already provide them)

(unless (fboundp 'probe-search)
  (defalias 'probe-search #'exhub-probe-search))

(unless (fboundp 'probe-query)
  (defalias 'probe-query #'exhub-probe-search))

(provide 'exhub-probe)
;;; exhub-probe.el ends here