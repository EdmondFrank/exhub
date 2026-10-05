;;; exhub-lsp.el --- LSP client over ExHub -*- lexical-binding: t; -*-

;;; Commentary:
;; Thin ExHub-native front end for the `Exhub.LspBridge' backend — the
;; option-B replacement for lsp-bridge's Python process, keeping the elisp
;; client in the ExHub idiom (`exhub-call' out, evaluated forms back).
;;
;; Diagnostics are rendered with flymake.  Phase 2 adds the read-only
;; features: hover, definition / type-definition / implementation /
;; references navigation and document / workspace symbols.  Phase 3 adds
;; completion, surfaced through `acm-backend-exhub-lsp.el' (acm's completion
;; menu), plus `completionItem/resolve' documentation.  Phase 4 adds edits:
;; rename (`prepareRename'/`rename'), formatting (whole buffer and region),
;; code actions and `workspace/executeCommand' — the resulting `WorkspaceEdit'/
;; `TextEdit's are applied to the affected buffers on the elisp side — plus
;; call hierarchy (incoming/outgoing).  ExHub owns the
;; language servers, the `didOpen'/`didChange'/`didSave'/`didClose' sync,
;; diagnostics and the feature requests; this file mirrors buffer content and
;; edits to it and renders what it pushes back.
;;
;; Enable `exhub-lsp-global-mode' (or `exhub-lsp-mode' per buffer) in a mode
;; listed in `exhub-lsp-enabled-modes'.  Completion uses acm
;; (`exhub-lsp-completion', bound to \\[exhub-lsp-completion]); the built-in
;; acm LSP candidates are replaced by ExHub's while `exhub-lsp-mode' is on.

;;; Code:

(require 'cl-lib)
(require 'flymake)
(require 'json)
(require 'url-util)
(require 'xref)
(require 'exhub)

(defgroup exhub-lsp nil
  "ExHub LSP client."
  :group 'tools)

;; The acm completion adapter.  Optional: the read-only features work without
;; acm, so a missing adapter degrades to no completion menu rather than an
;; error at load time.
(require 'acm-backend-exhub-lsp nil t)

(defcustom exhub-lsp-enabled-modes '(elixir-mode elixir-ts-mode)
  "Major modes for which `exhub-lsp-mode' opens a backend session."
  :type '(repeat symbol))

(defcustom exhub-lsp-idle-delay 0.4
  "Seconds the backend waits before pushing diagnostics after a change."
  :type 'number)

(defcustom exhub-lsp-log nil
  "When non-nil, log backend responses and notifications to the echo area."
  :type 'boolean)

(defcustom exhub-lsp-language-id-alist
  '((elixir-mode . "elixir")
    (elixir-ts-mode . "elixir")
    (python-mode . "python")
    (python-ts-mode . "python")
    (js-mode . "javascript")
    (js-ts-mode . "javascript")
    (typescript-ts-mode . "typescript")
    (ruby-mode . "ruby")
    (ruby-ts-mode . "ruby")
    (go-mode . "go")
    (go-ts-mode . "go")
    (rust-mode . "rust")
    (rust-ts-mode . "rust")
    (c-mode . "c")
    (c-ts-mode . "c")
    (c++-mode . "cpp")
    (c++-ts-mode . "cpp"))
  "Map major modes to LSP language ids."
  :type '(alist :key-type symbol :value-type string))

;;; Buffer-local state

(defvar-local exhub-lsp--opened nil
  "Non-nil once this buffer has been opened on the backend.")

(defvar-local exhub-lsp--language-id nil
  "Language id sent to the backend for this buffer.")

(defvar-local exhub-lsp--diagnostics nil
  "Latest diagnostics for this buffer, as decoded alists.")

(defvar-local exhub-lsp--flymake-report nil
  "Flymake report function captured by the backend callback.")

;;; Helpers

(defun exhub-lsp--log (format-string &rest args)
  "Log FORMAT-STRING/ARGS when `exhub-lsp-log' is enabled."
  (when exhub-lsp-log
    (message "[exhub-lsp] %s" (apply #'format format-string args))))

(defun exhub-lsp--language-id ()
  "Return the LSP language id for the current buffer, or nil."
  (or (cdr (assq major-mode exhub-lsp-language-id-alist))
      (let ((name (symbol-name major-mode)))
        (when (string-match "\\(.+\\)-ts-mode\\'" name)
          (match-string 1 name)))))

(defun exhub-lsp--utf16-length (string)
  "Number of UTF-16 code units in STRING, as LSP `character` counts them."
  (let ((units 0))
    (dolist (char (string-to-list string) units)
      (setq units (+ units (if (> char #xFFFF) 2 1))))))

(defun exhub-lsp--point-to-position (point)
  "Encode POINT as an LSP position alist (0-based line and UTF-16 column).

LSP `character` counts UTF-16 code units from the line start, not display
columns, so tabs must NOT be expanded (`current-column' would do that and
shift every position on tab-indented lines)."
  (save-excursion
    (goto-char point)
    (list (cons "line" (1- (line-number-at-pos)))
          (cons "character"
                (exhub-lsp--utf16-length
                 (buffer-substring-no-properties (line-beginning-position) point))))))

(defun exhub-lsp--position-to-point (position)
  "Decode an LSP POSITION (parsed as an alist) into a buffer position."
  (exhub-lsp--lsp-point position))

(defun exhub-lsp--buffer-for-path (path)
  "Return the buffer visiting PATH, comparing truenames."
  (or (get-file-buffer path)
      (let ((truename (ignore-errors (file-truename path))))
        (cl-find-if (lambda (buffer)
                      (let ((file (buffer-file-name buffer)))
                        (and file truename
                             (equal (ignore-errors (file-truename file)) truename))))
                    (buffer-list)))))

;;; Outbound — document lifecycle

(defun exhub-lsp--open ()
  "Tell the backend to open the current buffer."
  (when (and buffer-file-name
             (not exhub-lsp--opened)
             (exhub-open-connection))
    (setq exhub-lsp--language-id (exhub-lsp--language-id))
    (exhub-call "lsp-bridge" "open-file"
                (expand-file-name buffer-file-name)
                (buffer-substring-no-properties (point-min) (point-max))
                `(("language-id" . ,exhub-lsp--language-id)
                  ("diag-idle" . ,(truncate (* exhub-lsp-idle-delay 1000)))))
    (setq exhub-lsp--opened t)
    (when (bound-and-true-p exhub-lsp-inlay-hints-mode)
      (exhub-lsp--inlay-refresh))
    (when (bound-and-true-p exhub-lsp-semantic-tokens-mode)
      (exhub-lsp--semantic-refresh))))

(defun exhub-lsp--after-change (beg end old-length)
  "Mirror an edit (BEG END OLD-LENGTH) to the backend, opening on first edit."
  (exhub-lsp--ensure-open)
  (when (and exhub-lsp--opened buffer-file-name)
    (exhub-call "lsp-bridge" "change-file"
                (expand-file-name buffer-file-name)
                `(("range" . (("start" . ,(exhub-lsp--point-to-position beg))
                              ("end" . ,(exhub-lsp--point-to-position end))))
                  ("rangeLength" . ,old-length)
                  ("text" . ,(buffer-substring-no-properties beg end))))))

(defun exhub-lsp--after-save ()
  "Tell the backend the current buffer was saved."
  (when (and exhub-lsp--opened buffer-file-name)
    (exhub-call "lsp-bridge" "save-file" (expand-file-name buffer-file-name))))

(defun exhub-lsp--close ()
  "Tell the backend the current buffer is closing."
  (when (and exhub-lsp--opened buffer-file-name (exhub-open-connection))
    (exhub-call "lsp-bridge" "close-file" (expand-file-name buffer-file-name)))
  (setq exhub-lsp--opened nil))

;;; Outbound — read-only features

;;; Lazy open

(defun exhub-lsp--ensure-open ()
  "Open the current buffer on the backend on first use, if not already open."
  (unless exhub-lsp--opened
    (exhub-lsp--open)))

(defun exhub-lsp--open-visible-buffers ()
  "Open servers for buffers currently displayed in a window."
  (dolist (window (window-list nil t))
    (let ((buffer (window-buffer window)))
      (when (and (buffer-live-p buffer) (not (minibufferp buffer)))
        (with-current-buffer buffer
          (when (and (bound-and-true-p exhub-lsp-mode)
                     buffer-file-name
                     (not exhub-lsp--opened))
            (exhub-lsp--ensure-open)))))))

(defun exhub-lsp--window-buffer-change (&rest _)
  "Global `window-buffer-change-functions' hook: open newly shown buffers."
  (run-with-idle-timer 0.1 nil #'exhub-lsp--open-visible-buffers))

(add-hook 'window-buffer-change-functions #'exhub-lsp--window-buffer-change)

(defun exhub-lsp--call (command &rest args)
  "Send COMMAND with ARGS to the backend for the current buffer.
Opens the buffer lazily on first use."
  (exhub-lsp--ensure-open)
  (when (and exhub-lsp--opened buffer-file-name)
    (apply #'exhub-call "lsp-bridge" command (expand-file-name buffer-file-name) args)))

(defun exhub-lsp--call-path (path command &rest args)
  "Send COMMAND with ARGS to the backend for PATH (not the current buffer)."
  (when (and path (exhub-open-connection))
    (apply #'exhub-call "lsp-bridge" command (expand-file-name path) args)))

(defun exhub-lsp-hover ()
  "Show documentation for the symbol at point."
  (interactive)
  (exhub-lsp--call "hover" (exhub-lsp--point-to-position (point))))

(defun exhub-lsp-find-definition ()
  "Jump to the definition of the symbol at point."
  (interactive)
  (exhub-lsp--call "find-define" (exhub-lsp--point-to-position (point))))

(defun exhub-lsp-find-type-definition ()
  "Jump to the type definition of the symbol at point."
  (interactive)
  (exhub-lsp--call "find-type-define" (exhub-lsp--point-to-position (point))))

(defun exhub-lsp-find-implementation ()
  "Jump to the implementation of the symbol at point."
  (interactive)
  (exhub-lsp--call "find-implementation" (exhub-lsp--point-to-position (point))))

(defun exhub-lsp-find-references ()
  "List references to the symbol at point."
  (interactive)
  (exhub-lsp--call "find-references" (exhub-lsp--point-to-position (point))))

(defun exhub-lsp-signature-help ()
  "Show the signature of the call at point."
  (interactive)
  (exhub-lsp--call "signature-help" (exhub-lsp--point-to-position (point))))

(defun exhub-lsp-document-symbols ()
  "Populate imenu with the current buffer's document symbols."
  (interactive)
  (exhub-lsp--call "document-symbol"))

(defun exhub-lsp-workspace-symbols (query)
  "Search the workspace for symbols matching QUERY."
  (interactive "sSymbol query: ")
  (exhub-lsp--call "workspace-symbol" query))

;;; Outbound — edits (Phase 4)

(defun exhub-lsp--format-options ()
  "LSP `FormattingOptions' derived from the current buffer."
  `(("tabSize" . ,tab-width)
    ("insertSpaces" . ,(if indent-tabs-mode :json-false t))))

(defun exhub-lsp--region-or-point (beg end)
  "Return an LSP range alist spanning BEG..END, or a zero-width range at point."
  (list (cons "start" (exhub-lsp--point-to-position beg))
        (cons "end" (exhub-lsp--point-to-position end))))

(defun exhub-lsp-rename (new-name)
  "Rename the symbol at point to NEW-NAME across the project."
  (interactive (list (read-string "Rename to: " (thing-at-point 'symbol t))))
  (exhub-lsp--call "prepare-rename" (exhub-lsp--point-to-position (point)))
  (exhub-lsp--call "rename" (exhub-lsp--point-to-position (point)) new-name))

(defun exhub-lsp-format ()
  "Format the current buffer with the language server."
  (interactive)
  (exhub-lsp--call "format" (exhub-lsp--format-options)))

(defun exhub-lsp-format-region (beg end)
  "Format the region BEG..END with the language server."
  (interactive "r")
  (exhub-lsp--call "range-format"
                   (exhub-lsp--region-or-point beg end)
                   (exhub-lsp--format-options)))

(defun exhub-lsp-code-action ()
  "Offer the language server's code actions for point or the active region."
  (interactive)
  (exhub-lsp--call "code-action"
                   (if (region-active-p)
                       (exhub-lsp--region-or-point (region-beginning) (region-end))
                     (exhub-lsp--region-or-point (point) (point)))))

;;; Outbound — call hierarchy (Phase 4)

(defvar-local exhub-lsp--call-hierarchy-direction "incoming"
  "Direction of the pending call-hierarchy flow (`incoming' or `outgoing').")

(defun exhub-lsp--call-hierarchy-start (direction)
  "Begin a call-hierarchy flow in DIRECTION for the function at point."
  (setq-local exhub-lsp--call-hierarchy-direction direction)
  (exhub-lsp--call "call-hierarchy-prepare" (exhub-lsp--point-to-position (point))))

(defun exhub-lsp-call-hierarchy-incoming ()
  "Show the calls that reach the function at point."
  (interactive)
  (exhub-lsp--call-hierarchy-start "incoming"))

(defun exhub-lsp-call-hierarchy-outgoing ()
  "Show the calls the function at point makes."
  (interactive)
  (exhub-lsp--call-hierarchy-start "outgoing"))

;;; Inbound — callbacks invoked by `exhub-eval'

(defun exhub-lsp-ready (path servers)
  "Backend readiness callback for PATH with SERVERS."
  (exhub-lsp--log "ready: %s (%s)" path servers))

(defun exhub-lsp-diagnostics (path diagnostics _count)
  "Render DIAGNOSTICS (a JSON string) for PATH."
  (let ((buffer (exhub-lsp--buffer-for-path path)))
    (when buffer
      (with-current-buffer buffer
        (setq exhub-lsp--diagnostics
              (condition-case nil
                  (append (json-parse-string diagnostics
                                             :object-type 'alist
                                             :array-type 'list)
                          nil)
                (error nil)))
        (exhub-lsp--report-diagnostics)))))

(defun exhub-lsp-notification (server method params)
  "Log a backend notification from SERVER (METHOD/PARAMS)."
  (exhub-lsp--log "%s %s %s" server method params))

(defun exhub-lsp-response (server id result)
  "Log a backend response from SERVER (ID/RESULT)."
  (exhub-lsp--log "%s #%s => %s" server id result))

(defun exhub-lsp-error-response (server id error)
  "Log a backend error response from SERVER (ID/ERROR)."
  (exhub-lsp--log "%s #%s error: %s" server id error))

(defun exhub-lsp-error (message)
  "Report a backend error MESSAGE."
  (message "[exhub-lsp] %s" message))

(defun exhub-lsp-pong ()
  "Backend liveness reply (no-op)."
  nil)

(defun exhub-lsp-message (message)
  "Show an informational backend MESSAGE."
  (message "[exhub-lsp] %s" message))

;;; Inbound — read-only feature results

(defun exhub-lsp--uri-to-path (uri)
  "Decode a `file://' URI into a filesystem path."
  (if (string-prefix-p "file://" uri)
      (url-unhex-string (substring uri (length "file://")))
    uri))

(defun exhub-lsp--location-path (loc)
  "Filesystem path of a decoded location alist LOC."
  (or (alist-get 'path loc)
      (exhub-lsp--uri-to-path (or (alist-get 'uri loc) ""))))

(defun exhub-lsp--location-position (loc)
  "Start position as (LINE . COLUMN) of a decoded location alist LOC."
  (let* ((range (or (alist-get 'selectionRange loc) (alist-get 'range loc)))
         (start (alist-get 'start range)))
    (cons (or (alist-get 'line start) 0)
          (or (alist-get 'character start) 0))))

(defun exhub-lsp--goto-location (loc)
  "Visit the file and position of a decoded location alist LOC."
  (let ((path (exhub-lsp--location-path loc))
        (pos (exhub-lsp--location-position loc)))
    (find-file path)
    (goto-char (exhub-lsp--lsp-point (list (cons 'line (car pos))
                                           (cons 'character (cdr pos)))))))

(defun exhub-lsp--loc-xref (loc)
  "Build an `xref-item' from a decoded location alist LOC."
  (let* ((path (exhub-lsp--location-path loc))
         (pos (exhub-lsp--location-position loc))
         (summary (or (file-relative-name path default-directory) path)))
    (xref-make summary (xref-make-file-location path (1+ (car pos)) (cdr pos)))))

(defun exhub-lsp--locations (_path kind json)
  "Handle a locations result of KIND, decoded from JSON."
  (let* ((locations (condition-case nil
                        (append (json-parse-string json :object-type 'alist
                                                  :array-type 'list)
                                nil)
                      (error nil)))
         (items (mapcar #'exhub-lsp--loc-xref locations)))
    (cond
     ((null locations) (message "[exhub-lsp] no %s found" kind))
     ((and (= (length locations) 1)
           (member kind '("definition" "type-definition" "implementation")))
      (exhub-lsp--goto-location (car locations)))
     (t (xref--show-xrefs items nil)))))

(defun exhub-lsp--hover (_path markdown)
  "Display hover MARKDOWN in a dedicated buffer."
  (with-current-buffer (get-buffer-create "*exhub-lsp-hover*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert markdown)
      (goto-char (point-min))
      (when (fboundp 'gfm-view-mode) (gfm-view-mode))
      (view-mode 1))
    (display-buffer (current-buffer))))

(defun exhub-lsp--symbol-marker (sym)
  "A marker at the start of document symbol alist SYM."
  (let* ((range (or (alist-get 'range sym)
                    (alist-get 'range (alist-get 'location sym))))
         (start (alist-get 'start range)))
    (save-excursion
      (goto-char (exhub-lsp--lsp-point start))
      (point-marker))))

(defun exhub-lsp--symbols-to-imenu (symbols)
  "Convert decoded document SYMBOLS into an imenu index."
  (mapcar
   (lambda (sym)
     (let ((name (or (alist-get 'name sym) "?"))
           (children (alist-get 'children sym)))
       (if children
           (cons name (exhub-lsp--symbols-to-imenu children))
         (cons name (exhub-lsp--symbol-marker sym)))))
   symbols))

(defun exhub-lsp--symbols (path json)
  "Install document symbols (JSON) for PATH into imenu."
  (let ((symbols (condition-case nil
                     (append (json-parse-string json :object-type 'alist
                                                :array-type 'list)
                             nil)
                   (error nil)))
        (buffer (exhub-lsp--buffer-for-path path)))
    (when buffer
      (with-current-buffer buffer
        (setq-local imenu--index-alist (exhub-lsp--symbols-to-imenu symbols))))
    (message "[exhub-lsp] %d symbol(s)" (length symbols))))

(defun exhub-lsp--workspace-symbols (query json)
  "Prompt for a workspace symbol matching QUERY and jump to it."
  (let ((symbols (condition-case nil
                     (append (json-parse-string json :object-type 'alist
                                                :array-type 'list)
                             nil)
                   (error nil))))
    (if (null symbols)
        (message "[exhub-lsp] no symbols matching %s" query)
      (let* ((names (delq nil (mapcar (lambda (s) (alist-get 'name s)) symbols)))
             (choice (completing-read (format "Symbol (%d): " (length symbols))
                                      names nil t))
             (sym (seq-find (lambda (s) (equal (alist-get 'name s) choice)) symbols))
             (loc (alist-get 'location sym)))
        (when loc
          (exhub-lsp--goto-location
           (list (cons 'path (exhub-lsp--uri-to-path (alist-get 'uri loc)))
                 (cons 'range (alist-get 'range loc)))))))))

(defun exhub-lsp--signature-help (_path json)
  "Display signature help (JSON) in the echo area."
  (let* ((result (condition-case nil
                     (json-parse-string json :object-type 'alist :array-type 'list)
                   (error nil)))
         (signatures (alist-get 'signatures result))
         (active (or (alist-get 'activeSignature result) 0))
         (signature (nth active signatures)))
    (message "%s" (or (alist-get 'label signature) "no signature"))))

;;; Inbound — edits (Phase 4)

(defvar-local exhub-lsp--prohibit-completion nil
  "Set after applying LSP edits so completion does not pop up mid-edit.")

(defun exhub-lsp--lsp-point (position)
  "Buffer point for an LSP POSITION alist, counting UTF-16 columns like acm.

Inverse of `exhub-lsp--point-to-position': walk `character' UTF-16 code
units from the line start instead of using `move-to-column' (which would
expand tabs and overshoot)."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (min most-positive-fixnum (or (alist-get 'line position) 0)))
      (let ((target (or (alist-get 'character position) 0))
            (units 0))
        (while (and (< units target) (not (eolp)))
          (let ((len (if (> (char-after) #xFFFF) 2 1)))
            (if (> (+ units len) target)
                (setq units target)
              (setq units (+ units len))
              (forward-char 1)))))
      (point))))

(defun exhub-lsp--edit-start (edit)
  "Buffer point at the start of TextEdit EDIT."
  (exhub-lsp--lsp-point (alist-get 'start (alist-get 'range edit))))

(defun exhub-lsp--apply-text-edits (edits)
  "Apply LSP TextEdits EDITS to the current buffer, last position first."
  (let ((inhibit-modification-hooks t))
    (dolist (edit (sort (copy-sequence edits)
                        (lambda (a b) (> (exhub-lsp--edit-start a) (exhub-lsp--edit-start b)))))
      (let* ((range (alist-get 'range edit))
             (beg (exhub-lsp--lsp-point (alist-get 'start range)))
             (end (exhub-lsp--lsp-point (alist-get 'end range)))
             (text (or (alist-get 'newText edit) "")))
        (goto-char beg)
        (delete-region beg end)
        (insert text)))))

(defun exhub-lsp--resync (buffer)
  "Send BUFFER's full content to the backend after out-of-band edits.

`exhub-lsp--apply-text-edits' runs under `inhibit-modification-hooks', so the
incremental `after-change-functions' sync does not fire; this replaces the
mirrored document wholesale."
  (when (and (buffer-live-p buffer)
             (buffer-local-value 'exhub-lsp--opened buffer)
             (buffer-file-name buffer))
    (with-current-buffer buffer
      (exhub-lsp--call "update-file"
                       (buffer-substring-no-properties (point-min) (point-max))))))

(defun exhub-lsp--apply-workspace-edit-obj (edit)
  "Apply a decoded WorkspaceEdit alist EDIT to its target buffers."
  (let ((changes
         (cond
          ((alist-get 'documentChanges edit)
           (mapcar (lambda (change)
                     (cons (alist-get 'uri (alist-get 'textDocument change))
                           (alist-get 'edits change)))
                   (alist-get 'documentChanges edit)))
          ((alist-get 'changes edit)
           (alist-get 'changes edit)))))
    (dolist (pair changes)
      (let ((path (exhub-lsp--uri-to-path (or (car pair) "")))
            (edits (cdr pair)))
        (when (and path edits (not (string-empty-p path)))
          (let ((buffer (find-file-noselect path)))
            (with-current-buffer buffer
              (exhub-lsp--apply-text-edits edits))
            (exhub-lsp--resync buffer))))))
  (setq-local exhub-lsp--prohibit-completion t))

(defun exhub-lsp--rename-range (path json)
  "Flash the rename range (JSON) reported for PATH."
  (let ((buffer (exhub-lsp--buffer-for-path path))
        (range (condition-case nil
                   (json-parse-string json :object-type 'alist)
                 (error nil))))
    (when (and buffer range)
      (with-current-buffer buffer
        (require 'pulse)
        (pulse-momentary-highlight-region
         (exhub-lsp--lsp-point (alist-get 'start range))
         (exhub-lsp--lsp-point (alist-get 'end range)))))))

(defun exhub-lsp--workspace-edit (json message)
  "Apply a WorkspaceEdit decoded from JSON, then report MESSAGE."
  (let ((edit (condition-case nil
                  (json-parse-string json :object-type 'alist :array-type 'list)
                (error nil))))
    (when edit
      (exhub-lsp--apply-workspace-edit-obj edit))
    (message "[exhub-lsp] %s" message)))

(defun exhub-lsp--format (path json)
  "Apply formatting TextEdits decoded from JSON to PATH's buffer."
  (let ((buffer (exhub-lsp--buffer-for-path path))
        (edits (condition-case nil
                   (json-parse-string json :object-type 'alist :array-type 'list)
                 (error nil))))
    (when (and buffer edits)
      (with-current-buffer buffer
        (exhub-lsp--apply-text-edits edits))
      (exhub-lsp--resync buffer))
    (message "[exhub-lsp] formatted")))

(defun exhub-lsp--execute-command (path command arguments)
  "Send COMMAND with ARGUMENTS to the server owning PATH."
  (exhub-lsp--call-path path "execute-command" command (or arguments [])))

(defun exhub-lsp--run-code-action (path action)
  "Run code action ACTION (a decoded alist) for PATH."
  (let ((edit (alist-get 'edit action))
        (command (alist-get 'command action))
        (arguments (alist-get 'arguments action)))
    (cond
     (edit
      (exhub-lsp--apply-workspace-edit-obj edit)
      (when (stringp command)
        (exhub-lsp--execute-command path command arguments)))
     ((and (listp command) (alist-get 'command command))
      (exhub-lsp--execute-command path
                                  (alist-get 'command command)
                                  (or (alist-get 'arguments command) arguments)))
     ((stringp command)
      (exhub-lsp--execute-command path command arguments))
     (t
      (message "[exhub-lsp] code action has nothing to run")))))

(defun exhub-lsp--code-actions (path json)
  "Prompt for one of the code actions decoded from JSON and run it."
  (let ((actions (condition-case nil
                     (json-parse-string json :object-type 'alist :array-type 'list)
                   (error nil))))
    (if (null actions)
        (message "[exhub-lsp] no code actions")
      (let* ((titles (delq nil (mapcar (lambda (action) (alist-get 'title action)) actions)))
             (choice (completing-read "Code action: " titles nil t))
             (action (seq-find (lambda (a) (equal (alist-get 'title a) choice)) actions)))
        (when action
          (exhub-lsp--run-code-action path action))))))

;;; Inbound — call hierarchy (Phase 4)

(defun exhub-lsp--call-hierarchy-json (json)
  "Parse call-hierarchy JSON into an alist list, or nil."
  (condition-case nil
      (json-parse-string json :object-type 'alist :array-type 'list)
    (error nil)))

(defun exhub-lsp--call-hierarchy-items (path json)
  "Prompt for one prepared call-hierarchy item and fetch its calls."
  (let* ((items (exhub-lsp--call-hierarchy-json json))
         (buffer (exhub-lsp--buffer-for-path path))
         (direction (if buffer
                        (buffer-local-value 'exhub-lsp--call-hierarchy-direction buffer)
                      "incoming")))
    (if (null items)
        (message "[exhub-lsp] no call hierarchy here")
      (let* ((names (delq nil (mapcar (lambda (item) (alist-get 'name item)) items)))
             (choice (completing-read "Call hierarchy of: " names nil t))
             (item (seq-find (lambda (i) (equal (alist-get 'name i) choice)) items)))
        (when item
          (exhub-lsp--call-path path
                                (if (equal direction "outgoing")
                                    "call-hierarchy-outgoing"
                                  "call-hierarchy-incoming")
                                item))))))

(defun exhub-lsp--call-hierarchy (path direction json)
  "Prompt for and visit a call from JSON (DIRECTION relative to PATH)."
  (let* ((calls (exhub-lsp--call-hierarchy-json json))
         (buffer (exhub-lsp--buffer-for-path path))
         (directory (if buffer
                        (buffer-local-value 'default-directory buffer)
                      default-directory)))
    (if (null calls)
        (message "[exhub-lsp] no %s calls" direction)
      (let* ((targets
              (delq nil
                    (mapcar (lambda (call)
                              (let ((target
                                     (alist-get (if (equal direction "outgoing") 'to 'from) call)))
                                (when target
                                  (cons (format "%s  %s"
                                                (or (alist-get 'name target) "?")
                                                (file-relative-name
                                                 (exhub-lsp--uri-to-path
                                                  (or (alist-get 'uri target) ""))
                                                 directory))
                                        target))))
                            calls)))
             (choice (completing-read (format "%s calls: " direction)
                                      (mapcar #'car targets) nil t))
             (target (cdr (assoc choice targets))))
        (when target
          (let ((range (or (alist-get 'selectionRange target) (alist-get 'range target))))
            (with-current-buffer
                (find-file-noselect (exhub-lsp--uri-to-path (or (alist-get 'uri target) "")))
              (goto-char (exhub-lsp--lsp-point (alist-get 'start range))))))))))

;;; Decorations — inlay hints & semantic tokens (Phase 4)

(defun exhub-lsp--json-list (json)
  "Parse JSON into an alist list, or nil."
  (condition-case nil
      (json-parse-string json :object-type 'alist :array-type 'list)
    (error nil)))

(defun exhub-lsp--whole-buffer-range ()
  "An LSP range spanning the whole buffer."
  (list (cons "start" (exhub-lsp--point-to-position (point-min)))
        (cons "end" (exhub-lsp--point-to-position (point-max)))))

(defface exhub-lsp-inlay-hint-face
  '((t :inherit shadow :slant italic))
  "Face for inlay hints."
  :group 'exhub-lsp)

(defcustom exhub-lsp-semantic-tokens-faces
  '(("namespace" . font-lock-keyword-face)
    ("type" . font-lock-type-face)
    ("class" . font-lock-type-face)
    ("interface" . font-lock-type-face)
    ("enum" . font-lock-type-face)
    ("struct" . font-lock-type-face)
    ("typeParameter" . font-lock-type-face)
    ("function" . font-lock-function-name-face)
    ("method" . font-lock-function-name-face)
    ("property" . font-lock-variable-name-face)
    ("variable" . font-lock-variable-name-face)
    ("parameter" . font-lock-variable-name-face)
    ("keyword" . font-lock-keyword-face)
    ("modifier" . font-lock-keyword-face)
    ("operator" . font-lock-keyword-face)
    ("macro" . font-lock-keyword-face)
    ("string" . font-lock-string-face)
    ("number" . font-lock-constant-face)
    ("comment" . font-lock-comment-face))
  "Alist mapping LSP semantic-token types to faces."
  :type '(alist :key-type string :value-type face)
  :group 'exhub-lsp)

;; -- Inlay hints -----------------------------------------------------------

(defvar-local exhub-lsp--inlay-overlays nil
  "Inlay-hint overlays in this buffer.")

(defvar-local exhub-lsp--inlay-timer nil
  "Idle timer for refreshing inlay hints in this buffer.")

(defun exhub-lsp--inlay-clear ()
  "Delete this buffer's inlay-hint overlays."
  (mapc #'delete-overlay exhub-lsp--inlay-overlays)
  (setq exhub-lsp--inlay-overlays nil))

(defun exhub-lsp--inlay-label (label)
  "Render an inlay-hint LABEL (a string or `InlayHintLabelPart[]')."
  (cond
   ((stringp label) label)
   ((listp label)
    (mapconcat (lambda (part)
                 (if (listp part) (or (alist-get 'value part) "") (format "%s" part)))
               label ""))
   ((null label) "")
   (t (format "%s" label))))

(defun exhub-lsp--inlay-request (buffer)
  "Request inlay hints for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and exhub-lsp-inlay-hints-mode exhub-lsp--opened)
        (exhub-lsp--call "inlay-hint" (exhub-lsp--whole-buffer-range))))))

(defun exhub-lsp--inlay-refresh ()
  "Schedule an inlay-hint refresh for the current buffer."
  (when exhub-lsp-inlay-hints-mode
    (when (timerp exhub-lsp--inlay-timer)
      (cancel-timer exhub-lsp--inlay-timer))
    (setq-local exhub-lsp--inlay-timer
                (run-with-idle-timer 0.4 nil #'exhub-lsp--inlay-request (current-buffer)))))

(defun exhub-lsp--inlay-hints (path json)
  "Render inlay hints decoded from JSON for PATH."
  (let ((buffer (exhub-lsp--buffer-for-path path))
        (hints (exhub-lsp--json-list json)))
    (when buffer
      (with-current-buffer buffer
        (exhub-lsp--inlay-clear)
        (dolist (hint hints)
          (let* ((pos (exhub-lsp--lsp-point (alist-get 'position hint)))
                 (label (exhub-lsp--inlay-label (alist-get 'label hint)))
                 (text (concat (if (alist-get 'paddingLeft hint) " " "")
                               label
                               (if (alist-get 'paddingRight hint) " " "")))
                 (overlay (make-overlay pos pos)))
            (overlay-put overlay 'after-string
                         (propertize text 'face 'exhub-lsp-inlay-hint-face))
            (overlay-put overlay 'exhub-lsp 'inlay)
            (push overlay exhub-lsp--inlay-overlays)))))))

(define-minor-mode exhub-lsp-inlay-hints-mode
  "Show the language server's inlay hints in this buffer."
  :lighter " InH"
  (if exhub-lsp-inlay-hints-mode
      (progn
        (add-hook 'after-change-functions #'exhub-lsp--inlay-refresh nil t)
        (exhub-lsp--inlay-refresh))
    (remove-hook 'after-change-functions #'exhub-lsp--inlay-refresh t)
    (when (timerp exhub-lsp--inlay-timer)
      (cancel-timer exhub-lsp--inlay-timer))
    (setq-local exhub-lsp--inlay-timer nil)
    (exhub-lsp--inlay-clear)))

;; -- Semantic tokens -------------------------------------------------------

(defvar-local exhub-lsp--semantic-overlays nil
  "Semantic-token overlays in this buffer.")

(defvar-local exhub-lsp--semantic-timer nil
  "Idle timer for refreshing semantic tokens in this buffer.")

(defun exhub-lsp--semantic-clear ()
  "Delete this buffer's semantic-token overlays."
  (mapc #'delete-overlay exhub-lsp--semantic-overlays)
  (setq exhub-lsp--semantic-overlays nil))

(defun exhub-lsp--semantic-request (buffer)
  "Request semantic tokens for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and exhub-lsp-semantic-tokens-mode exhub-lsp--opened)
        (exhub-lsp--call "semantic-tokens")))))

(defun exhub-lsp--semantic-refresh ()
  "Schedule a semantic-token refresh for the current buffer."
  (when exhub-lsp-semantic-tokens-mode
    (when (timerp exhub-lsp--semantic-timer)
      (cancel-timer exhub-lsp--semantic-timer))
    (setq-local exhub-lsp--semantic-timer
                (run-with-idle-timer 0.4 nil #'exhub-lsp--semantic-request (current-buffer)))))

(defun exhub-lsp--semantic-tokens (path json)
  "Render semantic tokens decoded from JSON for PATH."
  (let ((buffer (exhub-lsp--buffer-for-path path))
        (tokens (exhub-lsp--json-list json)))
    (when buffer
      (with-current-buffer buffer
        (exhub-lsp--semantic-clear)
        (dolist (token tokens)
          (let* ((line (alist-get 'line token))
                 (character (alist-get 'character token))
                 (length (alist-get 'length token))
                 (face (cdr (assoc (alist-get 'type token) exhub-lsp-semantic-tokens-faces))))
            (when face
              (let ((beg (exhub-lsp--lsp-point (list (cons 'line line) (cons 'character character))))
                    (end (exhub-lsp--lsp-point
                          (list (cons 'line line) (cons 'character (+ character length))))))
                (when (< beg end)
                  (let ((overlay (make-overlay beg end)))
                    (overlay-put overlay 'face face)
                    (overlay-put overlay 'exhub-lsp 'semantic)
                    (push overlay exhub-lsp--semantic-overlays)))))))))))

(define-minor-mode exhub-lsp-semantic-tokens-mode
  "Paint the language server's semantic tokens in this buffer."
  :lighter " Sem"
  (if exhub-lsp-semantic-tokens-mode
      (progn
        (add-hook 'after-change-functions #'exhub-lsp--semantic-refresh nil t)
        (exhub-lsp--semantic-refresh))
    (remove-hook 'after-change-functions #'exhub-lsp--semantic-refresh t)
    (when (timerp exhub-lsp--semantic-timer)
      (cancel-timer exhub-lsp--semantic-timer))
    (setq-local exhub-lsp--semantic-timer nil)
    (exhub-lsp--semantic-clear)))

;;; Flymake backend

(defun exhub-lsp--to-flymake (diagnostic)
  "Convert a decoded DIAGNOSTIC alist into a flymake diagnostic."
  (let* ((range (alist-get 'range diagnostic))
         (start (alist-get 'start range))
         (end (alist-get 'end range))
         (beg (min (exhub-lsp--position-to-point start) (point-max)))
         (fin (min (exhub-lsp--position-to-point end) (point-max)))
         (severity (alist-get 'severity diagnostic)))
    (flymake-make-diagnostic
     (current-buffer) beg (max beg fin)
     (pcase severity
       (1 :error)
       (2 :warning)
       ((or 3 4) :note)
       (_ :warning))
     (format "%s" (or (alist-get 'message diagnostic) "")))))

(defun exhub-lsp--report-diagnostics ()
  "Hand the current buffer's diagnostics to the flymake report function."
  (when (functionp exhub-lsp--flymake-report)
    (funcall exhub-lsp--flymake-report
             (mapcar #'exhub-lsp--to-flymake exhub-lsp--diagnostics))))

(defun exhub-lsp-flymake (report-fn &rest _args)
  "Flymake backend: capture REPORT-FN and report current diagnostics."
  (setq-local exhub-lsp--flymake-report report-fn)
  (exhub-lsp--report-diagnostics))

;;; Minor mode

(defun exhub-lsp--enable ()
  "Enable the ExHub LSP client in the current buffer."
  (add-hook 'after-change-functions #'exhub-lsp--after-change nil t)
  (add-hook 'after-save-hook #'exhub-lsp--after-save nil t)
  (add-hook 'kill-buffer-hook #'exhub-lsp--close nil t)
  (add-hook 'flymake-diagnostic-functions #'exhub-lsp-flymake nil t)
  (flymake-mode 1)
  ;; Lazy: open only when the buffer is displayed, edited or a feature is used.
  (exhub-lsp--open-visible-buffers)
  (when (fboundp 'exhub-lsp--completion-enable)
    (exhub-lsp--completion-enable))
  (when (fboundp 'exhub-lsp--maybe-complete)
    (add-hook 'post-self-insert-hook #'exhub-lsp--maybe-complete nil t)))

(defun exhub-lsp--disable ()
  "Disable the ExHub LSP client in the current buffer."
  (remove-hook 'after-change-functions #'exhub-lsp--after-change t)
  (remove-hook 'after-save-hook #'exhub-lsp--after-save t)
  (remove-hook 'kill-buffer-hook #'exhub-lsp--close t)
  (remove-hook 'flymake-diagnostic-functions #'exhub-lsp-flymake t)
  (when (fboundp 'exhub-lsp--maybe-complete)
    (remove-hook 'post-self-insert-hook #'exhub-lsp--maybe-complete t))
  (when (fboundp 'exhub-lsp--completion-clean)
    (exhub-lsp--completion-clean))
  (when (bound-and-true-p exhub-lsp-inlay-hints-mode)
    (exhub-lsp-inlay-hints-mode -1))
  (when (bound-and-true-p exhub-lsp-semantic-tokens-mode)
    (exhub-lsp-semantic-tokens-mode -1))
  (exhub-lsp--close)
  (setq exhub-lsp--diagnostics nil))

(defvar exhub-lsp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") #'exhub-lsp-find-definition)
    (define-key map (kbd "M-?") #'exhub-lsp-find-references)
    (define-key map (kbd "M-,") #'exhub-lsp-find-implementation)
    (define-key map (kbd "C-c C-t") #'exhub-lsp-find-type-definition)
    (define-key map (kbd "C-c h") #'exhub-lsp-hover)
    (define-key map (kbd "C-c s") #'exhub-lsp-document-symbols)
    (define-key map (kbd "C-c S") #'exhub-lsp-workspace-symbols)
    (define-key map (kbd "C-c C-c") #'exhub-lsp-completion)
    (define-key map (kbd "C-c C-r") #'exhub-lsp-rename)
    (define-key map (kbd "C-c C-f") #'exhub-lsp-format)
    (define-key map (kbd "C-c C-a") #'exhub-lsp-code-action)
    (define-key map (kbd "C-c C-i") #'exhub-lsp-call-hierarchy-incoming)
    (define-key map (kbd "C-c C-o") #'exhub-lsp-call-hierarchy-outgoing)
    map)
  "Keymap for `exhub-lsp-mode'.")

;;;###autoload
(define-minor-mode exhub-lsp-mode
  "ExHub LSP client: flymake diagnostics, hover, navigation and symbols."
  :lighter " ExLSP"
  :keymap exhub-lsp-mode-map
  (if exhub-lsp-mode
      (exhub-lsp--enable)
    (exhub-lsp--disable)))

(defun exhub-lsp--turn-on ()
  "Enable `exhub-lsp-mode' in buffers whose major mode is supported."
  (when (apply #'derived-mode-p exhub-lsp-enabled-modes)
    (exhub-lsp-mode 1)))

;;;###autoload
(define-globalized-minor-mode exhub-lsp-global-mode
  exhub-lsp-mode
  exhub-lsp--turn-on)

(provide 'exhub-lsp)
;;; exhub-lsp.el ends here