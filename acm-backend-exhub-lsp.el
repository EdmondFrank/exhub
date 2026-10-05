;;; acm-backend-exhub-lsp.el --- acm adapter for ExHub LSP completion -*- lexical-binding: t; -*-

;;; Commentary:
;; Bridge ExHub's `lsp-bridge' completion output into acm's built-in LSP
;; backend — the completion half of the option-B front end (`exhub-lsp.el').
;;
;; `acm-update-candidates' hardcodes which `acm-backend-*-candidates'
;; functions it calls, so a brand-new backend name would never be consulted
;; without patching acm.  Instead — exactly as `lsp-bridge.el' does for its
;; Python backend — this adapter fills acm's LSP buffer-locals
;; (`acm-backend-lsp-items', `acm-backend-lsp-cache-candidates',
;; `acm-backend-lsp-completion-position', `acm-backend-lsp-server-names' and
;; `acm-backend-lsp-completion-trigger-characters') and calls `acm-update'.
;; acm then renders, expands (snippets, auto-import edits) and documents the
;; candidates; `exhub-lsp--completion-doc' feeds `completionItem/resolve'
;; results back into the doc frame.
;;
;; ExHub completion and the Python lsp-bridge share those buffer-locals, so
;; enable only one of them in a buffer (they do not need to fight: the ExHub
;; client only touches buffers running `exhub-lsp-mode').

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'acm)
(require 'acm-backend-lsp)

(declare-function exhub-lsp--call "exhub-lsp")
(declare-function exhub-lsp--buffer-for-path "exhub-lsp")
(declare-function exhub-lsp--point-to-position "exhub-lsp")

;; acm's own LSP backend initialises these with `setq-local' but never
;; `defvar's them; declare them so byte-compiling this file stays clean.
(defvar acm-backend-lsp-items nil)
(defvar acm-backend-lsp-cache-candidates nil)
(defvar acm-backend-lsp-completion-position nil)
(defvar acm-backend-lsp-completion-trigger-characters nil)
(defvar acm-backend-lsp-server-names nil)
(defvar acm-backend-lsp-filepath nil)

;; Raw LSP items (key -> item) per server, kept so `completionItem/resolve'
;; can send the exact item back.  acm's own `acm-backend-lsp-items' holds the
;; *transformed* candidates it renders.
(defvar-local exhub-lsp--raw-items nil)

(defcustom exhub-lsp-completion-auto nil
  "When non-nil, pop up completion automatically after `self-insert'."
  :type 'boolean
  :group 'exhub-lsp)

;;; JSON helpers

(defun exhub-lsp--json-plist (json)
  "Parse JSON into plists, or nil for an empty/blank value.

Objects become plists with keyword keys and arrays become lists, which is the
exact shape acm expects, so nested `textEdit'/`range' objects are usable
as-is."
  (when (and (stringp json) (not (string-empty-p json)))
    (condition-case nil
        (json-parse-string json :object-type 'plist :array-type 'list)
      (error nil))))

(defun exhub-lsp--item-key (key)
  "Normalise a parsed item-map KEY (a keyword) back to a string."
  (if (keywordp key) (substring (symbol-name key) 1) (format "%s" key)))

;;; Outbound

(defun exhub-lsp--completion-opts ()
  "acm's current LSP completion settings, as the backend `completion' opts."
  `(("match-mode" . ,(if (boundp 'acm-backend-lsp-match-mode)
                         acm-backend-lsp-match-mode "fuzzy"))
    ("case-mode" . ,(if (boundp 'acm-backend-lsp-case-mode)
                        acm-backend-lsp-case-mode "ignore"))
    ("items-limit" . ,(if (boundp 'acm-backend-lsp-candidates-max-number)
                          acm-backend-lsp-candidates-max-number 100))
    ("auto-import" . ,(if (and (boundp 'acm-backend-lsp-enable-auto-import)
                               acm-backend-lsp-enable-auto-import)
                          t :json-false))
    ("display-label-max-length" . ,(if (boundp 'acm-backend-lsp-candidate-max-length)
                                       acm-backend-lsp-candidate-max-length 60))
    ("block-kind-list" . ,(when (and (boundp 'acm-backend-lsp-block-kind-list)
                                     acm-backend-lsp-block-kind-list)
                            (mapcar #'downcase acm-backend-lsp-block-kind-list)))))

(defun exhub-lsp--prefix ()
  "Symbol prefix before point, or an empty string."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if bounds (buffer-substring-no-properties (car bounds) (point)) "")))

(defun exhub-lsp-completion ()
  "Pop up ExHub LSP completion candidates at point."
  (interactive)
  (exhub-lsp--call "completion"
                   (exhub-lsp--point-to-position (point))
                   (if (char-before) (char-to-string (char-before)) "")
                   (exhub-lsp--prefix)
                   (exhub-lsp--completion-opts)))

(defun exhub-lsp--maybe-complete ()
  "Auto-trigger completion after `self-insert' when `exhub-lsp-completion-auto'."
  (when (and exhub-lsp-completion-auto
             (not (memq (char-before) '(?\s ?\t ?\n ?\( ?\) ?\" ?\')))
             (not (nth 3 (syntax-ppss)))
             (not (nth 4 (syntax-ppss))))
    (exhub-lsp-completion)))

;;; Buffer lifecycle

(defun exhub-lsp--completion-enable ()
  "Set up acm's LSP buffer-locals for ExHub completion in this buffer."
  (setq-local acm-backend-lsp-fetch-completion-item-func #'exhub-lsp--fetch-completion-item-info)
  (setq-local acm-backend-lsp-items (make-hash-table :test 'equal))
  (setq-local acm-backend-lsp-cache-candidates nil)
  (setq-local acm-backend-lsp-completion-position nil)
  (setq-local acm-backend-lsp-completion-trigger-characters nil)
  (setq-local acm-backend-lsp-server-names nil)
  (setq-local acm-backend-lsp-fetch-completion-item-ticker nil)
  (setq-local acm-backend-lsp-filepath buffer-file-name)
  (setq-local exhub-lsp--raw-items (make-hash-table :test 'equal)))

(defun exhub-lsp--completion-clean ()
  "Reset the acm LSP state for this buffer."
  (setq-local acm-backend-lsp-items (make-hash-table :test 'equal))
  (setq-local acm-backend-lsp-cache-candidates nil)
  (setq-local acm-backend-lsp-server-names nil)
  (setq-local exhub-lsp--raw-items (make-hash-table :test 'equal)))

;;; Inbound callbacks (invoked by `exhub-eval')

(defun exhub-lsp--completion (path server candidates-json items-json meta-json)
  "Record ExHub completion for PATH.

SERVER produced the candidates; CANDIDATES-JSON, ITEMS-JSON and META-JSON are
the backend's payload.  Candidates are stored on acm's LSP buffer-locals and
the menu is refreshed."
  (let ((buffer (exhub-lsp--buffer-for-path path)))
    (when buffer
      (with-current-buffer buffer
        (let ((candidates (exhub-lsp--json-plist candidates-json))
              (items (exhub-lsp--json-plist items-json))
              (meta (exhub-lsp--json-plist meta-json))
              (candidate-table (make-hash-table :test 'equal))
              (raw-table (make-hash-table :test 'equal)))
          (setq-local acm-backend-lsp-cache-candidates nil)
          (setq-local acm-backend-lsp-completion-position (plist-get meta :position))
          (setq-local acm-backend-lsp-completion-trigger-characters
                      (plist-get meta :trigger-characters))
          (setq-local acm-backend-lsp-server-names
                      (or (plist-get meta :server-names) (list server)))
          (setq-local acm-backend-lsp-fetch-completion-item-ticker nil)

          (when (or (not (boundp 'exhub-lsp--raw-items))
                    (not (hash-table-p exhub-lsp--raw-items)))
            (setq-local exhub-lsp--raw-items (make-hash-table :test 'equal)))

          ;; acm renders its candidates from `acm-backend-lsp-items' (a hash of
          ;; key -> candidate); the raw LSP items ride alongside, keyed the same
          ;; way, for `completionItem/resolve'.
          (dolist (candidate candidates)
            (when-let* ((key (plist-get candidate :key)))
              (puthash key candidate candidate-table)))
          (cl-loop for (key item) on items by #'cddr
                   do (puthash (exhub-lsp--item-key key) item raw-table))
          (puthash server candidate-table acm-backend-lsp-items)
          (puthash server raw-table exhub-lsp--raw-items)

          (acm-update))))))

(defun exhub-lsp--completion-doc (path server key documentation edits-json)
  "Store resolved DOCUMENTATION/EDITS for KEY from SERVER in PATH's buffer."
  (let ((buffer (exhub-lsp--buffer-for-path path)))
    (when buffer
      (with-current-buffer buffer
        (when-let* ((server-items (gethash server acm-backend-lsp-items))
                    (item (gethash key server-items)))
          (let ((edits (exhub-lsp--json-plist edits-json)))
            (when edits
              (plist-put item :additionalTextEdits edits))
            (when (and documentation (not (string-empty-p documentation)))
              (plist-put item :documentation documentation)))
          ;; `plist-put' returns a new list when it adds a property (e.g.
          ;; `:documentation'), so store the (possibly new) ITEM back before acm
          ;; reads it from `acm-backend-lsp-items' via its `candidate-doc' func.
          (puthash key item server-items))

        (if (and documentation (not (string-empty-p documentation)))
            (acm-doc-try-show t)
          (acm-doc-hide))))))

(defun exhub-lsp--fetch-completion-item-info (candidate)
  "Ask the backend to `completionItem/resolve' CANDIDATE for its documentation."
  (let* ((key (plist-get candidate :key))
         (server (plist-get candidate :server))
         (item (and (boundp 'exhub-lsp--raw-items)
                    (hash-table-p exhub-lsp--raw-items)
                    (gethash key (gethash server exhub-lsp--raw-items)))))
    (when (and key server item
               (not (equal acm-backend-lsp-fetch-completion-item-ticker
                           (list acm-backend-lsp-filepath key server))))
      (exhub-lsp--call "completion-item-resolve" key server item)
      (setq-local acm-backend-lsp-fetch-completion-item-ticker
                  (list acm-backend-lsp-filepath key server)))))

(provide 'acm-backend-exhub-lsp)
;;; acm-backend-exhub-lsp.el ends here