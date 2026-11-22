;;; exhub-keys.el --- Keybinding tree for ExHub and Emacs tools -*- lexical-binding: t; -*-

;; Copyright (C) 2023 EdmondFrank

;; Author: Edmond Frank <EdmondFrank>
;; Version: 0.1
;; URL: https://github.com/edmondfrank/exhub.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; One discoverable namespace for the ExHub commands that have no short key of
;; their own:
;;
;;   C-c e c  chat                  C-c e s  search (blink-search, probe)
;;   C-c e t  translate             C-c e g  Gitee dashboard
;;   C-c e f  inline completion     C-c e v  vault
;;   C-c e l  LSP                   C-c e d  document preview
;;   C-c e a  agent                 C-c e m  model
;;   C-c e M  MCP servers           C-c e o  operations (log, reload, restart)
;;   C-c e ?  show the whole tree   (which-key)
;;
;; Design notes:
;;
;; * The tree only ADDS bindings.  Short chords that already exist are left
;;   untouched: `C-x b' (blink-search), `C-|' (translate insert), `C-c s ...'
;;   (probe), `C-c g d' (Gitee dashboard), `M-q' (agent-shell menu), the
;;   `C-b ...' keys of `exhub-lsp-mode-map' and `exhub-fim-active-mode-map',
;;   and `C-c v ...' in `exhub-vault-mode-map'.  The tree is the canonical
;;   index; mode maps keep the fast local chords.
;; * Prefixes are bound as named `(LABEL . KEYMAP)' prefixes and leaves are
;;   labelled through `which-key-add-key-based-replacements', because several
;;   ExHub commands have no docstring of their own.
;; * `envrc-mode-map' also binds `C-c e' locally (in buffers under a directory
;;   holding an .envrc).  This tree is installed with `global-set-key', so the
;;   buffer-local map wins there.  Change `exhub-keys-prefix' if that clashes.
;; * Restart and release ask for confirmation: restarting the BEAM VM cuts
;;   in-flight LLM proxy traffic (see AGENTS.md).
;;
;; The whole tree lives in `exhub-keys-tree'.  Edit that list, then
;; `M-x exhub-keys-rebuild' (or reload this file) to pick up the change.

;;; Code:

(require 'exhub)

(declare-function which-key-add-key-based-replacements "which-key"
                  (key-sequence replacement &rest more))
(declare-function which-key-show-full-keymap "which-key" (keymap))

(defgroup exhub-keys nil
  "Keybinding tree for ExHub commands."
  :group 'applications
  :prefix "exhub-keys-")

(defcustom exhub-keys-prefix "C-c e"
  "Prefix key of the ExHub command tree.
A `kbd'-style key sequence.  Set this before enabling
`exhub-keys-mode' (or re-enable it) to move the whole tree."
  :type 'string)

;;; The tree ----------------------------------------------------------------
;;
;; Every entry is (LABEL KEY TARGET), where TARGET is either a command symbol
;; or a nested list of the same shape.  KEYS are `kbd' strings.

(defconst exhub-keys-tree
  '(("chat" "c"
     (("chat" "c" exhub-chat)
      ("multiline prompt" "m" exhub-chat-with-multiline)
      ("multiline prompt, temp buffer" "M" exhub-chat-with-multiline-with-temp-buffer)
      ("temp buffer" "b" exhub-chat-with-temp-buffer)
      ("with MCP tools" "t" exhub-chat-with-tools)
      ("with git tools" "g" exhub-chat-with-git)
      ("code actions" "a"
       (("generate code" "g" exhub-chat-generate-code)
        ("adjust code" "a" exhub-chat-adjust-code)
        ("explain code" "e" exhub-chat-explain-code)
        ("comment code" "c" exhub-chat-comment-code)
        ("format code" "f" exhub-chat-format-code)
        ("refactor code" "r" exhub-chat-refactory-code)
        ("choose drafts" "d" exhub-chat-choose-drafts)
        ("polish document" "p" exhub-chat-polish-document)
        ("improve document" "i" exhub-chat-improve-document)
        ("optimize prompts" "o" exhub-chat-optimize-prompts)
        ("translate into Chinese" "z" exhub-chat-translate-into-chinese)
        ("translate into English" "E" exhub-chat-translate-into-english)))
      ("git and review" "r"
       (("commit message" "c" exhub-chat-generate-commit-message)
        ("review from Magit diff" "m" exhub-chat-generate-review-from-magit)
        ("pull request description" "p" exhub-chat-generate-pull-desc)
        ("pull request review" "P" exhub-chat-generate-pull-review)
        ("compare review" "v" exhub-chat-generate-compare-review)
        ("compare preview" "V" exhub-compare-preview)))))
    ("translate" "t"
     (("insert translation" "i" exhub-translate-insert)
      ("insert original (comment/string)" "I" exhub-translate-insert-original-translation)
      ("replace symbol" "r" exhub-translate-replace)
      ("posframe" "p" exhub-translate-posframe)
      ("fix grammar" "f" exhub-translate-fix-grammar)
      ("replace with Chinese" "z" exhub-translate-replace-zh)
      ("replace with Traditional Chinese" "Z" exhub-translate-replace-zh-tw)
      ("style variants" "v"
       (("insert camel" "c" exhub-translate-insert-with-camel)
        ("replace camel" "C" exhub-translate-replace-with-camel)
        ("insert line" "l" exhub-translate-insert-with-line)
        ("replace line" "L" exhub-translate-replace-with-line)
        ("insert underline" "u" exhub-translate-insert-with-underline)
        ("replace underline" "U" exhub-translate-replace-with-underline)))))
    ("inline completion (FIM)" "f"
     (("complete (dropdown)" "c" exhub-fim-complete)
      ("complete (minibuffer)" "m" exhub-fim-complete-with-minibuffer)
      ("show suggestion" "s" exhub-fim-show-suggestion)
      ("accept suggestion" "TAB" exhub-fim-accept-suggestion)
      ("accept N lines" "l" exhub-fim-accept-suggestion-line)
      ("next suggestion" "n" exhub-fim-next-suggestion)
      ("previous suggestion" "p" exhub-fim-previous-suggestion)
      ("dismiss suggestion" "d" exhub-fim-dismiss-suggestion)
      ("toggle auto suggestions" "t" exhub-fim-auto-suggestion-mode)
      ("configure provider" "P" exhub-fim-configure-provider)))
    ("lsp" "l"
     (("definition" "d" exhub-lsp-find-definition)
      ("references" "D" exhub-lsp-find-references)
      ("implementation" "i" exhub-lsp-find-implementation)
      ("type definition" "t" exhub-lsp-find-type-definition)
      ("hover" "h" exhub-lsp-hover)
      ("document symbols" "s" exhub-lsp-document-symbols)
      ("workspace symbols" "S" exhub-lsp-workspace-symbols)
      ("completion" "c" exhub-lsp-completion)
      ("rename" "r" exhub-lsp-rename)
      ("format buffer" "f" exhub-lsp-format)
      ("code action" "a" exhub-lsp-code-action)
      ("signature help" "n" exhub-lsp-signature-help)
      ("call hierarchy" "o"
       (("incoming calls" "i" exhub-lsp-call-hierarchy-incoming)
        ("outgoing calls" "o" exhub-lsp-call-hierarchy-outgoing)))
      ("toggles" "T"
       (("lsp-mode (this buffer)" "m" exhub-lsp-mode)
        ("global mode" "g" exhub-lsp-global-mode)
        ("auto completion" "a" exhub-lsp-auto-mode)
        ("inlay hints" "h" exhub-lsp-inlay-hints-mode)
        ("semantic tokens" "s" exhub-lsp-semantic-tokens-mode)))))
    ("agent" "a"
     (("chat with an agent" "c" exhub-agent-chat)
      ("reply to an agent" "r" exhub-agent-tool-reply)
      ("start agent" "s" exhub-agent-start)
      ("stop agent" "k" exhub-agent-kill)))
    ("search" "s"
     (("blink-search" "b" blink-search-exhub)
      ("probe search" "s" exhub-probe-search)
      ("probe at point" "." exhub-probe-at-point)
      ("probe glob" "g" exhub-probe-glob)
      ("probe content" "f" exhub-probe-content)))
    ("gitee dashboard" "g"
     (("open dashboard" "d" exhub-gitee-dashboard)
      ("set enterprise" "e" exhub-gitee-dashboard-set-enterprise)
      ("refresh" "r" exhub-gitee-dashboard-refresh)
      ("browse at point" "b" exhub-gitee-dashboard-browse)
      ("open detail at point" "o" exhub-gitee-dashboard-open-detail)
      ("toggle images" "i" exhub-gitee-dashboard--image-toggle)
      ("clear image cache" "c" exhub-gitee-dashboard-clear-image-cache)))
    ("vault" "v"
     (("insert secret" "i" exhub-vault-insert-secret)
      ("decrypt and copy" "c" exhub-vault-decrypt-and-copy)
      ("decrypt and show" "s" exhub-vault-decrypt-and-show)
      ("toggle vault mode" "m" exhub-vault-mode)))
    ("document preview" "d"
     (("preview Markdown (posframe)" "p" exhub-preview-markdown)
      ("preview Markdown in EWW" "w" exhub-preview-markdown-in-eww)))
    ("model" "m"
     (("switch model" "m" exhub-switch-model)
      ("show current model" "c" exhub-model)
      ("refresh model catalog" "r" exhub-refresh-models)))
    ("MCP servers" "M"
     (("start server" "s" exhub-keys-mcp-start-server)
      ("stop server" "x" exhub-keys-mcp-stop-server)
      ("stop all servers" "X" exhub-stop-all-mcp-servers)))
    ("operations" "o"
     (("show backend log" "l" exhub-keys-open-log)
      ("hot reload modules" "h" exhub-reload)
      ("reload API keys" "k" exhub-reload-keys)
      ("restart websocket" "w" exhub-restart-websocket)
      ("backend version" "v" exhub-keys-status)
      ("restart backend (confirm, drops traffic)" "b" exhub-keys-restart)
      ("build release (confirm)" "r" exhub-keys-release)
      ("soft restart (confirm)" "s" exhub-keys-soft-restart)
      ("hard restart (confirm)" "S" exhub-keys-hard-restart))))
  "The ExHub key tree: a list of (LABEL KEY TARGET) entries.
TARGET is a command symbol or a nested tree of the same shape.")

;;; Building the maps -------------------------------------------------------

(defun exhub-keys--build (tree)
  "Return a sparse keymap built from TREE."
  (let ((map (make-sparse-keymap)))
    (dolist (entry tree)
      (let ((label (nth 0 entry))
            (key (nth 1 entry))
            (target (nth 2 entry)))
        (when key
          (if (symbolp target)
              (define-key map (kbd key) target)
            (define-key map (kbd key)
                        (cons label (exhub-keys--build target)))))))
    map))

(defun exhub-keys--root-map ()
  "Return the root keymap, including the help keys."
  (let ((map (exhub-keys--build exhub-keys-tree)))
    (define-key map (kbd "?") #'exhub-keys-help)
    (define-key map (kbd "C-h") #'exhub-keys-help)
    map))

(defvar exhub-keys-map nil
  "Root keymap of the ExHub command tree.")

;; A plain `setq' (not the `defvar' value form) so that reloading this file
;; picks up edits to `exhub-keys-tree'.
(setq exhub-keys-map (exhub-keys--root-map))

(defvar exhub-keys--installed-prefix nil
  "Prefix currently installed by `exhub-keys-mode', or nil.")

;;; which-key labels -------------------------------------------------------

(defun exhub-keys--forget-label (key)
  "Drop the which-key replacement previously registered for KEY."
  (when (boundp 'which-key-replacement-alist)
    (let ((pattern (concat "\\`" (regexp-quote key) "\\'")))
      (setq which-key-replacement-alist
            (seq-remove (lambda (entry)
                          (equal (car entry) (list pattern)))
                        which-key-replacement-alist)))))

(defun exhub-keys--label (prefix tree)
  "Register which-key labels for TREE below PREFIX."
  (dolist (entry tree)
    (let ((label (nth 0 entry))
          (key (nth 1 entry))
          (target (nth 2 entry)))
      (when (and key label)
        (let ((full (if (string-empty-p prefix) key (concat prefix " " key))))
          (exhub-keys--forget-label full)
          (which-key-add-key-based-replacements full label)
          (when (listp target)
            (exhub-keys--label full target)))))))

(defun exhub-keys--install ()
  "Bind `exhub-keys-prefix' to the tree and label it for which-key."
  (exhub-keys--uninstall)
  (global-set-key (kbd exhub-keys-prefix) exhub-keys-map)
  (setq exhub-keys--installed-prefix exhub-keys-prefix)
  (when (require 'which-key nil t)
    (let ((prefix (key-description (kbd exhub-keys-prefix))))
      (which-key-add-key-based-replacements prefix "ExHub")
      (exhub-keys--label prefix exhub-keys-tree))))

(defun exhub-keys--uninstall ()
  "Remove the binding installed by `exhub-keys-mode'."
  (when exhub-keys--installed-prefix
    (global-unset-key (kbd exhub-keys--installed-prefix))
    (setq exhub-keys--installed-prefix nil)))

;;; Help and small commands ------------------------------------------------

(defun exhub-keys-help ()
  "Show the whole ExHub key tree."
  (interactive)
  (if (and (require 'which-key nil t)
           (fboundp 'which-key-show-full-keymap))
      (condition-case err
          (which-key-show-full-keymap 'exhub-keys-map)
        (error (describe-keymap 'exhub-keys-map)
               (message "which-key failed: %s" err)))
    (describe-keymap 'exhub-keys-map)))

(defun exhub-keys-open-log ()
  "Show the ExHub backend log buffer and jump to its end."
  (interactive)
  (let ((buffer (get-buffer "*exhub*")))
    (unless buffer
      (user-error "No *exhub* buffer: is the backend running?"))
    (pop-to-buffer buffer)
    (goto-char (point-max))))

(defun exhub-keys-status ()
  "Print the running ExHub backend version in the minibuffer."
  (interactive)
  (if (not (file-executable-p exhub-backend-path))
      (message "[ExHub] Backend binary not executable: %s" exhub-backend-path)
    (let ((buffer (get-buffer-create " *exhub-version*")))
      (with-current-buffer buffer (erase-buffer))
      (make-process
       :name "exhub-version"
       :buffer buffer
       :command (list exhub-backend-path "version")
       :sentinel
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (let ((output (with-current-buffer (process-buffer proc)
                           (string-trim (buffer-string)))))
             (message "[ExHub] %s"
                      (if (string-empty-p output) "no version output" output)))))))))

(defun exhub-keys--confirm (prompt command)
  "Run COMMAND if the user answers yes to PROMPT."
  (when (y-or-n-p prompt)
    (funcall command)))

(defun exhub-keys-restart ()
  "Restart the ExHub backend and websocket, after confirmation."
  (interactive)
  (exhub-keys--confirm
   "Restart the ExHub backend? In-flight LLM proxy requests will drop."
   #'exhub-restart))

(defun exhub-keys-release ()
  "Build a release and restart, after confirmation."
  (interactive)
  (exhub-keys--confirm
   "Build a new release and restart the ExHub backend?"
   #'exhub-release))

(defun exhub-keys-soft-restart ()
  "Soft-restart the ExHub BEAM VM, after confirmation."
  (interactive)
  (exhub-keys--confirm
   "Soft-restart the ExHub BEAM VM? In-flight LLM proxy requests will drop."
   #'exhub-graceful-restart))

(defun exhub-keys-hard-restart ()
  "Hard-restart the ExHub BEAM VM, after confirmation."
  (interactive)
  (exhub-keys--confirm
   "Hard-restart the ExHub BEAM VM (the OS process exits)?"
   #'exhub-graceful-restart-hard))

;;; MCP server dispatch ----------------------------------------------------

(defconst exhub-keys--mcp-servers
  '(("Git" exhub-start-git-mcp-server exhub-stop-git-mcp-server)
    ("GitHub" exhub-start-github-mcp-server exhub-stop-github-mcp-server)
    ("Gitee" exhub-start-gitee-mcp-server exhub-stop-gitee-mcp-server)
    ("Gitee Enterprise" exhub-start-gitee-mcp-ent-server
     exhub-stop-gitee-mcp-ent-server)
    ("File" exhub-start-file-mcp-server exhub-stop-file-mcp-server)
    ("Kubernetes" exhub-start-k8s-mcp-server exhub-stop-k8s-mcp-server))
  "MCP servers known to `exhub-tool', as (NAME START-COMMAND STOP-COMMAND).")

(defun exhub-keys--mcp-dispatch (column prompt)
  "Prompt for a server and run its COLUMN command (1 start, 2 stop)."
  (require 'exhub-tool)
  (let* ((names (mapcar #'car exhub-keys--mcp-servers))
         (choice (completing-read prompt names nil t))
         (command (nth column (assoc choice exhub-keys--mcp-servers))))
    (if command
        (call-interactively command)
      (user-error "No command for MCP server %s" choice))))

(defun exhub-keys-mcp-start-server ()
  "Start an MCP server chosen from `exhub-keys--mcp-servers'."
  (interactive)
  (exhub-keys--mcp-dispatch 1 "Start MCP server: "))

(defun exhub-keys-mcp-stop-server ()
  "Stop an MCP server chosen from `exhub-keys--mcp-servers'."
  (interactive)
  (exhub-keys--mcp-dispatch 2 "Stop MCP server: "))

;;; Rebuild and mode -------------------------------------------------------

(defvar exhub-keys-mode nil
  "Non-nil when the ExHub key tree is installed.")

(defun exhub-keys-rebuild ()
  "Rebuild the key tree from `exhub-keys-tree' and refresh its labels."
  (interactive)
  (setq exhub-keys-map (exhub-keys--root-map))
  (when exhub-keys-mode
    (exhub-keys--install))
  (message "[ExHub] Key tree rebuilt from `exhub-keys-tree'"))

;;;###autoload
(define-minor-mode exhub-keys-mode
  "Global minor mode binding the ExHub command tree.
The tree is reachable from `exhub-keys-prefix' (default \"C-c e\");
press `?' there to list every binding."
  :global t
  :group 'exhub-keys
  (if exhub-keys-mode
      (exhub-keys--install)
    (exhub-keys--uninstall)))

(provide 'exhub-keys)

;;; exhub-keys.el ends here