;;; exhub-vault.el --- Org-mode password vault integrated with Exhub  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 EdmondFrank

;; Author: EdmondFrank
;; Keywords: tools, vault, encryption

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides an org-mode password vault feature integrated with
;; the Exhub Elixir backend.  Secrets are encrypted with AES-256-GCM on the
;; backend and stored as org links like [[exhub-vault:CIPHERTEXT][description]].

;;; Code:

(require 'exhub)
(require 'org)

(defgroup exhub-vault nil
  "Org-mode password vault integrated with Exhub."
  :group 'applications)

(defvar exhub-vault-link-prefix "exhub-vault"
  "The org link type prefix for vault links.")

(defvar-local exhub-vault--pending-description nil
  "Pending description for the next vault insert callback.")

(defvar-local exhub-vault--pending-action nil
  "Pending action for the next vault decrypt callback.
Should be \"copy\" or \"show\".")

;;;###autoload
(defun exhub-vault-call (action callback &rest args)
  "Call vault API via Exhub WebSocket.
ACTION is the vault operation (e.g. \"encrypt\", \"decrypt\").
CALLBACK is the function name (as a string) to call with the result.
ARGS are additional arguments for the vault operation."
  (apply #'exhub-call "exhub-vault" action callback args))

;;;###autoload
(defun exhub-vault-insert-secret ()
  "Prompt for a secret and insert it as an encrypted vault link at point."
  (interactive)
  (let ((description (string-trim (read-string "Secret description: ")))
        (secret-value (string-trim (read-passwd "Secret value: "))))
    (if (or (string-empty-p description) (string-empty-p secret-value))
        (message "Description and secret value must not be empty")
      (setq exhub-vault--pending-description description)
      (exhub-vault-call "encrypt" "exhub-vault--insert-encrypted" secret-value))))

(defun exhub-vault--insert-encrypted (ciphertext)
  "Insert an encrypted vault link at point.
CIPHERTEXT is the base64-encoded encrypted string received from the backend."
  (let ((description exhub-vault--pending-description))
    (setq exhub-vault--pending-description nil)
    (insert (format "[[%s:%s][%s]]" exhub-vault-link-prefix ciphertext description))
    (message "Secret encrypted and inserted")))

;;;###autoload
(defun exhub-vault-decrypt-and-copy ()
  "Decrypt the vault link at point or active region and copy to clipboard."
  (interactive)
  (let ((ciphertext (or (exhub-vault--extract-ciphertext-at-point)
                        (and (use-region-p)
                             (string-trim (buffer-substring-no-properties
                                           (region-beginning) (region-end)))))))
    (if ciphertext
        (progn
          (setq exhub-vault--pending-action "copy")
          (exhub-vault-call "decrypt" "exhub-vault--decrypt-callback" ciphertext))
      (message "No vault link or region selected"))))

;;;###autoload
(defun exhub-vault-decrypt-and-show ()
  "Decrypt the vault link at point or active region and show in minibuffer."
  (interactive)
  (let ((ciphertext (or (exhub-vault--extract-ciphertext-at-point)
                        (and (use-region-p)
                             (string-trim (buffer-substring-no-properties
                                           (region-beginning) (region-end)))))))
    (if ciphertext
        (progn
          (setq exhub-vault--pending-action "show")
          (exhub-vault-call "decrypt" "exhub-vault--decrypt-callback" ciphertext))
      (message "No vault link or region selected"))))

(defun exhub-vault--decrypt-callback (plaintext)
  "Handle decrypted PLAINTEXT from the vault backend.
Action depends on `exhub-vault--pending-action'."
  (let ((action exhub-vault--pending-action))
    (setq exhub-vault--pending-action nil)
    (cond
     ((string= action "copy")
      (kill-new plaintext)
      (message "Secret decrypted and copied to clipboard"))
     ((string= action "show")
      (message "Secret: %s" plaintext))
     (t (message "Unknown vault action: %s" action)))))

(defun exhub-vault--org-follow (path)
  "Follow an exhub-vault org link at point.
PATH is the ciphertext extracted from the link."
  (setq exhub-vault--pending-action "copy")
  (exhub-vault-call "decrypt" "exhub-vault--decrypt-callback" path))

(defun exhub-vault--org-face (_path)
  "Return the face for exhub-vault links.
_PATH is the link path (unused)."
  '(:foreground "forest green" :underline t))

(defun exhub-vault--extract-ciphertext-at-point ()
  "Extract the ciphertext from the exhub-vault link at point.
Return the ciphertext string, or nil if point is not on a vault link."
  (let* ((element (org-element-context))
         (type (org-element-property :type element)))
    (when (and (eq (org-element-type element) 'link)
               (string= type exhub-vault-link-prefix))
      (org-element-property :path element))))

;;; Keybindings
;;; C-c <letter> is reserved for user in Emacs convention
(defvar exhub-vault-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c v i") #'exhub-vault-insert-secret)
    (define-key map (kbd "C-c v c") #'exhub-vault-decrypt-and-copy)
    (define-key map (kbd "C-c v s") #'exhub-vault-decrypt-and-show)
    map)
  "Keymap for exhub-vault commands.")

;;;###autoload
(define-minor-mode exhub-vault-mode
  "Minor mode for exhub-vault org-mode password vault."
  :global t
  :keymap exhub-vault-mode-map)

(org-link-set-parameters "exhub-vault"
                         :follow #'exhub-vault--org-follow
                         :face #'exhub-vault--org-face
                         :help-echo "Exhub vault encrypted secret")

(provide 'exhub-vault)

;;; exhub-vault.el ends here
