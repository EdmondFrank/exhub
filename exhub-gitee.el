;;;; exhub-gitee.el --- Gitee integration for Exhub

;;; Commentary:

;; This package provides integration with the Gitee API using the Exhub
;; framework. It allows you to fetch and display Gitee issues in an Org-mode
;; buffer.

;;; Code:

;;; Libraries
(require 'cl-lib)
(require 'json)
(require 'org)
(require 'org-element)
(require 'exhub)

;;; Variables
(defvar gitee-buffer-name "*Gitee*" "Gitee buffer name.")
(defcustom gitee-default-ent-id 1 "Default enterprise ID for Gitee API calls." :type 'integer :group 'gitee)

;;; Functions to interact with the Gitee API

(defun gitee-fetch-issues ()
  "Fetch issues from Gitee API."
  (exhub-gitee-call "Issues" "list" 'gitee-populate-buffer gitee-default-ent-id '(:only_related_me 1)))

(defun gitee-populate-buffer (response)
  "Populate the Gitee buffer with issues."
  (with-current-buffer (get-buffer-create gitee-buffer-name)
    (org-mode)  ; Ensure the buffer is in Org-mode
    (erase-buffer)
    (insert "* Gitee Issues\n")
    (let* ((json-object-type 'hash-table)
           (json-array-type 'list)
           (json-key-type 'string)
           (json (json-read-from-string response))
           (issues (gethash "data" json)))
      (dolist (issue issues)
        (let ((title (gethash "title" issue)))
          (insert (format "** %s\n" title)))))))

;;; Functions to create and populate an Org-mode buffer with issues

(defun gitee-open-buffer ()
  "Open a new Org-mode buffer to display Gitee issues."
  (interactive)
  (let ((buffer (get-buffer-create gitee-buffer-name)))
    (switch-to-buffer buffer)
    (org-mode)  ; Ensure the buffer is in Org-mode
    (erase-buffer)
    (insert "* Gitee Issues\n")
    (gitee-fetch-issues)))

;;; Menu to interact with issues

(defun exhub-gitee-call (module func callback &rest args)
  "Call a Gitee API function using Exhub."
  (exhub-call "exhub-gitee" module func callback args))

(defun exhub-gitee-response (result)
  "Handle the response from the Gitee API."
  (interactive)
  (message result))

;;; Integration

(provide 'exhub-gitee)

;;; exhub-gitee.el ends here
