;;;; exhub-gitee.el --- Gitee integration for Exhub

;;; Commentary:

;; This package provides integration with the Gitee API using the Exhub framework.
;; It allows you to fetch and display Gitee issues in an Org-mode
;; buffer.

;;; Code:

;;; Libraries
(require 'cl-lib)
(require 'json)
(require 'org)
(require 'org-element)
(require 'exhub)

;;; Variables
(defvar gitee-issues-buffer-name "*Gitee Issues*" "Gitee issues buffer name.")
(defcustom gitee-default-ent-id 1 "Default enterprise ID for Gitee API calls." :type 'integer :group 'gitee)

;;; Handle functions

(defun gitee-show-issues (response)
  "Render the Gitee Issues buffer with issues."
  (with-current-buffer (get-buffer-create gitee-issues-buffer-name)
    (org-mode)                      ; Ensure the buffer is in Org-mode
    (erase-buffer)
    (insert "* Gitee Issues\n")
    (let* ((json-object-type 'hash-table)
           (json-array-type 'list)
           (json-key-type 'string)
           (json (json-read-from-string response))
           (issues (gethash "data" json)))
      (dolist (issue issues)
        (let* ((title (gethash "title" issue))
               (id (gethash "id" issue))
               (state (gethash "state" issue))
               (org-state (cond
                           ((string= state "open") "TODO")
                           ((string= state "progressing") "IN-PROGRESS")
                           ((string= state "closed") "DONE")
                           ((string= state "rejected") "REJECTED")
                           (t "UNKNOWN"))))
          (insert (format "** %s %s\n" org-state title))
          (org-set-property "ID" (number-to-string id))
          (org-todo org-state))))))

(defun gitee-show-issue-detail (response)
  "Show the issue detail in a new buffer."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (issue (json-read-from-string response))
         (id (gethash "id" issue))
         (title (gethash "title" issue))
         (description (gethash "description" issue))
         (state (gethash "state" issue))
         (org-state (cond
                     ((string= state "open") "TODO")
                     ((string= state "progressing") "IN-PROGRESS")
                     ((string= state "closed") "DONE")
                     ((string= state "rejected") "REJECTED")
                     (t "UNKNOWN"))))
    (let ((detail-buffer-name (format "*Gitee Issue %s*" id)))
      (with-current-buffer (get-buffer-create detail-buffer-name)
        (org-mode)                  ; Ensure the buffer is in Org-mode
        (erase-buffer)
        (switch-to-buffer detail-buffer-name)
        (insert (format "* %s %s\n%s\n" org-state title description))))))

;;; Functions to interative with an Org-mode buffer with issues

(defun gitee-open-issue-detail-buffer ()
  "Fetch the issue detail for the current issue in the Gitee buffer."
  (interactive)
  (let* ((org-item (org-element-at-point))
         (id (org-element-property :ID org-item)))
    (when id
      (gitee--fetch-issue-detail (string-to-number id)))))

(defun gitee-open-issues-buffer ()
  "Open a new Org-mode buffer to display Gitee issues."
  (interactive)
  (let ((buffer (get-buffer-create gitee-issues-buffer-name)))
    (switch-to-buffer buffer)
    (org-mode)                      ; Ensure the buffer is in Org-mode
    (erase-buffer)
    (insert "* Gitee Issues\n")
    (gitee--fetch-issues)))


;;; Functions to interact with the Gitee API

(defun gitee--fetch-issues ()
  "Fetch issues from Gitee API."
  (exhub-gitee-call "Issues" "list" 'gitee-show-issues gitee-default-ent-id '(:only_related_me 1)))

(defun gitee--fetch-issue-detail (issue-id)
  "Fetch an issue detail from Gitee API."
  (exhub-gitee-call "Issues" "detail" 'gitee-show-issue-detail gitee-default-ent-id issue-id))

(defun exhub-gitee-call (module func callback &rest args)
  "Call a Gitee API function using Exhub."
  (exhub-call "exhub-gitee" module func callback args))

;;;

(provide 'exhub-gitee)

;;; exhub-gitee.el ends here
