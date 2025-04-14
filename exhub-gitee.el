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
(require 'diff-mode) ; Ensure diff-mode is loaded

;;; Variables
(defvar gitee-pulls-buffer-name "*Gitee Pulls*" "Gitee pulls buffer name.")
(defvar gitee-issues-buffer-name "*Gitee Issues*" "Gitee issues buffer name.")
(defcustom gitee-default-ent-id 1 "Default enterprise ID for Gitee API calls." :type 'integer :group 'exhub-gitee)

;;; Handle functions

(defun gitee-show-pulls (response)
  "Render the Gitee Pulls buffer with pulls."
  (with-current-buffer (get-buffer-create gitee-pulls-buffer-name)
    (org-mode)                      ; Ensure the buffer is in Org-mode
    (erase-buffer)
    (insert "* Gitee Pulls\n")
    (let* ((json-object-type 'hash-table)
           (json-array-type 'list)
           (json-key-type 'string)
           (json (json-read-from-string response))
           (pulls (gethash "data" json)))
      (dolist (pull pulls)
        (let* ((title (gethash "title" pull))
               (id (gethash "id" pull))
               (project-id (gethash "id" (gethash "project" pull)))
               (state (gethash "state" pull))
               (author (gethash "remark" (gethash "author" pull)))
               (source-branch (gethash "branch" (gethash "source_branch" pull)))
               (target-branch (gethash "branch" (gethash "target_branch" pull)))
               (org-state (cond
                           ((string= state "opened") "TODO")
                           ((string= state "reopened") "TODO")
                           ((string= state "closed") "REJECTED")
                           ((string= state "merged") "DONE")
                           (t "UNKNOWN"))))
          (insert (format "** %s %s\n" org-state title))
          (org-set-property "ID" (number-to-string id))
          (org-set-property "AUTHOR" author)
          (org-set-property "SOURCE_BRANCH" source-branch)
          (org-set-property "TARGET_BRANCH" target-branch)
          (org-todo org-state)
          ;; Add a link to show the diff
          (insert (format "[[elisp:(gitee-show-pull-diff %d %d)][Show Diff]]\n" project-id id)))))))

(defun gitee-show-pull-diff (project-id pull-id)
  "Fetch and display the raw diff for the pull request identified by PROJECT-ID and PULL-ID."
  (interactive "nProject ID: \nnPull ID: ")
  (let ((buffer-name (format "*Gitee Pull Diff %d*" pull-id)))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)  ; Clear the buffer
      ;; Insert title before calling the async function
      (insert (format "* Gitee Pull Diff for Pull ID: %d\n\n" pull-id))
      (gitee--fetch-pull-diff project-id pull-id 'gitee-handle-pull-diff-response))  ; Use named function
    (switch-to-buffer buffer-name)))

(defun gitee-handle-pull-diff-response (response)
  "Handle the response from fetching the pull diff and display it in diff-mode."
  (let ((inhibit-read-only t)) ; Allow modification even if buffer became read-only
    (condition-case err
        (let* ((json-object-type 'string) ; Expecting a simple string from JSON parse
               (json-array-type 'list)
               (json-key-type 'string)
               ;; Attempt to parse the response as a JSON string
               (diff-content (json-read-from-string response)))
          (if (stringp diff-content)
              (progn
                (insert diff-content) ; Insert the *parsed* diff content
                (goto-char (point-min)) ; Go to the beginning of the buffer
                (diff-mode)        ; Enable diff-mode for syntax highlighting and navigation
                (setq buffer-read-only t)) ; Make the buffer read-only after inserting content
            ;; Handle cases where JSON parsing succeeded but didn't yield a string
            (insert (format "Error: Expected diff string but got type %s" (type-of diff-content)))))
      ;; Handle JSON parsing errors or other errors during processing
      (error (insert (format "Error processing diff response: %s\nRaw response:\n%s"
                             (error-message-string err)
                             response))))))

(defun gitee--fetch-pull-diff (project-id pull-id callback)
  "Fetch the raw diff for the pull request using PROJECT-ID and PULL-ID, then call CALLBACK with the response."
  (exhub-gitee-call "Pulls" "raw_diff" callback gitee-default-ent-id project-id pull-id))

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

;;; Functions to interact with an Org-mode buffer with issues

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

(defun gitee-open-pulls-buffer ()
  "Open a new Org-mode buffer to display Gitee pulls."
  (interactive)
  (let ((buffer (get-buffer-create gitee-pulls-buffer-name)))
    (switch-to-buffer buffer)
    (org-mode)                      ; Ensure the buffer is in Org-mode
    (erase-buffer)
    (insert "* Gitee Pulls\n")
    (gitee--fetch-pulls)))


;;; Functions to interact with the Gitee API

(defun gitee--fetch-issues ()
  "Fetch issues from Gitee API."
  (exhub-gitee-call "Issues" "list" 'gitee-show-issues gitee-default-ent-id '(:only_related_me 1)))

(defun gitee--fetch-pulls ()
  "Fetch pulls from Gitee API."
  (exhub-gitee-call "Pulls" "list" 'gitee-show-pulls gitee-default-ent-id '(:scope "related_to_me")))

(defun gitee--fetch-issue-detail (issue-id)
  "Fetch an issue detail from Gitee API."
  (exhub-gitee-call "Issues" "detail" 'gitee-show-issue-detail gitee-default-ent-id issue-id))

(defun exhub-gitee-call (module func callback &rest args)
  "Call a Gitee API function using Exhub."
  (exhub-call "exhub-gitee" module func callback args))

;;;

(provide 'exhub-gitee)

;;; exhub-gitee.el ends here
