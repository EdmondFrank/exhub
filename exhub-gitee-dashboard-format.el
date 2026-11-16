;;; exhub-gitee-dashboard-format.el --- Gitee Enterprise dashboard: formatting -*- lexical-binding: t; -*-

;; Copyright (C) 2025 EdmondFrank

;; Author: EdmondFrank
;; Keywords: tools, gitee, dashboard

;;; Commentary:

;; JSON accessors, Org cell/table helpers and the row builders shared by the
;; dashboard and the detail renderers.

;;; Code:

(require 'org)
(require 'seq)
(require 'subr-x)

(require 'exhub-gitee-dashboard-vars)

(defun exhub-gitee-dashboard--get (alist key)
  "Return value of KEY in ALIST, or nil.
JSON objects are parsed with symbol keys for `alist' objects, so look the
key up as a symbol first and fall back to a string."
  (when (listp alist)
    (let ((cell (or (assq (intern-soft key) alist)
                    (assoc key alist))))
      (cdr cell))))

(defun exhub-gitee-dashboard--path (alist &rest keys)
  "Return the value at KEYS inside the nested JSON objects of ALIST."
  (let ((node alist))
    (dolist (key keys node)
      (setq node (exhub-gitee-dashboard--get node key)))))

(defun exhub-gitee-dashboard--names (items &optional key)
  "Join the names of ITEMS with \"、\".
KEY names the field to read, \"name\" by default."
  (let ((text (mapconcat
               (lambda (item)
                 (or (exhub-gitee-dashboard--get item (or key "name")) ""))
               items "、")))
    (if (string-empty-p text) nil text)))

(defun exhub-gitee-dashboard--state-label (state)
  "Return a Chinese label for the pull request / issue STATE."
  (pcase state
    ("opened" "进行中")
    ("open" "开启")
    ("merged" "已合并")
    ("closed" "已关闭")
    ("rejected" "已拒绝")
    (_ (or state "-"))))

(defun exhub-gitee-dashboard--num (n)
  "Format number N with thousands separators."
  (cond
   ((null n) "-")
   ((not (integerp n)) (format "%s" n))
   (t (let ((s (number-to-string n))
            (parts nil)
            (i nil))
        (setq i (length s))
        (while (> i 0)
          (push (substring s (max 0 (- i 3)) i) parts)
          (setq i (max 0 (- i 3))))
        (mapconcat #'identity parts ",")))))

(defun exhub-gitee-dashboard--cell (value)
  "Return VALUE as a safe Org table cell."
  (let ((s (cond ((null value) "")
                 ((stringp value) value)
                 (t (format "%s" value)))))
    (setq s (replace-regexp-in-string "[\n\r\t]+" " " s))
    (setq s (replace-regexp-in-string "|" "/" s))
    (setq s (string-trim s))
    (if (string-empty-p s) "-" s)))

(defun exhub-gitee-dashboard--link (url desc)
  "Return an Org link to URL with description DESC."
  (let ((text (replace-regexp-in-string "[][\n\r]" " " (or desc url))))
    (format "[[%s][%s]]" url (string-trim text))))

(defun exhub-gitee-dashboard--date (stamp)
  "Return the date part of ISO STAMP, or \"-\"."
  (if (and (stringp stamp) (>= (length stamp) 10))
      (substring stamp 0 10)
    "-"))

(defun exhub-gitee-dashboard--put-row-meta (meta)
  "Attach the row plist META to the current line as an overlay."
  (when meta
    (let ((overlay (make-overlay (line-beginning-position)
                                 (line-end-position))))
      (overlay-put overlay 'exhub-gitee-dashboard-row meta)
      (overlay-put overlay 'evaporate t))))

(defun exhub-gitee-dashboard--row-meta ()
  "Return the row plist stored on the current line, or nil."
  (or (get-char-property (point) 'exhub-gitee-dashboard-row)
      (get-char-property (line-beginning-position)
                         'exhub-gitee-dashboard-row)))

(defun exhub-gitee-dashboard--insert-table (header rows &optional row-meta)
  "Insert an Org table with HEADER and ROWS, then align it.
ROW-META, when non-nil, is a list parallel to ROWS; each element is a
plist stored on the corresponding table line so commands can tell which
pull request / issue the line belongs to."
  (let ((start (point)))
    (insert "| " (mapconcat #'exhub-gitee-dashboard--cell header " | ") " |\n")
    (insert "|" (mapconcat (lambda (_cell) "---") header "+") "|\n")
    (dolist (row rows)
      (insert "| " (mapconcat #'exhub-gitee-dashboard--cell row " | ") " |\n"))
    (save-excursion
      (goto-char start)
      (org-table-align))
    (when row-meta
      (save-excursion
        (goto-char start)
        (forward-line 2)
        (dolist (meta row-meta)
          (exhub-gitee-dashboard--put-row-meta meta)
          (forward-line 1))))))

(defun exhub-gitee-dashboard--period-rows (stats)
  "Return overview table rows from STATS."
  (list
   (list "新增代码行"
         (exhub-gitee-dashboard--num (exhub-gitee-dashboard--get stats "today_code_count"))
         (exhub-gitee-dashboard--num (exhub-gitee-dashboard--get stats "week_code_count"))
         (exhub-gitee-dashboard--num (exhub-gitee-dashboard--get stats "month_code_count")))
   (list "到期 Issue"
         (exhub-gitee-dashboard--num (exhub-gitee-dashboard--get stats "today_issue_count"))
         (exhub-gitee-dashboard--num (exhub-gitee-dashboard--get stats "week_issue_count"))
         (exhub-gitee-dashboard--num (exhub-gitee-dashboard--get stats "month_issue_count")))))

(defun exhub-gitee-dashboard--committer-rows (committers ent-path)
  "Return (ROWS . METAS) for COMMITTERS within enterprise path ENT-PATH."
  (let ((index 0)
        rows
        metas)
    (dolist (committer committers)
      (setq index (1+ index))
      (let* ((username (exhub-gitee-dashboard--get committer "username"))
             (name (or (exhub-gitee-dashboard--get committer "name") username))
             (url (if (and ent-path username)
                      (format "https://gitee.com/%s/dashboard/members/%s" ent-path username)
                    (exhub-gitee-dashboard--get committer "path"))))
        (push (list (number-to-string index)
                    (if url
                        (exhub-gitee-dashboard--link url name)
                      (exhub-gitee-dashboard--cell name))
                    (exhub-gitee-dashboard--num
                     (exhub-gitee-dashboard--get committer "commit_count")))
              rows)
        (push (list :kind 'member :title name :url url) metas)))
    (cons (nreverse rows) (nreverse metas))))

(defun exhub-gitee-dashboard--pull-rows (pulls)
  "Return (ROWS . METAS) for the open pull requests in PULLS."
  (let ((open (seq-filter
               (lambda (pull)
                 (equal "opened" (exhub-gitee-dashboard--get pull "state")))
               pulls)))
    (setq open (sort open
                     (lambda (a b)
                       (string> (or (exhub-gitee-dashboard--get a "updated_at") "")
                                (or (exhub-gitee-dashboard--get b "updated_at") "")))))
    (let (rows metas)
      (dolist (pull (seq-take open exhub-gitee-dashboard-item-limit))
        (let* ((project (exhub-gitee-dashboard--get pull "project"))
               (repo (or (exhub-gitee-dashboard--get project "path_with_namespace")
                         (exhub-gitee-dashboard--get project "path")))
               (project-id (exhub-gitee-dashboard--get pull "project_id"))
               (iid (exhub-gitee-dashboard--get pull "iid"))
               (title (exhub-gitee-dashboard--get pull "title"))
               (url (when (and repo iid)
                      (format "https://gitee.com/%s/pulls/%s" repo iid))))
          (push (list (if url
                          (exhub-gitee-dashboard--link url title)
                        (exhub-gitee-dashboard--cell title))
                      (exhub-gitee-dashboard--cell repo)
                      (if (exhub-gitee-dashboard--get pull "draft") "草稿" "进行中")
                      (exhub-gitee-dashboard--date
                       (exhub-gitee-dashboard--get pull "updated_at")))
                rows)
          (push (list :kind 'pull
                      :ref (and iid (number-to-string iid))
                      :subject (and project-id (number-to-string project-id))
                      :repo repo
                      :title title
                      :url url)
                metas)))
      (cons (nreverse rows) (nreverse metas)))))

(defun exhub-gitee-dashboard--issue-rows (issues ent-path)
  "Return (ROWS . METAS) for ISSUES in enterprise path ENT-PATH."
  (let (rows metas)
    (dolist (issue issues)
      (let* ((ident (exhub-gitee-dashboard--get issue "ident"))
             (title (exhub-gitee-dashboard--get issue "title"))
             (state (exhub-gitee-dashboard--get (exhub-gitee-dashboard--get issue "issue_state")
                                                "title"))
             (assignee (exhub-gitee-dashboard--get issue "assignee"))
             (owner (or (exhub-gitee-dashboard--get assignee "name")
                        (exhub-gitee-dashboard--get assignee "username")))
             (url (if (and ent-path ident)
                      (format "https://gitee.com/%s/dashboard/issues?id=%s" ent-path ident)
                    nil)))
        (push (list (if url
                        (exhub-gitee-dashboard--link url ident)
                      (exhub-gitee-dashboard--cell ident))
                    (exhub-gitee-dashboard--cell title)
                    (exhub-gitee-dashboard--cell state)
                    (exhub-gitee-dashboard--cell owner)
                    (exhub-gitee-dashboard--date
                     (exhub-gitee-dashboard--get issue "deadline")))
              rows)
        (push (list :kind 'issue :ref ident :title title :url url) metas)))
    (cons (nreverse rows) (nreverse metas))))

(provide 'exhub-gitee-dashboard-format)

;;; exhub-gitee-dashboard-format.el ends here
