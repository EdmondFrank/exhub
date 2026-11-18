;;; exhub-gitee-dashboard-detail.el --- Gitee Enterprise dashboard: detail view -*- lexical-binding: t; -*-

;; Copyright (C) 2025 EdmondFrank

;; Author: EdmondFrank
;; Keywords: tools, gitee, dashboard

;;; Commentary:

;; The pull request / issue detail buffer: its major mode, the renderer for
;; the detail payload and the refresh / browse commands.  Descriptions,
;; comments and the bodies of "description updated" events are converted from
;; Markdown to Org by `exhub-gitee-dashboard-markdown.el', and the comments
;; are merged with the operation log (event) entries into one timeline.  The
;; images they contain are downloaded and displayed inline by
;; `exhub-gitee-dashboard-image.el'.  The row metadata that decides which
;; detail to open is produced in `exhub-gitee-dashboard-format.el'.
;;
;; A pull request also has a unified diff, which lives in its own buffer
;; (`exhub-gitee-dashboard-diff.el'); `d' opens it.

;;; Code:

(require 'org)
(require 'subr-x)

(require 'exhub-gitee-dashboard-vars)
(require 'exhub-gitee-dashboard-fetch)
(require 'exhub-gitee-dashboard-format)
(require 'exhub-gitee-dashboard-markdown)
(require 'exhub-gitee-dashboard-image)
(require 'exhub-gitee-dashboard-diff)

(defvar exhub-gitee-dashboard-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    map)
  "Keymap for `exhub-gitee-dashboard-detail-mode'.")

(defvar-local exhub-gitee-dashboard--detail-request nil
  "Request plist of the current detail buffer.
Holds :kind, :ref, :subject, :repo, :title and :url.")

(define-derived-mode exhub-gitee-dashboard-detail-mode org-mode "GiteeDetail"
  "Major mode for a Gitee pull request / issue detail buffer."
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil)
  (setq-local header-line-format " Gitee 详情 | 加载中…"))

(define-key exhub-gitee-dashboard-detail-mode-map (kbd "g")
  #'exhub-gitee-dashboard-detail-refresh)
(define-key exhub-gitee-dashboard-detail-mode-map (kbd "o")
  #'exhub-gitee-dashboard-detail-browse)
(define-key exhub-gitee-dashboard-detail-mode-map (kbd "i")
  #'exhub-gitee-dashboard-detail-toggle-images)
(define-key exhub-gitee-dashboard-detail-mode-map (kbd "d")
  #'exhub-gitee-dashboard-detail-diff)
(define-key exhub-gitee-dashboard-detail-mode-map (kbd "RET")
  #'exhub-gitee-dashboard-detail-browse)
(define-key exhub-gitee-dashboard-detail-mode-map (kbd "q") #'quit-window)

(defun exhub-gitee-dashboard--detail-buffer-name (request)
  "Return the detail buffer name for REQUEST."
  (if (eq (plist-get request :kind) 'pull)
      (format "*Gitee Pull Request: %s!%s*"
              (or (plist-get request :repo) "-")
              (or (plist-get request :ref) "-"))
    (format "*Gitee Issue: %s*" (or (plist-get request :ref) "-"))))

(defun exhub-gitee-dashboard--timeline-author (item data)
  "Return the author name of the timeline ITEM, whose note object is DATA."
  (or (exhub-gitee-dashboard--path item "author" "name")
      (exhub-gitee-dashboard--path item "author" "username")
      (exhub-gitee-dashboard--path item "author" "login")
      (exhub-gitee-dashboard--path item "user" "name")
      (exhub-gitee-dashboard--path item "user" "username")
      (exhub-gitee-dashboard--path item "user" "login")
      (exhub-gitee-dashboard--path data "author" "name")))

(defun exhub-gitee-dashboard--text (value)
  "Return VALUE when it is a string, else nil.
Gitee reports an inline code review thread with its `content' set to the array
of diff lines the thread hangs off, not to text; treating that as Markdown
makes the whole detail render fail."
  (when (stringp value) value))

(defun exhub-gitee-dashboard--event-p (item)
  "Return non-nil when the timeline ITEM is an operation log (event) entry."
  (let ((type (exhub-gitee-dashboard--get item "type")))
    (or (equal type "operate_log")
        (equal type "event")
        (and (null (exhub-gitee-dashboard--get item "data"))
             (exhub-gitee-dashboard--get item "icon")))))

(defun exhub-gitee-dashboard--event-body (item)
  "Return the Markdown body carried by the event ITEM, when there is one.
Gitee reports a title / description edit twice: an HTML summary in `content'
plus the new value in `after_value' (pull requests) or `after_change'
(issues).  Only a value long enough to be a body is worth rendering."
  (let ((after (or (exhub-gitee-dashboard--get item "after_value")
                   (exhub-gitee-dashboard--get item "after_change"))))
    (when (and (stringp after)
               (not (string-empty-p (string-trim after)))
               (or (string-match-p "\n" after) (> (length after) 60)))
      after)))

(defun exhub-gitee-dashboard--timeline-iso (stamp)
  "Return STAMP as an ISO 8601 string.
Gitee reports comment timestamps as epoch seconds in the pull request
timeline and as ISO strings everywhere else."
  (cond ((stringp stamp) stamp)
        ((integerp stamp) (format-time-string "%Y-%m-%dT%H:%M:%S" stamp))
        (t "")))

(defun exhub-gitee-dashboard--timeline-seconds (stamp)
  "Return STAMP as epoch seconds, for sorting the timeline."
  (cond
   ((integerp stamp) stamp)
   ((and (stringp stamp) (not (string-empty-p stamp)))
    (condition-case nil
        (float-time (date-to-time stamp))
      (error 0)))
   (t 0)))

(defun exhub-gitee-dashboard--timeline-entry (item)
  "Normalize the raw timeline ITEM into a plist.
The result holds :kind (`note' or `event'), :author, :created (an ISO
string), :stamp (epoch seconds, for ordering), the :content of an event and
the Markdown :body to render below the entry."
  (let* ((data (exhub-gitee-dashboard--get item "data"))
         (event (exhub-gitee-dashboard--event-p item))
         (created (or (exhub-gitee-dashboard--get item "created_at")
                      (exhub-gitee-dashboard--get data "created_at"))))
    (list :kind (if event 'event 'note)
          :author (exhub-gitee-dashboard--timeline-author item data)
          :created (exhub-gitee-dashboard--timeline-iso created)
          :stamp (exhub-gitee-dashboard--timeline-seconds created)
          :content (when event
                     (exhub-gitee-dashboard--text
                      (exhub-gitee-dashboard--get item "content")))
          :body (if event
                    (exhub-gitee-dashboard--event-body item)
                  (or (exhub-gitee-dashboard--text
                       (exhub-gitee-dashboard--get data "note"))
                      (exhub-gitee-dashboard--text
                       (exhub-gitee-dashboard--get item "content")))))))

(defun exhub-gitee-dashboard--timeline (raw)
  "Return the raw timeline RAW as normalized, ordered and capped entries."
  (let ((entries (mapcar #'exhub-gitee-dashboard--timeline-entry (or raw '())))
        (limit exhub-gitee-dashboard-notes-limit))
    (setq entries
          (sort entries
                (lambda (a b)
                  (< (or (plist-get a :stamp) 0)
                     (or (plist-get b :stamp) 0)))))
    (when (and limit (> limit 0) (> (length entries) limit))
      (setq entries (last entries limit)))
    entries))

(defun exhub-gitee-dashboard--timeline-date (stamp)
  "Return STAMP as `YYYY-MM-DD HH:MM' for the timeline."
  (let ((iso (exhub-gitee-dashboard--timeline-iso stamp)))
    (if (>= (length iso) 16)
        (replace-regexp-in-string "T" " " (substring iso 0 16) t t)
      (exhub-gitee-dashboard--date iso))))

(defun exhub-gitee-dashboard--insert-timeline-entry (entry)
  "Insert the normalized timeline ENTRY as an Org subtree."
  (if (eq (plist-get entry :kind) 'event)
      (progn
        (insert (format "** %s%s\n\n"
                        (exhub-gitee-dashboard--timeline-date
                         (plist-get entry :created))
                        (if-let ((author (plist-get entry :author)))
                            (format " · %s" author)
                          "")))
        (let ((summary (string-trim
                        (exhub-gitee-dashboard--md-inline
                         (or (plist-get entry :content) "")))))
          (unless (string-empty-p summary)
            (insert summary "\n\n"))))
    (insert (format "** %s · %s\n\n"
                    (or (plist-get entry :author) "未知")
                    (exhub-gitee-dashboard--timeline-date
                     (plist-get entry :created)))))
  (when-let ((body (plist-get entry :body)))
    (when (exhub-gitee-dashboard--insert-markdown body 2)
      (insert "\n"))))

(defun exhub-gitee-dashboard--insert-timeline (entries total)
  "Insert the merged comment / event section for ENTRIES, out of TOTAL."
  (insert (format "* 事件与评论（共 %s 条，显示 %d 条）\n\n"
                  (exhub-gitee-dashboard--num (or total (length entries)))
                  (length entries)))
  (if entries
      (dolist (entry entries)
        (exhub-gitee-dashboard--insert-timeline-entry entry))
    (insert "暂无评论或事件。\n")))

(defun exhub-gitee-dashboard--check-detail (detail)
  "Signal a `user-error' when DETAIL is an API error payload."
  (when (and (null (exhub-gitee-dashboard--get detail "title"))
             (exhub-gitee-dashboard--get detail "message"))
    (user-error "%s" (exhub-gitee-dashboard--get detail "message")))
  detail)

(defun exhub-gitee-dashboard--render-pull-detail (request data)
  "Render the pull request detail DATA described by REQUEST."
  (let* ((detail (exhub-gitee-dashboard--check-detail
                  (or (exhub-gitee-dashboard--get data "detail") '())))
         (timeline (exhub-gitee-dashboard--timeline
                    (exhub-gitee-dashboard--get data "timeline")))
         (timeline-total (exhub-gitee-dashboard--get data "timeline_total"))
         (repo (or (exhub-gitee-dashboard--path detail "project" "path_with_namespace")
                   (plist-get request :repo)))
         (iid (or (exhub-gitee-dashboard--get detail "iid")
                  (plist-get request :ref)))
         (title (or (exhub-gitee-dashboard--get detail "title")
                    (plist-get request :title)))
         (state (exhub-gitee-dashboard--state-label
                 (exhub-gitee-dashboard--get detail "state")))
         (draft (exhub-gitee-dashboard--get detail "draft"))
         (author (or (exhub-gitee-dashboard--path detail "author" "name")
                     (exhub-gitee-dashboard--path detail "author" "username")))
         (source (exhub-gitee-dashboard--path detail "source_branch" "branch"))
         (target (exhub-gitee-dashboard--path detail "target_branch" "branch"))
         (body (exhub-gitee-dashboard--get detail "body"))
         (url (plist-get request :url))
         (generated (exhub-gitee-dashboard--get data "generated_at")))
    (erase-buffer)
    (insert (format "#+TITLE: %s!%s · Pull Request\n\n" (or repo "-") (or iid "-")))
    (insert (format "* %s\n\n" (or title "-")))
    (insert (format "- 仓库: =%s= · 编号: =%s= · 状态: %s%s\n"
                    (or repo "-") (or iid "-") state
                    (if draft "（草稿）" "")))
    (exhub-gitee-dashboard--insert-field "作者" author)
    (when (or source target)
      (insert (format "- 分支: =%s= → =%s=\n" (or source "-") (or target "-"))))
    (exhub-gitee-dashboard--insert-field
     "评审" (exhub-gitee-dashboard--names (exhub-gitee-dashboard--get detail "assignees")))
    (exhub-gitee-dashboard--insert-field
     "测试" (exhub-gitee-dashboard--names (exhub-gitee-dashboard--get detail "testers")))
    (exhub-gitee-dashboard--insert-field
     "标签" (exhub-gitee-dashboard--names (exhub-gitee-dashboard--get detail "labels")))
    (exhub-gitee-dashboard--insert-field
     "里程碑" (exhub-gitee-dashboard--path detail "milestone" "title"))
    (insert (format "- 变更: +%s / -%s · %s 个文件 · %s 个提交\n"
                    (exhub-gitee-dashboard--num (exhub-gitee-dashboard--get detail "additions"))
                    (exhub-gitee-dashboard--num (exhub-gitee-dashboard--get detail "deletions"))
                    (exhub-gitee-dashboard--num (exhub-gitee-dashboard--get detail "files_count"))
                    (exhub-gitee-dashboard--num (exhub-gitee-dashboard--get detail "commits_count"))))
    (insert (format "- 创建: %s · 更新: %s\n"
                    (exhub-gitee-dashboard--date (exhub-gitee-dashboard--get detail "created_at"))
                    (exhub-gitee-dashboard--date (exhub-gitee-dashboard--get detail "updated_at"))))
    (when url
      (insert (format "- 网页: %s\n"
                      (exhub-gitee-dashboard--link url "在浏览器打开"))))
    (insert "\n* 描述\n\n")
    (unless (exhub-gitee-dashboard--insert-markdown body 1)
      (insert "（无描述）\n"))
    (insert "\n")
    (exhub-gitee-dashboard--insert-timeline timeline timeline-total)
    (exhub-gitee-dashboard--image-inline)
    (goto-char (point-min))
    (setq header-line-format
          (format " %s!%s · Pull Request · %s | 更新 %s | g 刷新  d diff  i 图片  o 浏览器  q 退出"
                  (or repo "-") (or iid "-") state (or generated "-")))))

(defun exhub-gitee-dashboard--render-issue-detail (request data)
  "Render the issue detail DATA described by REQUEST."
  (let* ((detail (exhub-gitee-dashboard--check-detail
                  (or (exhub-gitee-dashboard--get data "detail") '())))
         (timeline (exhub-gitee-dashboard--timeline
                    (exhub-gitee-dashboard--get data "timeline")))
         (timeline-total (exhub-gitee-dashboard--get data "timeline_total"))
         (ident (or (exhub-gitee-dashboard--get detail "ident")
                    (plist-get request :ref)))
         (title (or (exhub-gitee-dashboard--get detail "title")
                    (plist-get request :title)))
         (state (exhub-gitee-dashboard--path detail "issue_state" "title"))
         (itype (exhub-gitee-dashboard--path detail "issue_type" "title"))
         (priority (exhub-gitee-dashboard--get detail "priority_human"))
         (description (or (exhub-gitee-dashboard--get detail "description")
                          (exhub-gitee-dashboard--path detail "description" "value")))
         (url (plist-get request :url))
         (generated (exhub-gitee-dashboard--get data "generated_at")))
    (erase-buffer)
    (insert (format "#+TITLE: %s · Issue\n\n" (or ident "-")))
    (insert (format "* %s\n\n" (or title "-")))
    (insert (format "- 编号: =%s= · 状态: %s\n" (or ident "-") (or state "-")))
    (exhub-gitee-dashboard--insert-field "类型" itype)
    (exhub-gitee-dashboard--insert-field "优先级" priority)
    (exhub-gitee-dashboard--insert-field
     "负责人" (or (exhub-gitee-dashboard--path detail "assignee" "name")
                  (exhub-gitee-dashboard--path detail "assignee" "username")))
    (exhub-gitee-dashboard--insert-field
     "协作者" (exhub-gitee-dashboard--names
               (exhub-gitee-dashboard--get detail "collaborators")))
    (exhub-gitee-dashboard--insert-field
     "所属项目" (exhub-gitee-dashboard--path detail "project" "name"))
    (exhub-gitee-dashboard--insert-field
     "标签" (exhub-gitee-dashboard--names (exhub-gitee-dashboard--get detail "labels")))
    (exhub-gitee-dashboard--insert-field
     "里程碑" (exhub-gitee-dashboard--path detail "milestone" "title"))
    (exhub-gitee-dashboard--insert-field
     "截止" (exhub-gitee-dashboard--get detail "deadline"))
    (insert (format "- 创建: %s · 更新: %s\n"
                    (exhub-gitee-dashboard--date (exhub-gitee-dashboard--get detail "created_at"))
                    (exhub-gitee-dashboard--date (exhub-gitee-dashboard--get detail "updated_at"))))
    (when url
      (insert (format "- 网页: %s\n"
                      (exhub-gitee-dashboard--link url "在浏览器打开"))))
    (insert "\n* 描述\n\n")
    (unless (exhub-gitee-dashboard--insert-markdown description 1)
      (insert "（无描述）\n"))
    (insert "\n")
    (exhub-gitee-dashboard--insert-timeline timeline timeline-total)
    (exhub-gitee-dashboard--image-inline)
    (goto-char (point-min))
    (setq header-line-format
          (format " %s · Issue · %s | 更新 %s | g 刷新  i 图片  o 浏览器  q 退出"
                  (or ident "-") (or state "-") (or generated "-")))))

(defun exhub-gitee-dashboard--render-detail (request data)
  "Render DATA as the detail described by REQUEST."
  (pcase (plist-get request :kind)
    ('pull (exhub-gitee-dashboard--render-pull-detail request data))
    ('issue (exhub-gitee-dashboard--render-issue-detail request data))
    (_ (user-error "Unknown Gitee detail kind: %s" (plist-get request :kind)))))

(defun exhub-gitee-dashboard--render-detail-error (message)
  "Replace the detail buffer content with an error page for MESSAGE."
  (erase-buffer)
  (insert "#+TITLE: Gitee 详情\n\n* 无法加载详情\n\n")
  (insert (format "错误: %s\n\n" message))
  (insert (format "- 确认 ExHub 后端在运行: =curl %s/gitee/detail=\n"
                  (exhub-gitee-dashboard--backend-url)))
  (insert "- 确认 =gitee_ent_mcp_access_token= 已存入 ExHub SecretVault\n")
  (insert "- 按 =g= 重试\n")
  (goto-char (point-min))
  (setq header-line-format " Gitee 详情 | 加载失败 | g 重试  q 退出"))

(defun exhub-gitee-dashboard--detail-buffer (request)
  "Return the detail buffer of REQUEST, in detail mode, with REQUEST stored."
  (let ((buffer (get-buffer-create (exhub-gitee-dashboard--detail-buffer-name request))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'exhub-gitee-dashboard-detail-mode)
        (exhub-gitee-dashboard-detail-mode))
      (setq-local exhub-gitee-dashboard--detail-request request))
    buffer))

(defun exhub-gitee-dashboard--show-detail (request data)
  "Display the detail buffer for REQUEST rendered from DATA."
  (let ((buffer (exhub-gitee-dashboard--detail-buffer request)))
    (with-current-buffer buffer
      (exhub-gitee-dashboard--image-remove)
      (let ((inhibit-read-only t))
        (condition-case err
            (exhub-gitee-dashboard--render-detail request data)
          (user-error
           (exhub-gitee-dashboard--render-detail-error (error-message-string err))))))
    (pop-to-buffer buffer)))

(defun exhub-gitee-dashboard--detail-loading (request)
  "Show REQUEST's detail buffer with a placeholder while the payload loads."
  (let ((buffer (exhub-gitee-dashboard--detail-buffer request)))
    (with-current-buffer buffer
      (exhub-gitee-dashboard--image-remove)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "#+TITLE: Gitee 详情\n\n")
        (insert (format "* 加载中…\n\n%s\n"
                        (or (plist-get request :title) "")))
        (goto-char (point-min))
        (setq header-line-format " Gitee 详情 | 加载中…")))
    (pop-to-buffer buffer)
    buffer))

(defun exhub-gitee-dashboard--refresh-detail-async (request)
  "Fetch REQUEST in the background and fill its detail buffer.
A stale response (a newer refresh started meanwhile) is dropped."
  (let* ((buffer (exhub-gitee-dashboard--detail-buffer request))
         (kind (if (eq (plist-get request :kind) 'pull) "pull" "issue"))
         (ref (format "%s" (or (plist-get request :ref) "")))
         (generation (with-current-buffer buffer
                       (setq exhub-gitee-dashboard--generation
                             (1+ (or exhub-gitee-dashboard--generation 0))))))
    (with-current-buffer buffer
      (setq header-line-format " Gitee 详情 | 加载中…"))
    (exhub-gitee-dashboard--fetch-detail-async
     kind ref (plist-get request :subject)
     (lambda (data error)
       (when (and (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (and (derived-mode-p 'exhub-gitee-dashboard-detail-mode)
                         (eql generation exhub-gitee-dashboard--generation))))
         (with-current-buffer buffer
           (exhub-gitee-dashboard--image-remove)
           (let ((inhibit-read-only t))
             (condition-case err
                 (if error
                     (exhub-gitee-dashboard--render-detail-error error)
                   (exhub-gitee-dashboard--render-detail request data))
               (error
                (exhub-gitee-dashboard--render-detail-error
                 (error-message-string err)))))))))
    buffer))

(defun exhub-gitee-dashboard--open-detail (request)
  "Display REQUEST's detail, fetching it synchronously or asynchronously."
  (if exhub-gitee-dashboard-async
      (progn
        (exhub-gitee-dashboard--detail-loading request)
        (exhub-gitee-dashboard--refresh-detail-async request))
    (exhub-gitee-dashboard--show-detail
     request
     (exhub-gitee-dashboard--fetch-detail
      (if (eq (plist-get request :kind) 'pull) "pull" "issue")
      (plist-get request :ref)
      (plist-get request :subject)))))

;;;###autoload
(defun exhub-gitee-dashboard-detail-refresh ()
  "Refetch and redraw the current Gitee detail buffer."
  (interactive)
  (let ((request exhub-gitee-dashboard--detail-request))
    (unless (and (derived-mode-p 'exhub-gitee-dashboard-detail-mode) request)
      (user-error "Not in a Gitee detail buffer"))
    (exhub-gitee-dashboard--open-detail request)
    (message "Gitee detail refreshed")))

;;;###autoload
(defun exhub-gitee-dashboard-detail-browse ()
  "Open the pull request / issue of the current detail buffer in a browser."
  (interactive)
  (if-let ((url (plist-get exhub-gitee-dashboard--detail-request :url)))
      (browse-url url)
    (user-error "No web URL for this item")))

;;;###autoload
(defun exhub-gitee-dashboard-detail-diff ()
  "Open the diff of the pull request of the current detail buffer.
The diff goes to its own buffer, where Magit's sections render it: `TAB'
collapses and expands a file or a hunk."
  (interactive)
  (let ((request exhub-gitee-dashboard--detail-request))
    (unless (and (derived-mode-p 'exhub-gitee-dashboard-detail-mode) request)
      (user-error "Not in a Gitee detail buffer"))
    (unless (eq (plist-get request :kind) 'pull)
      (user-error "Only a pull request has a diff"))
    (unless (plist-get request :subject)
      (user-error "No project id for this pull request; its diff is unreachable"))
    (exhub-gitee-dashboard--open-diff request)))

;;;###autoload
(defun exhub-gitee-dashboard-detail-toggle-images ()
  "Toggle the inline images of the current Gitee detail buffer."
  (interactive)
  (unless (derived-mode-p 'exhub-gitee-dashboard-detail-mode)
    (user-error "Not in a Gitee detail buffer"))
  (exhub-gitee-dashboard--image-toggle))

(provide 'exhub-gitee-dashboard-detail)

;;; exhub-gitee-dashboard-detail.el ends here
