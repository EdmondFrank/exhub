;;; exhub-gitee-dashboard-programs.el --- Gitee Enterprise dashboard: programs and repositories -*- lexical-binding: t; -*-

;; Copyright (C) 2025 EdmondFrank

;; Author: EdmondFrank
;; Keywords: tools, gitee, dashboard

;;; Commentary:

;; Programs (项目集) and repositories (仓库) of the Gitee Enterprise
;; dashboard.  The dashboard lists both: `--insert-program-section' and
;; `--insert-project-section' are called by `exhub-gitee-dashboard-render.el'
;; after the org scale table.  RET on a program row opens
;; `exhub-gitee-dashboard-program-mode', a read-only Org buffer holding the
;; program header, its description, its unfinished tasks (任务) and its
;; repositories, fetched from `GET /gitee/program'
;; (`Exhub.Gitee.Dashboard.program/2').  RET on a task opens its detail
;; buffer, on a repository the web page.
;;
;; Upstream ids are kept as-is: status 0/1/2 is 开始/暂停/关闭 and category is
;; scrum/kanban/standard/custom.

;;; Code:

(require 'org)
(require 'subr-x)

(require 'exhub-gitee-dashboard-vars)
(require 'exhub-gitee-dashboard-fetch)
(require 'exhub-gitee-dashboard-format)
(require 'exhub-gitee-dashboard-markdown)

;; Defined in `exhub-gitee-dashboard.el' and `exhub-gitee-dashboard-detail.el',
;; which require this file: referenced at run time only, to avoid a load cycle.
(declare-function exhub-gitee-dashboard--open-link "exhub-gitee-dashboard")
(declare-function exhub-gitee-dashboard--open-detail "exhub-gitee-dashboard-detail")

(defconst exhub-gitee-dashboard-program-statuses
  '((0 . "开始") (1 . "暂停") (2 . "关闭"))
  "Program status labels keyed by the Gitee status id.")

(defconst exhub-gitee-dashboard-program-categories
  '(("scrum" . "敏捷")
    ("kanban" . "看板")
    ("standard" . "标准")
    ("custom" . "自定义"))
  "Program category labels keyed by the Gitee category id.")

(defun exhub-gitee-dashboard--program-status (status)
  "Return a Chinese label for the program STATUS."
  (or (cdr (assoc status exhub-gitee-dashboard-program-statuses))
      (if (null status) "-" (format "%s" status))))

(defun exhub-gitee-dashboard--program-category (category)
  "Return a Chinese label for the program CATEGORY."
  (or (cdr (assoc category exhub-gitee-dashboard-program-categories))
      (if (null category) "-" (format "%s" category))))

(defun exhub-gitee-dashboard--program-url (ent-path id)
  "Return the web URL of program ID within enterprise path ENT-PATH."
  (when (and ent-path id)
    (format "https://gitee.com/%s/dashboard/programs/%s" ent-path id)))

(defun exhub-gitee-dashboard--project-url (path)
  "Return the web URL of the repository at PATH."
  (when (and (stringp path) (not (string-empty-p path)))
    (format "https://gitee.com/%s" path)))

(defun exhub-gitee-dashboard--completion (closed total)
  "Return CLOSED out of TOTAL as a percentage string, or \"-\"."
  (if (and (integerp closed) (integerp total) (> total 0))
      (format "%d%%" (round (* 100.0 (/ (float closed) total))))
    "-"))

(defun exhub-gitee-dashboard--program-rows (programs ent-path)
  "Return (ROWS . METAS) for the PROGRAMS of enterprise path ENT-PATH."
  (let (rows metas)
    (dolist (program programs)
      (let* ((id (exhub-gitee-dashboard--get program "id"))
             (ident (or (exhub-gitee-dashboard--get program "ident")
                        (and id (format "%s" id))))
             (name (or (exhub-gitee-dashboard--get program "name") ident))
             (url (exhub-gitee-dashboard--program-url ent-path id)))
        (push (list (exhub-gitee-dashboard--cell ident)
                    (if url
                        (exhub-gitee-dashboard--link url name)
                      (exhub-gitee-dashboard--cell name))
                    (exhub-gitee-dashboard--program-category
                     (exhub-gitee-dashboard--get program "category"))
                    (exhub-gitee-dashboard--program-status
                     (exhub-gitee-dashboard--get program "status"))
                    (exhub-gitee-dashboard--num
                     (exhub-gitee-dashboard--get program "users_count"))
                    (exhub-gitee-dashboard--num
                     (exhub-gitee-dashboard--get program "projects_count"))
                    (exhub-gitee-dashboard--num
                     (exhub-gitee-dashboard--get program "issues_count"))
                    (exhub-gitee-dashboard--completion
                     (exhub-gitee-dashboard--get program "closed_issues_count")
                     (exhub-gitee-dashboard--get program "issues_count")))
              rows)
        (push (list :kind 'program
                    :id (and id (format "%s" id))
                    :name name
                    :url url
                    :ent-path ent-path)
              metas)))
    (cons (nreverse rows) (nreverse metas))))

(defun exhub-gitee-dashboard--project-rows (projects &optional total-pr-only)
  "Return (ROWS . METAS) for the repositories in PROJECTS.
With TOTAL-PR-ONLY non-nil the pull request column holds the total only:
`GET .../programs/<id>/projects' does not return `open_pr_count', so an
\"open / total\" pair would always read 0."
  (let (rows metas)
    (dolist (project projects)
      (let* ((path (exhub-gitee-dashboard--get project "path_with_namespace"))
             (name (or (exhub-gitee-dashboard--get project "name") path))
             (url (exhub-gitee-dashboard--project-url path))
             (open (exhub-gitee-dashboard--get project "open_pr_count"))
             (total (exhub-gitee-dashboard--get project "total_pr_count")))
        (push (list (if url
                        (exhub-gitee-dashboard--link url name)
                      (exhub-gitee-dashboard--cell name))
                    (exhub-gitee-dashboard--cell path)
                    (exhub-gitee-dashboard--num
                     (exhub-gitee-dashboard--get project "members_count"))
                    (cond
                     (total-pr-only
                      (exhub-gitee-dashboard--num total))
                     ((or open total)
                      (format "%s / %s"
                              (exhub-gitee-dashboard--num open)
                              (exhub-gitee-dashboard--num total)))
                     (t "-"))
                    (exhub-gitee-dashboard--date
                     (exhub-gitee-dashboard--get project "last_push_at")))
              rows)
        (push (list :kind 'project :name name :url url) metas)))
    (cons (nreverse rows) (nreverse metas))))

;;; Dashboard sections

(defun exhub-gitee-dashboard--insert-program-section (data)
  "Insert the programs (项目集) section of the dashboard payload DATA."
  (let* ((ent-path (exhub-gitee-dashboard--path data "enterprise" "path"))
         (items (or (exhub-gitee-dashboard--get data "program_items") '()))
         (total (exhub-gitee-dashboard--get data "programs")))
    (insert (format "\n* 项目集（显示 %d 个，共 %s 个）\n\n"
                    (length items) (exhub-gitee-dashboard--num total)))
    (if items
        (let ((rows (exhub-gitee-dashboard--program-rows items ent-path)))
          (exhub-gitee-dashboard--insert-table
           '("编号" "名称" "类型" "状态" "成员" "仓库" "任务" "完成率")
           (car rows) (cdr rows)))
      (insert "暂无项目集。\n"))))

(defun exhub-gitee-dashboard--insert-project-section (data)
  "Insert the repository (仓库) section of the dashboard payload DATA."
  (let* ((items (or (exhub-gitee-dashboard--get data "project_items") '()))
         (total (exhub-gitee-dashboard--get data "projects")))
    (insert (format "\n* 仓库（最近推送 %d 个，共 %s 个）\n\n"
                    (length items) (exhub-gitee-dashboard--num total)))
    (if items
        (let ((rows (exhub-gitee-dashboard--project-rows items)))
          (exhub-gitee-dashboard--insert-table
           '("仓库" "路径" "成员" "PR（开启/全部）" "最近推送")
           (car rows) (cdr rows)))
      (insert "暂无仓库。\n"))))

;;; Program buffer

(defvar exhub-gitee-dashboard-program-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    map)
  "Keymap for `exhub-gitee-dashboard-program-mode'.")

(defvar-local exhub-gitee-dashboard--program-request nil
  "Request plist of the current program buffer.
Holds :id, :name, :url and :ent-path.")

(define-derived-mode exhub-gitee-dashboard-program-mode org-mode "GiteeProgram"
  "Major mode for a Gitee program (项目集) buffer."
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil)
  (when (and exhub-gitee-dashboard-use-valign
             (require 'valign nil t))
    (valign-mode 1))
  (setq-local header-line-format " Gitee 项目集 | 加载中…"))

(define-key exhub-gitee-dashboard-program-mode-map (kbd "g")
  #'exhub-gitee-dashboard-program-refresh)
(define-key exhub-gitee-dashboard-program-mode-map (kbd "o")
  #'exhub-gitee-dashboard-program-browse)
(define-key exhub-gitee-dashboard-program-mode-map (kbd "RET")
  #'exhub-gitee-dashboard-program-open-at-point)
(define-key exhub-gitee-dashboard-program-mode-map (kbd "q") #'quit-window)

(defun exhub-gitee-dashboard--program-buffer (request)
  "Return the program buffer of REQUEST, in program mode, with REQUEST stored."
  (let ((buffer (get-buffer-create
                 (format exhub-gitee-dashboard-program-buffer-format
                         (or (plist-get request :id) "-")))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'exhub-gitee-dashboard-program-mode)
        (exhub-gitee-dashboard-program-mode))
      (setq-local exhub-gitee-dashboard--program-request request))
    buffer))

(defun exhub-gitee-dashboard--render-program (request data)
  "Render the program payload DATA described by REQUEST."
  (let* ((program (or (exhub-gitee-dashboard--get data "program") '()))
         (projects (or (exhub-gitee-dashboard--get data "projects") '()))
         (total (exhub-gitee-dashboard--get data "projects_total"))
         (issues (or (exhub-gitee-dashboard--get data "issues") '()))
         (issues-total (exhub-gitee-dashboard--get data "issues_total"))
         (generated (exhub-gitee-dashboard--get data "generated_at"))
         (name (or (exhub-gitee-dashboard--get program "name")
                   (plist-get request :name)
                   "-"))
         (ident (exhub-gitee-dashboard--get program "ident"))
         (status (exhub-gitee-dashboard--program-status
                  (exhub-gitee-dashboard--get program "status")))
         (category (exhub-gitee-dashboard--program-category
                    (exhub-gitee-dashboard--get program "category")))
         (assignee (or (exhub-gitee-dashboard--path program "assignee" "name")
                       (exhub-gitee-dashboard--path program "assignee" "username")))
         (description (exhub-gitee-dashboard--get program "description"))
         (ent-path (plist-get request :ent-path))
         (url (plist-get request :url)))
    (erase-buffer)
    (insert (format "#+TITLE: %s · 项目集\n\n" name))
    (insert (format "* %s\n\n" name))
    (insert (format "- 编号: =%s= · 类型: %s · 状态: %s\n"
                    (or ident "-") category status))
    (exhub-gitee-dashboard--insert-field "负责人" assignee)
    (insert (format "- 创建: %s\n"
                    (exhub-gitee-dashboard--date
                     (exhub-gitee-dashboard--get program "created_at"))))
    (when url
      (insert (format "- 网页: %s\n"
                      (exhub-gitee-dashboard--link url "在浏览器打开"))))
    (insert "\n* 描述\n\n")
    (unless (exhub-gitee-dashboard--insert-markdown description 1)
      (insert "（无描述）\n"))
    (insert (format "\n* 未完成任务（显示 %d 条，共 %s 条）\n\n"
                    (length issues) (exhub-gitee-dashboard--num issues-total)))
    (if issues
        (let ((rows (exhub-gitee-dashboard--issue-rows issues ent-path)))
          (exhub-gitee-dashboard--insert-table
           '("ID" "标题" "状态" "负责人" "截止") (car rows) (cdr rows)))
      (insert "没有未完成的任务。\n"))
    (insert (format "\n* 仓库（显示 %d 个，共 %s 个）\n\n"
                    (length projects) (exhub-gitee-dashboard--num total)))
    (if projects
        (let ((rows (exhub-gitee-dashboard--project-rows projects t)))
          (exhub-gitee-dashboard--insert-table
           '("仓库" "路径" "成员" "PR 数" "最近推送")
           (car rows) (cdr rows)))
      (insert "暂无仓库。\n"))
    (when (and (bound-and-true-p valign-mode)
               (get-buffer-window (current-buffer) t))
      (valign-region (point-min) (point-max)))
    (goto-char (point-min))
    (setq header-line-format
          (format " %s · 项目集 | 更新 %s | RET 打开任务/仓库  o 项目集  g 刷新  q 退出"
                  name (or generated "-")))))

(defun exhub-gitee-dashboard--render-program-error (message)
  "Render a program error page containing MESSAGE."
  (erase-buffer)
  (insert "#+TITLE: Gitee 项目集\n\n* 无法加载项目集\n\n")
  (insert (format "错误: %s\n\n" message))
  (insert (format "- 确认 ExHub 后端在运行: =curl %s/gitee/program=\n"
                  (exhub-gitee-dashboard--backend-url)))
  (insert "- 确认 =gitee_ent_mcp_access_token= 已存入 ExHub SecretVault\n")
  (insert "- 按 =g= 重试\n")
  (goto-char (point-min))
  (setq header-line-format " Gitee 项目集 | 加载失败 | g 重试  q 退出"))

(defun exhub-gitee-dashboard--program-loading (request)
  "Show REQUEST's program buffer with a placeholder while it loads."
  (let ((buffer (exhub-gitee-dashboard--program-buffer request)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "#+TITLE: Gitee 项目集\n\n")
        (insert (format "* 加载中…\n\n%s\n"
                        (or (plist-get request :name) "")))
        (goto-char (point-min))
        (setq header-line-format " Gitee 项目集 | 加载中…")))
    (pop-to-buffer buffer)
    buffer))

(defun exhub-gitee-dashboard--show-program (request data)
  "Display the program buffer for REQUEST rendered from DATA."
  (let ((buffer (exhub-gitee-dashboard--program-buffer request)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (condition-case err
            (exhub-gitee-dashboard--render-program request data)
          (user-error
           (exhub-gitee-dashboard--render-program-error
            (error-message-string err))))))
    (pop-to-buffer buffer)))

(defun exhub-gitee-dashboard--refresh-program-async (request)
  "Fetch REQUEST in the background and fill its program buffer.
A stale response (a newer refresh started meanwhile) is dropped."
  (let* ((buffer (exhub-gitee-dashboard--program-buffer request))
         (id (plist-get request :id))
         (generation (with-current-buffer buffer
                       (setq exhub-gitee-dashboard--generation
                             (1+ (or exhub-gitee-dashboard--generation 0))))))
    (with-current-buffer buffer
      (setq header-line-format " Gitee 项目集 | 加载中…"))
    (exhub-gitee-dashboard--fetch-program-async
     id
     (lambda (data error)
       (when (and (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (and (derived-mode-p 'exhub-gitee-dashboard-program-mode)
                         (eql generation exhub-gitee-dashboard--generation))))
         (with-current-buffer buffer
           (let ((inhibit-read-only t))
             (condition-case err
                 (if error
                     (exhub-gitee-dashboard--render-program-error error)
                   (exhub-gitee-dashboard--render-program request data))
               (error
                (exhub-gitee-dashboard--render-program-error
                 (error-message-string err)))))))))
    buffer))

(defun exhub-gitee-dashboard--open-program (request)
  "Display REQUEST's program, fetching it synchronously or asynchronously."
  (unless (plist-get request :id)
    (user-error "No program id on this line"))
  (if exhub-gitee-dashboard-async
      (progn
        (exhub-gitee-dashboard--program-loading request)
        (exhub-gitee-dashboard--refresh-program-async request))
    (exhub-gitee-dashboard--show-program
     request
     (exhub-gitee-dashboard--fetch-program (plist-get request :id)))))

;;;###autoload
(defun exhub-gitee-dashboard-program-refresh ()
  "Refetch and redraw the current Gitee program buffer."
  (interactive)
  (let ((request exhub-gitee-dashboard--program-request))
    (unless (and (derived-mode-p 'exhub-gitee-dashboard-program-mode) request)
      (user-error "Not in a Gitee program buffer"))
    (exhub-gitee-dashboard--open-program request)
    (message "Gitee program refreshed")))

;;;###autoload
(defun exhub-gitee-dashboard-program-browse ()
  "Open the program of the current program buffer in a browser."
  (interactive)
  (if-let ((url (plist-get exhub-gitee-dashboard--program-request :url)))
      (browse-url url)
    (user-error "No web URL for this program")))

;;;###autoload
(defun exhub-gitee-dashboard-program-open-at-point ()
  "Open what the current program-buffer line refers to.
A task (任务) opens its detail buffer, a repository its web page, any other
line the Org link at point."
  (interactive)
  (let ((meta (exhub-gitee-dashboard--row-meta)))
    (cond
     ((eq (plist-get meta :kind) 'issue)
      (exhub-gitee-dashboard--open-detail meta))
     ((and (eq (plist-get meta :kind) 'project)
           (plist-get meta :url))
      (browse-url (plist-get meta :url)))
     ((eq (plist-get meta :kind) 'project)
      (user-error "No web URL for this repository"))
     (t
      (exhub-gitee-dashboard--open-link)))))

(provide 'exhub-gitee-dashboard-programs)

;;; exhub-gitee-dashboard-programs.el ends here