;;; exhub-gitee-dashboard.el --- Gitee Enterprise org dashboard -*- lexical-binding: t; -*-

;; Copyright (C) 2025 EdmondFrank

;; Author: EdmondFrank
;; Keywords: tools, gitee, dashboard

;;; Commentary:

;; Renders a Gitee Enterprise (企业版) organisation dashboard in an Org buffer.
;;
;; Data comes from the Gitee Enterprise API (Grape v8, mounted at
;; https://api.gitee.com/enterprises/<id>) using the *enterprise MCP token*
;; created at /profile/mcp_gitee_ent_access_tokens.  Per
;; app/api/enterprise_v1/helpers/api_helper.rb the MCP application is only
;; authorised when the request carries a `User-Agent' that contains
;; "mcp-gitee-ent".
;;
;; Retrieval runs in the ExHub backend (`Exhub.Gitee.Dashboard'): the buffer
;; asks `GET /gitee/dashboard' (or `GET /gitee/detail') for one merged payload,
;; and the backend fans the individual enterprise calls out concurrently.  The
;; Enterprise MCP token is read there from ExHub's SecretVault, so Emacs never
;; sees it and the fetch does not shell out to envless/curl/jq.
;;
;; Endpoints used (see ~/Code/gitee/app/api/enterprise_v1/):
;;   GET /enterprises/<id>                                 enterprise info
;;   GET /enterprises/<id>/dashboard_statistics/overview   org overview
;;   GET /enterprises/<id>/dashboard_statistics/user_dashboard  my overview
;;   GET /enterprises/<id>/statistics/top_committer        month commit ranking
;;   GET /enterprises/<id>/projects|programs|members       org scale
;;   GET /enterprises/<id>/programs?sort=updated_at        programs section
;;   GET /enterprises/<id>/projects?sort=last_push_at      repository section
;;   GET /enterprises/<id>/pull_requests?scope=related_to_me
;;   GET /enterprises/<id>/issues?only_related_me=1&state=open
;;
;; Program detail view (RET on a program row):
;;   GET /enterprises/<id>/programs/<program_id>
;;   GET /enterprises/<id>/programs/<program_id>/projects?per_page=<n>
;;
;; Detail view (RET on a pull request or issue row):
;;   GET /enterprises/<id>/projects/<project_id>/pull_requests/<iid>?pr_qt=iid
;;   GET /enterprises/<id>/projects/<project_id>/pull_requests/<iid>/notes?pr_qt=iid
;;   GET /enterprises/<id>/issues/<ident>?qt=ident
;;   GET /enterprises/<id>/issues/<ident>/notes?qt=ident
;; The project-scoped routes need the *numeric* project id; the API's
;; `:project_id' segment only accepts one path element, so
;; `path_with_namespace' answers 404.
;;
;; The Org tables contain Chinese text, so the dashboard buffer enables
;; `valign-mode' (when installed) for pixel-accurate column alignment.
;;
;; The implementation is split across sibling files of the same prefix:
;;   exhub-gitee-dashboard-vars.el    user options and buffer-local state
;;   exhub-gitee-dashboard-fetch.el   backend HTTP retrieval (sync + async)
;;   exhub-gitee-dashboard-format.el  JSON accessors and Org table helpers
;;   exhub-gitee-dashboard-render.el  dashboard rendering
;;   exhub-gitee-dashboard-markdown.el  Markdown to Org conversion
;;   exhub-gitee-dashboard-programs.el  programs (项目集) and repositories
;;   exhub-gitee-dashboard-detail.el  pull request / issue detail buffer
;;   exhub-gitee-dashboard-image.el   inline images of a detail
;; This file requires them and defines the dashboard mode and commands.

;;; Code:

(require 'org)
(require 'subr-x)

(require 'exhub-gitee-dashboard-vars)
(require 'exhub-gitee-dashboard-fetch)
(require 'exhub-gitee-dashboard-format)
(require 'exhub-gitee-dashboard-render)
(require 'exhub-gitee-dashboard-programs)
(require 'exhub-gitee-dashboard-detail)

;;; Commands and mode

(defvar exhub-gitee-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    map)
  "Keymap for `exhub-gitee-dashboard-mode'.")

(define-derived-mode exhub-gitee-dashboard-mode org-mode "GiteeDash"
  "Major mode for the Gitee Enterprise dashboard buffer."
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil)
  (when (and exhub-gitee-dashboard-use-valign
             (require 'valign nil t))
    (valign-mode 1))
  (setq-local header-line-format " Gitee 企业仪表盘 | 加载中…")
  (add-hook 'kill-buffer-hook #'exhub-gitee-dashboard--cancel-timer nil t)
  (exhub-gitee-dashboard--schedule-refresh))

(define-key exhub-gitee-dashboard-mode-map (kbd "g") #'exhub-gitee-dashboard-refresh)
(define-key exhub-gitee-dashboard-mode-map (kbd "G") #'exhub-gitee-dashboard-set-enterprise)
(define-key exhub-gitee-dashboard-mode-map (kbd "q") #'quit-window)
(define-key exhub-gitee-dashboard-mode-map (kbd "RET")
  #'exhub-gitee-dashboard-open-detail)
(define-key exhub-gitee-dashboard-mode-map (kbd "o")
  #'exhub-gitee-dashboard-browse)

(defun exhub-gitee-dashboard--cancel-timer ()
  "Cancel the auto-refresh timer of the current buffer."
  (when (timerp exhub-gitee-dashboard--timer)
    (cancel-timer exhub-gitee-dashboard--timer)
    (setq exhub-gitee-dashboard--timer nil)))

(defun exhub-gitee-dashboard--schedule-refresh ()
  "Schedule the next auto-refresh, when enabled."
  (exhub-gitee-dashboard--cancel-timer)
  (when (and exhub-gitee-dashboard-auto-refresh-interval
             (> exhub-gitee-dashboard-auto-refresh-interval 0))
    (setq exhub-gitee-dashboard--timer
          (run-at-time
           exhub-gitee-dashboard-auto-refresh-interval nil
           (lambda (buffer)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (derived-mode-p 'exhub-gitee-dashboard-mode)
                   (exhub-gitee-dashboard--refresh)
                   (exhub-gitee-dashboard--schedule-refresh)))))
           (current-buffer)))))

;;;###autoload
(defun exhub-gitee-dashboard-open-detail ()
  "Open the pull request, issue or program on the current dashboard line.
A pull request / issue detail is rendered in its own read-only buffer; a
program opens its own buffer listing its repositories; a repository row
opens the web page.  Use `o' to open the web page instead."
  (interactive)
  (let* ((meta (exhub-gitee-dashboard--row-meta))
         (kind (plist-get meta :kind))
         (ref (plist-get meta :ref)))
    (cond
     ((null meta)
      (exhub-gitee-dashboard--open-link))
     ((eq kind 'program)
      (exhub-gitee-dashboard--open-program meta))
     ((eq kind 'project)
      (if-let ((url (plist-get meta :url)))
          (browse-url url)
        (exhub-gitee-dashboard--open-link)))
     ((or (null ref) (eq kind 'member))
      (if-let ((url (plist-get meta :url)))
          (browse-url url)
        (exhub-gitee-dashboard--open-link)))
     (t
      (exhub-gitee-dashboard--open-detail meta)))))

;;;###autoload
(defun exhub-gitee-dashboard-browse ()
  "Open the web page of the current dashboard line in a browser."
  (interactive)
  (let ((meta (exhub-gitee-dashboard--row-meta)))
    (if-let ((url (plist-get meta :url)))
        (browse-url url)
      (exhub-gitee-dashboard--open-link))))

(defun exhub-gitee-dashboard--open-link ()
  "Open the Org link at point, or signal a helpful error."
  (condition-case nil
      (org-open-at-point)
    (error
     (user-error "No pull request, issue or link on this line"))))

;;;###autoload
(defun exhub-gitee-dashboard-refresh ()
  "Refresh the Gitee Enterprise dashboard buffer."
  (interactive)
  (unless (derived-mode-p 'exhub-gitee-dashboard-mode)
    (user-error "Not in a Gitee dashboard buffer"))
  (exhub-gitee-dashboard--refresh)
  (exhub-gitee-dashboard--schedule-refresh)
  (message (if exhub-gitee-dashboard-async
               "Gitee dashboard refreshing…"
             "Gitee dashboard refreshed")))

;;;###autoload
(defun exhub-gitee-dashboard-set-enterprise (id)
  "Switch the dashboard to enterprise ID and refresh."
  (interactive
   (list (read-number "Enterprise id: " exhub-gitee-dashboard-enterprise-id)))
  (setq exhub-gitee-dashboard-enterprise-id id)
  (when-let ((buffer (get-buffer exhub-gitee-dashboard-buffer-name)))
    (with-current-buffer buffer
      (exhub-gitee-dashboard--refresh)
      (exhub-gitee-dashboard--schedule-refresh))))

;;;###autoload
(defun exhub-gitee-dashboard (&optional arg)
  "Open the Gitee Enterprise organisation dashboard.
With prefix ARG, prompt for the enterprise id first."
  (interactive "P")
  (when arg
    (call-interactively #'exhub-gitee-dashboard-set-enterprise))
  (let ((buffer (get-buffer-create exhub-gitee-dashboard-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'exhub-gitee-dashboard-mode)
        (exhub-gitee-dashboard-mode)))
    (pop-to-buffer buffer)
    (exhub-gitee-dashboard--refresh)
    (exhub-gitee-dashboard--schedule-refresh)))

(provide 'exhub-gitee-dashboard)

;;; exhub-gitee-dashboard.el ends here
