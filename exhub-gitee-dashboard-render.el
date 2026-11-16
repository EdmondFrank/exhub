;;; exhub-gitee-dashboard-render.el --- Gitee Enterprise dashboard: rendering -*- lexical-binding: t; -*-

;; Copyright (C) 2025 EdmondFrank

;; Author: EdmondFrank
;; Keywords: tools, gitee, dashboard

;;; Commentary:

;; Render the dashboard payload into the read-only Org buffer, including the
;; error page and the fetch-and-render entry used by the mode.

;;; Code:

(require 'seq)

(require 'exhub-gitee-dashboard-vars)
(require 'exhub-gitee-dashboard-fetch)
(require 'exhub-gitee-dashboard-format)

(defun exhub-gitee-dashboard--render (data)
  "Render the dashboard payload DATA in the current buffer."
  (let* ((enterprise (exhub-gitee-dashboard--get data "enterprise"))
         (ent-name (or (exhub-gitee-dashboard--get enterprise "name") "Gitee 企业"))
         (ent-path (exhub-gitee-dashboard--get enterprise "path"))
         (ent-id (or (exhub-gitee-dashboard--get data "enterprise_id")
                     (number-to-string exhub-gitee-dashboard-enterprise-id)))
         (generated (exhub-gitee-dashboard--get data "generated_at"))
         (committers (or (exhub-gitee-dashboard--get data "committers") '()))
         (pulls (or (exhub-gitee-dashboard--get data "pulls") '()))
         (issues (or (exhub-gitee-dashboard--get data "issues") '()))
         (pulls-open (seq-count
                      (lambda (pull)
                        (equal "opened" (exhub-gitee-dashboard--get pull "state")))
                      pulls)))
    (erase-buffer)
    (insert (format "#+TITLE: %s · Gitee 企业仪表盘\n" ent-name))
    (insert "#+OPTIONS: toc:nil num:nil\n")
    (insert (format "# 企业: %s (id %s) · 生成于 %s\n\n"
                    (or ent-path ent-name) ent-id (or generated "-")))
    (insert "* 企业概览\n\n")
    (exhub-gitee-dashboard--insert-table
     '("指标" "今日" "本周" "本月")
     (exhub-gitee-dashboard--period-rows
      (exhub-gitee-dashboard--get data "overview")))
    (insert "\n* 我的概览\n\n")
    (exhub-gitee-dashboard--insert-table
     '("指标" "今日" "本周" "本月")
     (exhub-gitee-dashboard--period-rows
      (exhub-gitee-dashboard--get data "mine")))
    (insert (format "\n* 本月提交排行 Top %d\n\n" exhub-gitee-dashboard-top-n))
    (if committers
        (let ((rows (exhub-gitee-dashboard--committer-rows committers ent-path)))
          (exhub-gitee-dashboard--insert-table
           '("#" "成员" "提交数") (car rows) (cdr rows)))
      (insert "暂无数据。\n"))
    (insert "\n* 规模\n\n")
    (exhub-gitee-dashboard--insert-table
     '("仓库" "项目" "成员")
     (list (list (exhub-gitee-dashboard--num (exhub-gitee-dashboard--get data "projects"))
                 (exhub-gitee-dashboard--num (exhub-gitee-dashboard--get data "programs"))
                 (exhub-gitee-dashboard--num (exhub-gitee-dashboard--get data "members")))))
    (let ((rows (exhub-gitee-dashboard--pull-rows pulls)))
      (insert (format "\n* 我参与的 Pull Requests（显示 %d 条，其中进行中 %d）\n\n"
                      (length (car rows)) pulls-open))
      (if (car rows)
          (exhub-gitee-dashboard--insert-table
           '("标题" "仓库" "状态" "更新") (car rows) (cdr rows))
        (insert "没有进行中的 Pull Request。\n")))
    (let ((rows (exhub-gitee-dashboard--issue-rows issues ent-path)))
      (insert (format "\n* 我相关的 Issue（共 %s 个开启，显示 %d 条）\n\n"
                      (exhub-gitee-dashboard--num
                       (exhub-gitee-dashboard--get data "issues_total"))
                      (length (car rows))))
      (if (car rows)
          (exhub-gitee-dashboard--insert-table
           '("ID" "标题" "状态" "负责人" "截止") (car rows) (cdr rows))
        (insert "没有开启的 Issue。\n")))
    ;; Align the fresh tables now instead of waiting for the next
    ;; jit-lock pass.  Only when the buffer is on screen: valign measures
    ;; glyph widths, which needs a live window.
    (when (and (bound-and-true-p valign-mode)
               (get-buffer-window (current-buffer) t))
      (valign-region (point-min) (point-max)))
    (goto-char (point-min))
    (setq header-line-format
          (format " %s | 更新 %s | RET 详情  o 浏览器  g 刷新  G 切换企业  q 退出"
                  ent-name (or generated "-")))))

(defun exhub-gitee-dashboard--render-error (message)
  "Render an error page containing MESSAGE."
  (erase-buffer)
  (insert "#+TITLE: Gitee 企业仪表盘\n\n")
  (insert "* 无法加载仪表盘\n\n")
  (insert (format "错误: %s\n\n" message))
  (insert "* 排查提示\n\n")
  (insert "- 确认 vault 已解锁: =ENVLESS_ROOT=~/GTD envless list=\n")
  (insert (format "- 确认密钥 =%s= 存在（envless 退出码 64/66/255）\n"
                  exhub-gitee-dashboard-token-env-var))
  (insert "- 确认企业 MCP 令牌仍有访问权限（Gitee 个人设置 → MCP 企业令牌）\n")
  (insert "- 按 =g= 重试\n")
  (goto-char (point-min))
  (setq header-line-format " Gitee 企业仪表盘 | 加载失败 | g 重试  q 退出"))

(defun exhub-gitee-dashboard--refresh ()
  "Fetch and render the dashboard in the current buffer."
  (let ((inhibit-read-only t))
    (condition-case err
        (exhub-gitee-dashboard--render (exhub-gitee-dashboard--fetch))
      (user-error
       (exhub-gitee-dashboard--render-error (error-message-string err))))))

(provide 'exhub-gitee-dashboard-render)

;;; exhub-gitee-dashboard-render.el ends here
