;;; exhub-gitee-dashboard-diff.el --- Gitee Enterprise dashboard: pull request diff -*- lexical-binding: t; -*-

;; Copyright (C) 2025 EdmondFrank

;; Author: EdmondFrank
;; Keywords: tools, gitee, dashboard

;;; Commentary:

;; The diff of a pull request lives in its own buffer rather than below the
;; detail: a large diff would bury the discussion.  It is rendered the way Magit
;; renders a diff: Magit's washer turns the raw unified diff into `file' and
;; `hunk' sections, which `TAB' collapses and expands, and the hunk bodies are
;; painted with `magit-diff-added' / `magit-diff-removed'.  `d' in a pull
;; request detail buffer opens it.
;;
;; The buffer is not tied to a repository: it only reuses Magit's diff parser
;; and section machinery on the text returned by `GET /gitee/diff'.  The section
;; commands that would run git (stage, apply, discard, reverse, visit-file) are
;; therefore stripped from the section keymaps, and `revert-buffer' reloads the
;; diff from the backend instead of asking git for one.
;;
;; The buffer fetches only `GET /gitee/diff' (the detail payload does not carry
;; the diff), asynchronously when `exhub-gitee-dashboard-async' is non-nil, and
;; discards a response that a newer refresh has superseded.

;;; Code:

(require 'magit-diff)
(require 'subr-x)

(require 'exhub-gitee-dashboard-vars)
(require 'exhub-gitee-dashboard-fetch)
(require 'exhub-gitee-dashboard-format)

(defvar exhub-gitee-dashboard-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-diff-mode-map)
    map)
  "Keymap for `exhub-gitee-dashboard-diff-mode'.
Section motion and expansion -- `TAB', `S-TAB', `n', `p', `^' and the level
keys -- come from `magit-section-mode-map'.")

(defvar-local exhub-gitee-dashboard--diff-request nil
  "Request plist of the current diff buffer.
Holds :kind, :ref, :subject, :repo, :title and :url.")

(defvar-local exhub-gitee-dashboard--diff-generation 0
  "Identifier of the newest in-flight diff fetch of the current buffer.
A response whose generation no longer matches is discarded, so a slow request
can never overwrite the result of a newer one.")

(define-derived-mode exhub-gitee-dashboard-diff-mode magit-diff-mode "GiteeDiff"
  "Major mode for the diff of a Gitee pull request.
Magit supplies the sections (`TAB' collapses and expands a file or a hunk),
the hunk faces and the section motion; the content is written by the
dashboard, so the buffer is read-only."
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil)
  ;; Nothing here belongs to a repository, so reloading must refetch the diff
  ;; instead of letting Magit run `git diff' in `default-directory'.
  (setq-local revert-buffer-function #'exhub-gitee-dashboard--diff-revert)
  ;; Sections take their keymap from these variables, so emptying them keeps
  ;; Magit's stage/apply/discard/reverse/visit commands out of the buffer.
  (setq-local magit-file-section-map (make-sparse-keymap))
  (setq-local magit-hunk-section-map (make-sparse-keymap))
  (magit-set-header-line-format " Gitee Diff | 加载中…"))

(define-key exhub-gitee-dashboard-diff-mode-map (kbd "TAB")
  #'exhub-gitee-dashboard-diff-toggle-section)
(define-key exhub-gitee-dashboard-diff-mode-map (kbd "RET")
  #'exhub-gitee-dashboard-diff-toggle-section)
(define-key exhub-gitee-dashboard-diff-mode-map (kbd "g")
  #'exhub-gitee-dashboard-diff-refresh)
(define-key exhub-gitee-dashboard-diff-mode-map (kbd "o")
  #'exhub-gitee-dashboard-diff-browse)
(define-key exhub-gitee-dashboard-diff-mode-map (kbd "q") #'quit-window)

(defun exhub-gitee-dashboard--diff-buffer-name (request)
  "Return the diff buffer name of REQUEST."
  (format exhub-gitee-dashboard-diff-buffer-format
          (or (plist-get request :repo) "-")
          (or (plist-get request :ref) "-")))

(defun exhub-gitee-dashboard--diff-lines (diff)
  "Return the unified DIFF as a list of lines, without a trailing empty line."
  (let ((lines (split-string (or diff "") "\n")))
    (if (and lines (not (string-empty-p (car (last lines)))))
        lines
      (butlast lines))))

(defun exhub-gitee-dashboard--diff-stats (lines)
  "Return (FILES ADDED DELETED) counted from the unified diff LINES.
The `+++' / `---' file headers are not changes, so they are skipped before
the one-character added / deleted prefixes are counted."
  (let ((files 0) (added 0) (deleted 0))
    (dolist (line lines)
      (cond
       ((string-prefix-p "diff --git " line) (setq files (1+ files)))
       ((string-prefix-p "+++ " line))
       ((string-prefix-p "--- " line))
       ((string-prefix-p "+" line) (setq added (1+ added)))
       ((string-prefix-p "-" line) (setq deleted (1+ deleted)))))
    (list files added deleted)))

(defun exhub-gitee-dashboard--diff-section-counts ()
  "Return (FILES HUNKS) covered by the sections of the current buffer.
Return nil when the buffer holds no Magit section, i.e. when the diff is being
shown as plain text."
  (when magit-root-section
    (let ((files (oref magit-root-section children)))
      (list (length files)
            (apply #'+ 0 (mapcar (lambda (file) (length (oref file children)))
                                 files))))))

(defun exhub-gitee-dashboard--diff-header-line (request lines stats hunks
                                                       generated fallback)
  "Return the header line of a diff buffer for REQUEST.
LINES is the number of diff lines, STATS the (FILES ADDED DELETED) triple,
HUNKS the number of hunks Magit parsed (nil when the diff is plain text),
GENERATED the backend timestamp and FALLBACK non-nil when Magit could not
parse the diff."
  (pcase-let ((`(,files ,added ,deleted) stats))
    (format (concat " %s!%s · Diff | %d 个文件 · +%d / -%d · %d 行%s%s"
                    " | 更新 %s | TAB 折叠  S-TAB 全部  g 刷新  o 浏览器  q 退出")
            (or (plist-get request :repo) "-")
            (or (plist-get request :ref) "-")
            files added deleted lines
            (if hunks (format " · %d hunk" hunks) "")
            (if fallback "（纯文本回退）" "")
            (or generated "-"))))

(defun exhub-gitee-dashboard--diff-strip-prefixes ()
  "Drop the a/ and b/ prefixes Gitee puts on the file headers of a diff.
Magit's washer expects the `--no-prefix' form of the headers, which `git diff
--no-prefix' produces; left as they are, every file would be shown as
`a/x -> b/x', i.e. as if it had been renamed."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (cond
       ((looking-at "^diff --git a/\\(.+\\) b/\\(.+\\)$")
        (let ((old (match-string 1))
              (new (match-string 2)))
          (when (equal old new)
            (replace-match (concat "diff --git " old " " old) t t))))
       ((looking-at "^--- a/\\(.+\\)$")
        (replace-match (concat "--- " (match-string 1)) t t))
       ((looking-at "^\\+\\+\\+ b/\\(.+\\)$")
        (replace-match (concat "+++ " (match-string 1)) t t)))
      (forward-line 1))))

(defun exhub-gitee-dashboard--diff-wash (diff)
  "Replace the buffer content with DIFF, washed into Magit sections.
Return non-nil when Magit parsed DIFF, in which case the buffer shows file and
hunk sections.  Otherwise it keeps the plain text, which is still readable but
has no sections and no hunk faces."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert diff)
    (unless (string-suffix-p "\n" diff) (insert "\n"))
    (goto-char (point-min))
    (exhub-gitee-dashboard--diff-strip-prefixes)
    (let ((washed
           (condition-case err
               (progn
                 ;; Reusing the previous tree lets a collapsed file stay
                 ;; collapsed across a refresh, exactly as in a Magit buffer.
                 (magit-insert-section (diffbuf)
                   (magit-diff-wash-diffs '("--no-prefix")))
                 ;; Magit paints the hunk bodies right after a refresh.
                 (magit-section-show magit-root-section)
                 ;; A diff Magit recognizes always yields at least one file.
                 (> (length (oref magit-root-section children)) 0))
             (error
              (message "Gitee diff: Magit could not parse the diff: %s"
                       (error-message-string err))
              nil))))
      (goto-char (point-min))
      (unless washed
        ;; Keep the text, which is still better than an empty buffer.
        (setq magit-root-section nil)
        (erase-buffer)
        (insert diff)
        (unless (string-suffix-p "\n" diff) (insert "\n"))
        (goto-char (point-min)))
      washed)))

(defun exhub-gitee-dashboard--render-diff (request data)
  "Render the diff payload DATA described by REQUEST into the current buffer."
  (let* ((diff (or (exhub-gitee-dashboard--get data "diff") ""))
         (empty (string-empty-p (string-trim diff)))
         washed)
    (if empty
        (let ((inhibit-read-only t))
          (setq magit-root-section nil)
          (erase-buffer)
          (insert "（无变更）\n")
          (goto-char (point-min)))
      (setq washed (exhub-gitee-dashboard--diff-wash diff)))
    (let* ((lines (exhub-gitee-dashboard--diff-lines diff))
           (stats (if lines (exhub-gitee-dashboard--diff-stats lines) '(0 0 0)))
           (counts (and washed (exhub-gitee-dashboard--diff-section-counts))))
      (magit-set-header-line-format
       (exhub-gitee-dashboard--diff-header-line
        request
        (length lines)
        stats
        (nth 1 counts)
        (exhub-gitee-dashboard--get data "generated_at")
        (and (not empty) (not washed)))))
    (set-buffer-modified-p nil)))

(defun exhub-gitee-dashboard--diff-error (request message)
  "Replace the diff buffer of REQUEST with an error page for MESSAGE."
  (let ((inhibit-read-only t))
    (setq magit-root-section nil)
    (erase-buffer)
    (insert (format "无法加载 diff: %s!%s\n\n"
                    (or (plist-get request :repo) "-")
                    (or (plist-get request :ref) "-")))
    (insert (format "错误: %s\n\n" message))
    (insert (format "- 确认 ExHub 后端在运行: =curl %s/gitee/diff=\n"
                    (exhub-gitee-dashboard--backend-url)))
    (insert "- 确认 =gitee_ent_mcp_access_token= 已存入 ExHub SecretVault\n")
    (insert "- 按 =g= 重试\n")
    (goto-char (point-min)))
  (magit-set-header-line-format " Gitee Diff | 加载失败 | g 重试  q 退出")
  (set-buffer-modified-p nil))

(defun exhub-gitee-dashboard--diff-buffer (request)
  "Return the diff buffer of REQUEST, in diff mode, with REQUEST stored."
  (let ((buffer (get-buffer-create
                 (exhub-gitee-dashboard--diff-buffer-name request))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'exhub-gitee-dashboard-diff-mode)
        (exhub-gitee-dashboard-diff-mode))
      (setq-local exhub-gitee-dashboard--diff-request request))
    buffer))

(defun exhub-gitee-dashboard--diff-render-or-error (request data error)
  "Fill REQUEST's diff buffer from DATA, or with an error page for ERROR.
Returns the buffer so the caller can display it."
  (let ((buffer (exhub-gitee-dashboard--diff-buffer request)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (condition-case err
            (if error
                (exhub-gitee-dashboard--diff-error request error)
              (exhub-gitee-dashboard--render-diff request data))
          (error
           (exhub-gitee-dashboard--diff-error request
                                              (error-message-string err))))))
    buffer))

(defun exhub-gitee-dashboard--diff-loading (request)
  "Show REQUEST's diff buffer with a placeholder while the diff loads."
  (let ((buffer (exhub-gitee-dashboard--diff-buffer request)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (setq magit-root-section nil)
        (erase-buffer)
        (insert (format "加载 diff 中…  %s!%s\n"
                        (or (plist-get request :repo) "-")
                        (or (plist-get request :ref) "-")))
        (goto-char (point-min)))
      (magit-set-header-line-format " Gitee Diff | 加载中…")
      (set-buffer-modified-p nil))
    (pop-to-buffer buffer)
    buffer))

(defun exhub-gitee-dashboard--refresh-diff-async (request)
  "Fetch REQUEST's diff in the background and fill its diff buffer.
A stale response (a newer refresh started meanwhile) is dropped."
  (let* ((buffer (exhub-gitee-dashboard--diff-buffer request))
         (generation (with-current-buffer buffer
                       (setq exhub-gitee-dashboard--diff-generation
                             (1+ (or exhub-gitee-dashboard--diff-generation 0))))))
    (exhub-gitee-dashboard--fetch-diff-async
     (plist-get request :ref) (plist-get request :subject)
     (lambda (data error)
       (when (and (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (and (derived-mode-p 'exhub-gitee-dashboard-diff-mode)
                         (eql generation exhub-gitee-dashboard--diff-generation))))
         (exhub-gitee-dashboard--diff-render-or-error request data error))))
    buffer))

(defun exhub-gitee-dashboard--fetch-diff-into-buffer (request)
  "Fetch REQUEST's diff into its buffer, leaving display to the caller."
  (exhub-gitee-dashboard--diff-buffer request)
  (if exhub-gitee-dashboard-async
      (exhub-gitee-dashboard--refresh-diff-async request)
    (pcase-let ((`(,data ,error)
                 (condition-case err
                     (list (exhub-gitee-dashboard--fetch-diff
                            (plist-get request :ref)
                            (plist-get request :subject))
                           nil)
                   (error (list nil (error-message-string err))))))
      (exhub-gitee-dashboard--diff-render-or-error request data error))))

(defun exhub-gitee-dashboard--diff-revert (&rest _)
  "Reload the current diff buffer from the backend.
This is the `revert-buffer-function' of `exhub-gitee-dashboard-diff-mode': it
must not fall through to Magit, which would ask git for a diff of
`default-directory' and overwrite the pull request's diff with it."
  (when (and (derived-mode-p 'exhub-gitee-dashboard-diff-mode)
             exhub-gitee-dashboard--diff-request)
    (exhub-gitee-dashboard--fetch-diff-into-buffer
     exhub-gitee-dashboard--diff-request)))

(defun exhub-gitee-dashboard--open-diff (request)
  "Display REQUEST's diff, fetching it synchronously or asynchronously."
  (if exhub-gitee-dashboard-async
      (progn
        (exhub-gitee-dashboard--diff-loading request)
        (exhub-gitee-dashboard--fetch-diff-into-buffer request))
    (pop-to-buffer (exhub-gitee-dashboard--fetch-diff-into-buffer request))))

;;;###autoload
(defun exhub-gitee-dashboard-diff-toggle-section ()
  "Collapse or expand the file or hunk section at point."
  (interactive)
  (let ((section (magit-current-section)))
    (cond ((or (null section) (eq section magit-root-section))
           (user-error "Point is not on a file or hunk heading"))
          ((magit-section-toggle section)))))

;;;###autoload
(defun exhub-gitee-dashboard-diff-refresh ()
  "Refetch and redraw the current Gitee diff buffer."
  (interactive)
  (let ((request exhub-gitee-dashboard--diff-request))
    (unless (and (derived-mode-p 'exhub-gitee-dashboard-diff-mode) request)
      (user-error "Not in a Gitee diff buffer"))
    (exhub-gitee-dashboard--open-diff request)
    (message "Gitee diff refreshed")))

;;;###autoload
(defun exhub-gitee-dashboard-diff-browse ()
  "Open the pull request of the current diff buffer in a browser."
  (interactive)
  (if-let ((url (plist-get exhub-gitee-dashboard--diff-request :url)))
      (browse-url url)
    (user-error "No web URL for this pull request")))

(provide 'exhub-gitee-dashboard-diff)

;;; exhub-gitee-dashboard-diff.el ends here