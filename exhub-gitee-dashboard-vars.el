;;; exhub-gitee-dashboard-vars.el --- Gitee Enterprise dashboard: configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 EdmondFrank

;; Author: EdmondFrank
;; Keywords: tools, gitee, dashboard

;;; Commentary:

;; User options and buffer-local state shared by the Gitee Enterprise
;; dashboard.  `exhub-gitee-dashboard.el' is the entry point; fetching lives in
;; `exhub-gitee-dashboard-fetch.el', formatting in
;; `exhub-gitee-dashboard-format.el', rendering in
;; `exhub-gitee-dashboard-render.el', the detail buffer in
;; `exhub-gitee-dashboard-detail.el' and its inline images in
;; `exhub-gitee-dashboard-image.el'.

;;; Code:

(defgroup exhub-gitee-dashboard nil
  "Gitee Enterprise org dashboard."
  :group 'exhub)

(defcustom exhub-gitee-dashboard-enterprise-id 1
  "Enterprise id queried by the dashboard.
1 is 开源中国, the account's default enterprise."
  :type 'integer)

(defcustom exhub-gitee-dashboard-top-n 10
  "Number of members shown in the commit ranking."
  :type 'integer)

(defcustom exhub-gitee-dashboard-item-limit 10
  "Maximum number of pull requests and issues listed."
  :type 'integer)

(defcustom exhub-gitee-dashboard-notes-limit 20
  "Maximum number of timeline entries (comments and events) shown in a detail."
  :type 'integer)

(defcustom exhub-gitee-dashboard-backend-url nil
  "Base URL of the ExHub backend serving the dashboard endpoints.
When nil it is derived from `exhub-endpoint' (\"ws://localhost:9069/exhub\"
becomes \"http://localhost:9069\"); the fallback is
\"http://127.0.0.1:9069\"."
  :type '(choice (const :tag "Derive from exhub-endpoint" nil) string))

(defcustom exhub-gitee-dashboard-request-timeout 20
  "Seconds to wait for the ExHub backend before giving up.
Only used by the synchronous fallback; the asynchronous path has no timer."
  :type 'integer)

(defcustom exhub-gitee-dashboard-async t
  "Whether to fetch and render asynchronously.
When non-nil the dashboard never blocks Emacs while the backend fans out to
the Gitee API; opening the buffer, `g' and the auto-refresh timer return at
once.  When nil every fetch is a single synchronous local request."
  :type 'boolean)

(defcustom exhub-gitee-dashboard-auto-refresh-interval nil
  "Seconds between automatic refreshes, or nil to disable auto-refresh."
  :type '(choice (const :tag "Disabled" nil) integer))

(defcustom exhub-gitee-dashboard-use-valign t
  "Whether to enable `valign-mode' in dashboard buffers.
Valign aligns the Org tables pixel-wise, which keeps the Chinese cell
contents lined up.  It has no effect on a non-graphical display or when
the valign package is not installed."
  :type 'boolean)

(defcustom exhub-gitee-dashboard-render-images t
  "Whether to show the images of a detail inline.
Images are downloaded asynchronously into
`exhub-gitee-dashboard-image-cache-dir' and displayed over their links.  A
download that fails leaves the link untouched.  Only a graphical display can
show images."
  :type 'boolean)

(defcustom exhub-gitee-dashboard-image-cache-dir
  (expand-file-name "exhub-gitee-images/" "~/.cache")
  "Directory caching the images downloaded from Gitee."
  :type 'directory)

(defcustom exhub-gitee-dashboard-image-max-width 480
  "Maximum width of an inline image, in pixels.
Wider images are scaled down; nil keeps their natural size."
  :type '(choice (const :tag "Natural size" nil) integer))

(defcustom exhub-gitee-dashboard-image-max-size (* 10 1024 1024)
  "Maximum size of a downloaded image, in bytes, or nil for no limit."
  :type '(choice (const :tag "No limit" nil) integer))

(defcustom exhub-gitee-dashboard-image-timeout 20
  "Seconds after which downloading one image is abandoned."
  :type 'integer)

(defconst exhub-gitee-dashboard-buffer-name "*Gitee Enterprise Dashboard*"
  "Buffer name of the dashboard.")

(defvar-local exhub-gitee-dashboard--timer nil
  "Auto-refresh timer of the current dashboard buffer.")

(defvar-local exhub-gitee-dashboard--generation 0
  "Identifier of the newest in-flight fetch of the current buffer.
A response whose generation no longer matches is discarded, so a slow request
can never overwrite the result of a newer one.")

(provide 'exhub-gitee-dashboard-vars)

;;; exhub-gitee-dashboard-vars.el ends here
