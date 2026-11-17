;;; exhub-gitee-dashboard-fetch.el --- Gitee Enterprise dashboard: fetching -*- lexical-binding: t; -*-

;; Copyright (C) 2025 EdmondFrank

;; Author: EdmondFrank
;; Keywords: tools, gitee, dashboard

;;; Commentary:

;; Retrieval goes through the running ExHub backend: one local request to
;; `GET /gitee/dashboard' or `GET /gitee/detail', which fans the dashboard's
;; Gitee Enterprise calls out concurrently (Exhub.Gitee.Dashboard) and returns the
;; merged payload.  The Enterprise MCP token lives in ExHub's SecretVault, so
;; nothing here touches the envless vault, curl or jq.
;;
;; That replaced a POSIX script that ran nine sequential curl calls with an
;; `envless exec' wrapper and a jq merge per response — about six seconds of
;; Emacs-blocking latency per refresh.  The backend answers in roughly the
;; time of its slowest section (the pull request list), and the asynchronous
;; path keeps even that off the UI thread.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'url-http)
(require 'url-util)

(require 'exhub-gitee-dashboard-vars)

(defun exhub-gitee-dashboard--backend-url ()
  "Return the HTTP base URL of the ExHub backend."
  (or exhub-gitee-dashboard-backend-url
      (let ((endpoint (and (boundp 'exhub-endpoint) (symbol-value 'exhub-endpoint))))
        (if (and (stringp endpoint)
                 (string-match "\\`\\(ws\\)\\(s?\\)://\\([^/]+\\)" endpoint))
            (concat "http" (match-string 2 endpoint) "://" (match-string 3 endpoint))
          "http://127.0.0.1:9069"))))

(defun exhub-gitee-dashboard--url (path params)
  "Return the backend URL for PATH, with PARAMS as its query.
PARAMS is a list of (KEY VALUE) lists, the shape `url-build-query-string'
expects."
  (let ((query (and params (url-build-query-string params))))
    (concat (exhub-gitee-dashboard--backend-url)
            path
            (if (and query (not (string-empty-p query)))
                (concat "?" query)
              ""))))

(defun exhub-gitee-dashboard--json-get (text key)
  "Return KEY from the JSON object TEXT, or nil."
  (condition-case nil
      (let ((data (json-parse-string text
                                     :object-type 'alist
                                     :array-type 'list
                                     :null-object nil
                                     :false-object nil)))
        (when (listp data)
          (cdr (or (assq (intern-soft key) data) (assoc key data)))))
    (error nil)))

(defun exhub-gitee-dashboard--error-text (status body)
  "Return a readable message for HTTP STATUS and BODY from the backend."
  (format "ExHub backend error (HTTP %s)%s"
          (or status "?")
          (let ((message (and (stringp body)
                              (exhub-gitee-dashboard--json-get body "error"))))
            (if (and (stringp message) (not (string-empty-p message)))
                (concat ": " message)
              (let ((text (string-trim (or body ""))))
                (if (string-empty-p text)
                    ""
                  (concat ": " (truncate-string-to-width text 200))))))))

(defun exhub-gitee-dashboard--response-body ()
  "Return the body of the current `url-http' response buffer."
  (save-restriction
    (widen)
    (let ((start (if (and (boundp 'url-http-end-of-headers)
                          url-http-end-of-headers)
                     (min url-http-end-of-headers (point-max))
                   (point-min))))
      (buffer-substring-no-properties start (point-max)))))

(defun exhub-gitee-dashboard--parse (body)
  "Parse BODY (the JSON payload) into alists."
  (condition-case nil
      (let ((data (json-parse-string
                   body
                   :object-type 'alist
                   :array-type 'list
                   :null-object nil
                   :false-object nil)))
        (if (listp data)
            data
          (user-error "Unexpected Gitee response: %s" body)))
    (error
     (user-error "Unexpected Gitee response: %s"
                 (if (> (length body) 200) (concat (substring body 0 200) "…") body)))))

(defun exhub-gitee-dashboard--notify (callback data error)
  "Call CALLBACK with (DATA ERROR), never letting it signal.
A rendering error inside CALLBACK would otherwise escape into the
`url-retrieve' process filter, where it only reaches the echo area as a
cryptic backtrace."
  (condition-case err
      (funcall callback data error)
    (error (message "Gitee dashboard: %s" (error-message-string err)))))

(defun exhub-gitee-dashboard--deliver (callback status body)
  "Hand the parsed BODY of HTTP STATUS to CALLBACK as (DATA ERROR)."
  (if (and (integerp status) (<= 200 status 299))
      (condition-case err
          (exhub-gitee-dashboard--notify callback
                                         (exhub-gitee-dashboard--parse body) nil)
        (error (exhub-gitee-dashboard--notify callback nil (error-message-string err))))
    (exhub-gitee-dashboard--notify callback nil (exhub-gitee-dashboard--error-text status body))))

(defun exhub-gitee-dashboard--request (path params)
  "Fetch PATH with PARAMS from the backend and return the parsed payload.
Signals `user-error' when the backend is unreachable or reports an error."
  (let ((url-request-method "GET")
        (url-request-extra-headers '(("Accept" . "application/json")))
        (url-proxy-services nil)
        (buffer nil))
    (setq buffer (url-retrieve-synchronously
                  (exhub-gitee-dashboard--url path params)
                  t t exhub-gitee-dashboard-request-timeout))
    (unless (buffer-live-p buffer)
      (user-error "ExHub backend unreachable at %s"
                  (exhub-gitee-dashboard--backend-url)))
    (unwind-protect
        (with-current-buffer buffer
          (let ((status (if (bound-and-true-p url-http-response-status)
                            url-http-response-status
                          0))
                (body (exhub-gitee-dashboard--response-body)))
            (if (and (integerp status) (<= 200 status 299))
                (exhub-gitee-dashboard--parse body)
              (user-error "%s" (exhub-gitee-dashboard--error-text status body)))))
      (kill-buffer buffer))))

(defun exhub-gitee-dashboard--request-async (path params callback)
  "Fetch PATH with PARAMS and call CALLBACK with (DATA ERROR).
CALLBACK runs in whatever buffer `url-retrieve' completes in; the temporary
response buffer is killed before it is called."
  (let ((url-request-method "GET")
        (url-request-extra-headers '(("Accept" . "application/json")))
        (url-proxy-services nil))
    (condition-case err
        (url-retrieve
         (exhub-gitee-dashboard--url path params)
         (lambda (status-plist)
           (let ((failure (plist-get status-plist :error))
                 (status (if (bound-and-true-p url-http-response-status)
                             url-http-response-status
                           0))
                 (body (condition-case nil
                           (exhub-gitee-dashboard--response-body)
                         (error ""))))
             (when (buffer-live-p (current-buffer))
               (let ((kill-buffer-query-functions nil))
                 (kill-buffer (current-buffer))))
             ;; `url-http' also reports a 4xx/5xx as a `:error' entry; the
             ;; status and body are still there, so let `--deliver' turn the
             ;; backend's JSON error message into the user-visible one.
             (if (and failure (not (and (integerp status) (> status 0))))
                 (exhub-gitee-dashboard--notify callback nil (error-message-string failure))
               (exhub-gitee-dashboard--deliver callback status body))))
         nil t)
      (error (exhub-gitee-dashboard--notify callback nil (error-message-string err))))))

(defun exhub-gitee-dashboard--dashboard-params ()
  "Return the query parameters of the dashboard request."
  (list (list "enterprise_id" (number-to-string exhub-gitee-dashboard-enterprise-id))
        (list "top" (number-to-string exhub-gitee-dashboard-top-n))
        (list "limit" (number-to-string exhub-gitee-dashboard-item-limit))
        (list "program_limit" (number-to-string exhub-gitee-dashboard-program-limit))
        (list "project_limit" (number-to-string exhub-gitee-dashboard-project-limit))))

(defun exhub-gitee-dashboard--detail-params (kind ref subject)
  "Return the query parameters of a KIND/REF detail request.
SUBJECT is the numeric project id of a pull request and is omitted when nil."
  (append
   (list (list "kind" kind)
         (list "ref" (or ref ""))
         (list "enterprise_id" (number-to-string exhub-gitee-dashboard-enterprise-id))
         (list "notes" (number-to-string exhub-gitee-dashboard-notes-limit)))
   (when (and subject (not (string-empty-p (format "%s" subject))))
     (list (list "subject" (format "%s" subject))))))

(defun exhub-gitee-dashboard--fetch ()
  "Fetch and parse the dashboard payload from the ExHub backend."
  (exhub-gitee-dashboard--request
   "/gitee/dashboard" (exhub-gitee-dashboard--dashboard-params)))

(defun exhub-gitee-dashboard--fetch-async (callback)
  "Fetch the dashboard payload; call CALLBACK with (DATA ERROR)."
  (exhub-gitee-dashboard--request-async
   "/gitee/dashboard" (exhub-gitee-dashboard--dashboard-params) callback))

(defun exhub-gitee-dashboard--fetch-detail (kind ref &optional subject)
  "Fetch the detail payload of KIND (\"pull\" or \"issue\") REF.
SUBJECT is the numeric project id of a pull request and is ignored for an
issue.  Returns an alist with `detail', `timeline' and `timeline_total'."
  (exhub-gitee-dashboard--request
   "/gitee/detail" (exhub-gitee-dashboard--detail-params kind ref subject)))

(defun exhub-gitee-dashboard--program-params (program-id)
  "Return the query parameters of a program (项目集) request.
PROGRAM-ID is the numeric program id."
  (list (list "program_id" (format "%s" program-id))
        (list "enterprise_id" (number-to-string exhub-gitee-dashboard-enterprise-id))
        (list "limit" (number-to-string exhub-gitee-dashboard-project-limit))
        (list "issue_limit"
              (number-to-string exhub-gitee-dashboard-program-issue-limit))))

(defun exhub-gitee-dashboard--fetch-program (program-id)
  "Fetch the PROGRAM-ID payload: the program and its repositories."
  (exhub-gitee-dashboard--request
   "/gitee/program" (exhub-gitee-dashboard--program-params program-id)))

(defun exhub-gitee-dashboard--fetch-program-async (program-id callback)
  "Fetch PROGRAM-ID; call CALLBACK with (DATA ERROR)."
  (exhub-gitee-dashboard--request-async
   "/gitee/program" (exhub-gitee-dashboard--program-params program-id) callback))

(defun exhub-gitee-dashboard--fetch-detail-async (kind ref subject callback)
  "Fetch the detail of KIND/REF; call CALLBACK with (DATA ERROR)."
  (exhub-gitee-dashboard--request-async
   "/gitee/detail" (exhub-gitee-dashboard--detail-params kind ref subject) callback))

(provide 'exhub-gitee-dashboard-fetch)

;;; exhub-gitee-dashboard-fetch.el ends here