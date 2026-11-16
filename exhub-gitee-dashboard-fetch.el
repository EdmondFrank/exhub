;;; exhub-gitee-dashboard-fetch.el --- Gitee Enterprise dashboard: fetching -*- lexical-binding: t; -*-

;; Copyright (C) 2025 EdmondFrank

;; Author: EdmondFrank
;; Keywords: tools, gitee, dashboard

;;; Commentary:

;; Fetch the Gitee Enterprise dashboard payload, and the detail of a single
;; pull request or issue, with `envless exec' + curl + jq.  The token is handed
;; to curl on stdin (`curl -K -'), so it never appears in a process argument,
;; a file, a buffer or a log.

;;; Code:

(require 'json)
(require 'subr-x)

(require 'exhub-gitee-dashboard-vars)

(defconst exhub-gitee-dashboard--script
  (mapconcat
   #'identity
   '("#!/bin/sh"
     "set -u"
     "if [ -z \"${GITEE_ENT_MCP_ACCESS_TOKEN:-}\" ]; then"
     "  echo \"GITEE_ENT_MCP_ACCESS_TOKEN is not set (envless unlock failed?)\" >&2"
     "  exit 66"
     "fi"
     "if ! command -v jq >/dev/null 2>&1; then"
     "  echo \"jq is required\" >&2"
     "  exit 69"
     "fi"
     "BASE=\"${GITEE_DASH_BASE:-https://api.gitee.com}/enterprises/${GITEE_DASH_ENTERPRISE_ID:-1}\""
     "TOP=\"${GITEE_DASH_TOP:-10}\""
     "LIMIT=\"${GITEE_DASH_LIMIT:-10}\""
     ""
     "# The token goes to curl on stdin so it never shows up in argv."
     "api() {"
     "  printf 'header = \"User-Agent: mcp-gitee-ent\"\\nheader = \"Authorization: token %s\"\\n' \\"
     "    \"$GITEE_ENT_MCP_ACCESS_TOKEN\" \\"
     "    | curl -sS --max-time 30 -K - \"${BASE}$1\""
     "}"
     ""
     "json() { api \"$1\" | jq -c '.' 2>/dev/null || echo null; }"
     ""
     "jq -n \\"
     "  --argjson enterprise \"$(json '')\" \\"
     "  --argjson overview \"$(json '/dashboard_statistics/overview')\" \\"
     "  --argjson mine \"$(json '/dashboard_statistics/user_dashboard')\" \\"
     "  --argjson committers \"$(json \"/statistics/top_committer?rank=$TOP\")\" \\"
     "  --argjson projects \"$(json '/projects?per_page=1')\" \\"
     "  --argjson programs \"$(json '/programs?per_page=1')\" \\"
     "  --argjson members \"$(json '/members?per_page=1')\" \\"
     "  --argjson pulls \"$(json \"/pull_requests?scope=related_to_me&per_page=$LIMIT\")\" \\"
     "  --argjson issues \"$(json \"/issues?only_related_me=1&state=open&per_page=$LIMIT\")\" \\"
     "  --arg enterprise_id \"${GITEE_DASH_ENTERPRISE_ID:-1}\" \\"
     "  --arg generated_at \"$(date '+%Y-%m-%d %H:%M:%S')\" \\"
     "  '{ enterprise_id: $enterprise_id, generated_at: $generated_at,"
     "     enterprise: ($enterprise | objects),"
     "     overview: ($overview | objects),"
     "     mine: ($mine | objects),"
     "     committers: (if ($committers | type) == \"array\" then $committers else [] end),"
     "     projects: ($projects | objects | .total_count),"
     "     programs: ($programs | objects | .total_count),"
     "     members: ($members | objects | .total_count),"
     "     pulls: (($pulls | objects | .data) // []),"
     "     pulls_total: ($pulls | objects | .total_count),"
     "     issues: (($issues | objects | .data) // []),"
     "     issues_total: ($issues | objects | .total_count) }'")
   "\n")
  "POSIX-sh script that fetches the dashboard payload and prints it as JSON.")

(defconst exhub-gitee-dashboard--detail-script
  (mapconcat
   #'identity
   '("#!/bin/sh"
     "set -u"
     "if [ -z \"${GITEE_ENT_MCP_ACCESS_TOKEN:-}\" ]; then"
     "  echo \"GITEE_ENT_MCP_ACCESS_TOKEN is not set (envless unlock failed?)\" >&2"
     "  exit 66"
     "fi"
     "if ! command -v jq >/dev/null 2>&1; then"
     "  echo \"jq is required\" >&2"
     "  exit 69"
     "fi"
     "BASE=\"${GITEE_DASH_BASE:-https://api.gitee.com}/enterprises/${GITEE_DASH_ENTERPRISE_ID:-1}\""
     "KIND=\"${GITEE_DASH_KIND:-}\""
     "SUBJECT=\"${GITEE_DASH_SUBJECT:-}\""
     "REF=\"${GITEE_DASH_REF:-}\""
     "NOTES=\"${GITEE_DASH_NOTES:-20}\""
     ""
     "# The token goes to curl on stdin so it never shows up in argv."
     "api() {"
     "  printf 'header = \"User-Agent: mcp-gitee-ent\"\\nheader = \"Authorization: token %s\"\\n' \\"
     "    \"$GITEE_ENT_MCP_ACCESS_TOKEN\" \\"
     "    | curl -sS --max-time 30 -K - \"${BASE}$1\""
     "}"
     ""
     "json() { api \"$1\" | jq -c '.' 2>/dev/null || echo null; }"
     ""
     "case \"$KIND\" in"
     "  pull)"
     "    DETAIL_PATH=\"/projects/$SUBJECT/pull_requests/$REF?pr_qt=iid\""
     "    NOTES_PATH=\"/projects/$SUBJECT/pull_requests/$REF/notes/trees?pr_qt=iid&direction=asc\""
     "    LOGS_PATH=\"\""
     "    ;;"
     "  issue)"
     "    DETAIL_PATH=\"/issues/$REF?qt=ident\""
     "    NOTES_PATH=\"/issues/$REF/notes?qt=ident&per_page=$NOTES\""
     "    LOGS_PATH=\"/issues/$REF/operate_logs?qt=ident&limit=$NOTES\""
     "    ;;"
     "  *)"
     "    echo \"unknown GITEE_DASH_KIND: $KIND\" >&2"
     "    exit 64"
     "    ;;"
     "esac"
     ""
     "if [ -n \"$LOGS_PATH\" ]; then"
     "  LOGS_JSON=$(json \"$LOGS_PATH\")"
     "else"
     "  LOGS_JSON='{\"data\":[],\"total_count\":0}'"
     "fi"
     ""
     "jq -n \\"
     "  --argjson detail \"$(json \"$DETAIL_PATH\")\" \\"
     "  --argjson notes \"$(json \"$NOTES_PATH\")\" \\"
     "  --argjson logs \"$LOGS_JSON\" \\"
     "  --arg kind \"$KIND\" \\"
     "  --arg ref \"$REF\" \\"
     "  --arg generated_at \"$(date '+%Y-%m-%d %H:%M:%S')\" \\"
     "  '{ kind: $kind, ref: $ref, generated_at: $generated_at,"
     "     detail: ($detail | objects),"
     "     timeline: (if $kind == \"pull\""
     "                then (($notes | objects | .list) // [])"
     "                else ((($notes | objects | .data) // []) + (($logs | objects | .data) // []))"
     "                end),"
     "     timeline_total: (if $kind == \"pull\""
     "                      then ((($notes | objects | .count) // 0)"
     "                            + (((($notes | objects | .list) // [])"
     "                                | map(select(.type == \"operate_log\")) | length)))"
     "                      else ((($notes | objects | .total_count) // 0) + (($logs | objects | .total_count) // 0))"
     "                      end) }'")
   "\n")
  "POSIX-sh script that fetches one pull request / issue and its timeline.")

(defun exhub-gitee-dashboard--write-script (script)
  "Write the POSIX-sh SCRIPT body to a temporary file and return its name."
  (let ((file (make-temp-file "exhub-gitee-dashboard-" nil ".sh")))
    (with-temp-file file
      (insert script "\n"))
    file))

(defun exhub-gitee-dashboard--run-script (script &optional extra-env)
  "Run SCRIPT with EXTRA-ENV and return its output as a string.
EXTRA-ENV is a list of \"NAME=value\" strings added to the process
environment.  Signals `user-error' with a hint when the script fails."
  (let* ((script-file (exhub-gitee-dashboard--write-script script))
         (envless (executable-find "envless"))
         (token-present (getenv exhub-gitee-dashboard-token-env-var))
         (environment (append
                       (list (format "ENVLESS_ROOT=%s"
                                     (expand-file-name exhub-gitee-dashboard-envless-root))
                             (format "GITEE_DASH_BASE=%s" exhub-gitee-dashboard-api-base)
                             (format "GITEE_DASH_ENTERPRISE_ID=%d"
                                     exhub-gitee-dashboard-enterprise-id)
                             (format "GITEE_DASH_TOP=%d" exhub-gitee-dashboard-top-n)
                             (format "GITEE_DASH_LIMIT=%d" exhub-gitee-dashboard-item-limit))
                       extra-env
                       process-environment))
         (command (cond
                   (token-present (list "/bin/sh" script-file))
                   (envless (list envless "exec" "--" "/bin/sh" script-file))
                   (t (user-error
                       "Cannot fetch: `envless' not found and %s is unset"
                       exhub-gitee-dashboard-token-env-var)))))
    (let ((stderr-file (make-temp-file "exhub-gitee-dashboard-stderr-")))
      (unwind-protect
          (with-temp-buffer
            (let* ((default-directory (expand-file-name "~"))
                   (process-environment environment)
                   (code (apply #'call-process (car command) nil
                                (list (current-buffer) stderr-file) nil
                                (cdr command)))
                   (err (with-temp-buffer
                          (insert-file-contents stderr-file)
                          (buffer-string))))
              (unless (and (integerp code) (zerop code))
                (user-error
                 "Gitee dashboard fetch failed (exit %s)%s"
                 code
                 (let ((trimmed (string-trim err)))
                   (if (string-empty-p trimmed) "" (format ": %s" trimmed)))))
              (string-trim (buffer-string))))
        (delete-file script-file)
        (delete-file stderr-file))))) 

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

(defun exhub-gitee-dashboard--fetch ()
  "Fetch and parse the dashboard payload."
  (exhub-gitee-dashboard--parse
   (exhub-gitee-dashboard--run-script exhub-gitee-dashboard--script)))

(defun exhub-gitee-dashboard--fetch-detail (kind ref &optional subject)
  "Fetch the detail payload of KIND (`pull' or `issue') REF.
SUBJECT is the numeric project id for a pull request and is ignored for
an issue.  Returns an alist with `detail', `timeline' and `timeline_total'.
`timeline' merges the comments with the operation log (event) entries, both
of which are left in their raw API shape."
  (exhub-gitee-dashboard--parse
   (exhub-gitee-dashboard--run-script
    exhub-gitee-dashboard--detail-script
    (list (format "GITEE_DASH_KIND=%s" kind)
          (format "GITEE_DASH_REF=%s" (or ref ""))
          (format "GITEE_DASH_SUBJECT=%s" (or subject ""))
          (format "GITEE_DASH_NOTES=%d" exhub-gitee-dashboard-notes-limit)))))

(provide 'exhub-gitee-dashboard-fetch)

;;; exhub-gitee-dashboard-fetch.el ends here
