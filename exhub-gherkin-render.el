;;; exhub-gherkin-render.el --- Render a parsed Gherkin feature as Org -*- lexical-binding: t; -*-

;; Copyright (C) 2025 EdmondFrank

;; Author: EdmondFrank
;; Keywords: tools, gherkin, bdd, org

;;; Commentary:

;; Turn the plist tree produced by `exhub-gherkin-parse' into Org markup
;; for the preview buffer in `exhub-gherkin.el'.
;;
;; Feature, Rule and Scenario become headings, Gherkin tags become Org
;; tags, steps become list items with a bold keyword, doc strings become
;; Org example blocks and data tables become Org tables.  Every heading
;; and step line carries an `exhub-gherkin-source-line' text property so
;; the preview can jump back to the `.feature' source.

;;; Code:

(require 'subr-x)

(require 'exhub-gherkin-parse)

(defgroup exhub-gherkin nil
  "Preview Gherkin `.feature' files as Org."
  :group 'exhub
  :prefix "exhub-gherkin-")

(defcustom exhub-gherkin-render-tags t
  "Whether to show Gherkin tags as Org tags on the rendered headings."
  :type 'boolean)

(defcustom exhub-gherkin-render-bold-keywords t
  "Whether to render the Given/When/Then keyword in bold.
When nil the keyword is left plain."
  :type 'boolean)

(defcustom exhub-gherkin-render-step-tables t
  "Whether to render data tables attached to a step.
When nil only the step text is shown."
  :type 'boolean)

(defcustom exhub-gherkin-render-highlight-placeholders t
  "Whether to mark Scenario Outline placeholders such as `<term>' in code.
The placeholder keeps its angle brackets and is only made visually distinct."
  :type 'boolean)

;;;; Helpers

(defun exhub-gherkin-render--tag (tag)
  "Return TAG as a valid Org tag."
  (replace-regexp-in-string "[ \t:]" "_" tag))

(defun exhub-gherkin-render--heading (level text tags line)
  "Return an Org heading of LEVEL with TEXT, TAGS and source LINE."
  (let* ((tag-string (if (and exhub-gherkin-render-tags tags)
                         (concat "  :"
                                 (mapconcat #'exhub-gherkin-render--tag tags ":")
                                 ":")
                       ""))
         (heading (concat (make-string level ?*) " " text tag-string)))
    (propertize heading 'exhub-gherkin-source-line line)))

(defun exhub-gherkin-render--inline (text)
  "Return TEXT with Scenario Outline placeholders made visually distinct."
  (if (and exhub-gherkin-render-highlight-placeholders
           (string-match-p "<[^<>\n]+>" text))
      (replace-regexp-in-string "<\\([^<>\n]+\\)>" "~<\\1>~" text)
    text))

(defun exhub-gherkin-render--description-lines (text)
  "Return the Org lines for the description TEXT.
Lines are indented so that a description line starting with `*', `|' or
`#+' is never mistaken for Org syntax."
  (when (and text (not (string-empty-p text)))
    (mapcar (lambda (line) (concat "  " line))
            (split-string text "\n"))))

(defun exhub-gherkin-render--cell (cell)
  "Return CELL as an Org table cell."
  (let ((cell (or cell "")))
    (setq cell (replace-regexp-in-string "\n" " " cell t t))
    (replace-regexp-in-string "|" "\\vert{}" cell t t)))

(defun exhub-gherkin-render--table-lines (table indent)
  "Return the Org table lines for TABLE, prefixed with INDENT."
  (when (and table (plist-get table :header))
    (let* ((header (plist-get table :header))
           (rows (plist-get table :rows))
           (width (length header))
           (format-row (lambda (cells)
                         (concat indent "| "
                                 (mapconcat #'exhub-gherkin-render--cell cells " | ")
                                 " |")))
           (separator (concat indent "|"
                              (mapconcat (lambda (_cell) "---") header "+")
                              "|")))
      (append
       (list (funcall format-row header) separator)
       (mapcar (lambda (row)
                 (funcall format-row
                          (append row (make-list (max 0 (- width (length row))) ""))))
               rows)))))

(defun exhub-gherkin-render--docstring-lines (docstring)
  "Return the Org lines for the step DOCSTRING."
  (when docstring
    (let ((content (or (plist-get docstring :content) "")))
      (append
       (list "  #+begin_example")
       (mapcar (lambda (line) (concat "  " line))
               (split-string content "\n"))
       (list "  #+end_example")))))

(defun exhub-gherkin-render--step-lines (step)
  "Return the Org lines for STEP."
  (let* ((keyword (or (plist-get step :keyword) "*"))
         (text (exhub-gherkin-render--inline (or (plist-get step :text) "")))
         (label (if exhub-gherkin-render-bold-keywords
                    (format "*%s*" keyword)
                  keyword))
         (heading (string-trim-right
                   (propertize (concat "- " label " " text)
                               'exhub-gherkin-source-line (plist-get step :line)))))
    (append
     (list heading)
     (exhub-gherkin-render--docstring-lines (plist-get step :docstring))
     (when exhub-gherkin-render-step-tables
       (exhub-gherkin-render--table-lines (plist-get step :table) "  ")))))

(defun exhub-gherkin-render--titled (node default-keyword)
  "Return the heading title of NODE, falling back to DEFAULT-KEYWORD."
  (let ((keyword (or (plist-get node :keyword) default-keyword))
        (name (plist-get node :name)))
    (if (and name (not (string-empty-p name)))
        (concat keyword ": " name)
      keyword)))

(defun exhub-gherkin-render--examples-lines (examples level)
  "Return the Org lines for the EXAMPLES block at LEVEL."
  (let ((lines (list ""
                     (exhub-gherkin-render--heading
                      level
                      (exhub-gherkin-render--titled examples "Examples")
                      (plist-get examples :tags)
                      (plist-get examples :line)))))
    (let ((description (exhub-gherkin-render--description-lines
                        (plist-get examples :description))))
      (when description
        (setq lines (append lines (list "") description))))
    (let ((table (exhub-gherkin-render--table-lines
                  (plist-get examples :table) "")))
      (when table
        (setq lines (append lines (list "") table))))
    lines))

(defun exhub-gherkin-render--scenario-lines (scenario level)
  "Return the Org lines for the SCENARIO at LEVEL."
  (let ((lines (list ""
                     (exhub-gherkin-render--heading
                      level
                      (exhub-gherkin-render--titled scenario "Scenario")
                      (plist-get scenario :tags)
                      (plist-get scenario :line)))))
    (let ((description (exhub-gherkin-render--description-lines
                        (plist-get scenario :description))))
      (when description
        (setq lines (append lines (list "") description))))
    (let ((steps (plist-get scenario :steps)))
      (when steps
        (setq lines (append lines (list "")))
        (dolist (step steps)
          (setq lines (append lines (exhub-gherkin-render--step-lines step))))))
    (dolist (examples (plist-get scenario :examples))
      (setq lines (append lines
                          (exhub-gherkin-render--examples-lines examples (1+ level)))))
    lines))

(defun exhub-gherkin-render--background-lines (background level)
  "Return the Org lines for the BACKGROUND block at LEVEL."
  (let ((lines (list ""
                     (exhub-gherkin-render--heading
                      level
                      (exhub-gherkin-render--titled background "Background")
                      (plist-get background :tags)
                      (plist-get background :line)))))
    (let ((description (exhub-gherkin-render--description-lines
                        (plist-get background :description))))
      (when description
        (setq lines (append lines (list "") description))))
    (let ((steps (plist-get background :steps)))
      (when steps
        (setq lines (append lines (list "")))
        (dolist (step steps)
          (setq lines (append lines (exhub-gherkin-render--step-lines step))))))
    lines))

(defun exhub-gherkin-render--child-lines (child level)
  "Return the Org lines for CHILD at LEVEL."
  (pcase (plist-get child :type)
    ('background (exhub-gherkin-render--background-lines child level))
    ('scenario (exhub-gherkin-render--scenario-lines child level))
    ('rule (exhub-gherkin-render--rule-lines child level))
    (_ nil)))

(defun exhub-gherkin-render--rule-lines (rule level)
  "Return the Org lines for the RULE at LEVEL."
  (let ((lines (list ""
                     (exhub-gherkin-render--heading
                      level
                      (exhub-gherkin-render--titled rule "Rule")
                      (plist-get rule :tags)
                      (plist-get rule :line)))))
    (let ((description (exhub-gherkin-render--description-lines
                        (plist-get rule :description))))
      (when description
        (setq lines (append lines (list "") description))))
    (dolist (child (plist-get rule :children))
      (setq lines (append lines (exhub-gherkin-render--child-lines child (1+ level)))))
    lines))

;;;; Public API

(defun exhub-gherkin-render (feature &optional source-path)
  "Render FEATURE, a plist from `exhub-gherkin-parse', as Org text.
SOURCE-PATH, when non-nil, is shown in a `#+SUBTITLE' line."
  (let* ((name (or (plist-get feature :name) ""))
         (keyword (or (plist-get feature :keyword) "Feature"))
         (title (if (string-empty-p name) keyword (concat keyword ": " name)))
         (lines (append
                 (list (format "#+TITLE: %s"
                               (if (string-empty-p name) "Feature" name)))
                 (when source-path
                   (list (format "#+SUBTITLE: %s" source-path)))
                 (list "#+OPTIONS: toc:nil num:nil"
                       "#+STARTUP: showall"
                       ""
                       (exhub-gherkin-render--heading
                        1 title (plist-get feature :tags) (plist-get feature :line))))))
    (let ((description (exhub-gherkin-render--description-lines
                        (plist-get feature :description))))
      (when description
        (setq lines (append lines (list "") description))))
    (dolist (child (plist-get feature :children))
      (setq lines (append lines (exhub-gherkin-render--child-lines child 2))))
    (concat (mapconcat #'identity lines "\n") "\n")))

(provide 'exhub-gherkin-render)

;;; exhub-gherkin-render.el ends here
