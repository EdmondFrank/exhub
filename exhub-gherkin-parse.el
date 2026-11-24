;;; exhub-gherkin-parse.el --- Parse Gherkin feature files -*- lexical-binding: t; -*-

;; Copyright (C) 2025 EdmondFrank

;; Author: EdmondFrank
;; Keywords: tools, gherkin, bdd

;;; Commentary:

;; A dependency-free, line based parser for Gherkin `.feature' files.
;;
;; It understands the subset a preview needs: the `# language:' header,
;; comments, tags, Feature / Rule / Background, Scenario and Scenario
;; Outline, Examples, steps (`Given'/`When'/`Then'/`And'/`But'/`*'),
;; doc strings (triple quotes or backticks) and data tables.  English is
;; the default dialect; `zh-CN' is supported as well.  Parsing never
;; signals on malformed input: a line that is not recognised becomes
;; description text (or is skipped), so a preview always renders.
;;
;; The result is a plist tree:
;;
;;   (:language "en" :type feature :keyword "Feature" :name "..."
;;    :line 1 :tags ("smoke")
;;    :description "..."
;;    :children (node...))
;;
;; with child nodes of type `rule', `background', `scenario' or
;; `examples'.  A scenario carries `:mode' (`scenario' or `outline'),
;; `:steps' and `:examples'; a step carries `:keyword', `:text',
;; `:docstring' and `:table'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst exhub-gherkin-parse--dialects
  '(("en"
     (feature "Feature")
     (rule "Rule")
     (background "Background")
     (scenario "Scenario")
     (scenario-outline "Scenario Outline" "Scenario Template")
     (examples "Examples" "Scenarios")
     (given "Given")
     (when "When")
     (then "Then")
     (and "And")
     (but "But"))
    ("zh-CN"
     (feature "功能")
     (rule "规则")
     (background "背景")
     (scenario "场景" "剧本")
     (scenario-outline "场景大纲" "剧本大纲")
     (examples "例子")
     (given "假如" "假设" "假定")
     (when "当")
     (then "那么")
     (and "而且" "并且" "同时")
     (but "但是")))
  "Gherkin keyword dialects known to the parser.")

(defconst exhub-gherkin-parse--step-kinds '(given when then and but)
  "Dialect entries that introduce a step.")

;;;; Line state

(defun exhub-gherkin-parse--lines (source)
  "Split SOURCE into a vector of (LINE-NUMBER . TEXT) conses."
  (let ((number 0))
    (vconcat
     (mapcar (lambda (line)
               (setq number (1+ number))
               (cons number (string-trim-right line)))
             (split-string (replace-regexp-in-string "\r" "" (or source ""))
                           "\n")))))

(defun exhub-gherkin-parse--eobp (state)
  "Return non-nil when STATE is past its last line."
  (>= (cdr state) (length (car state))))

(defun exhub-gherkin-parse--peek (state)
  "Return the (LINE-NUMBER . TEXT) at STATE, or nil at the end."
  (unless (exhub-gherkin-parse--eobp state)
    (aref (car state) (cdr state))))

(defun exhub-gherkin-parse--text (state)
  "Return the raw text at STATE."
  (cdr-safe (exhub-gherkin-parse--peek state)))

(defun exhub-gherkin-parse--line-number (state)
  "Return the line number at STATE."
  (car-safe (exhub-gherkin-parse--peek state)))

(defun exhub-gherkin-parse--next (state)
  "Advance STATE by one line."
  (setcdr state (1+ (cdr state))))

(defun exhub-gherkin-parse--skip-blank-lines (state)
  "Advance STATE past blank lines."
  (while (and (not (exhub-gherkin-parse--eobp state))
              (string-empty-p (string-trim (exhub-gherkin-parse--text state))))
    (exhub-gherkin-parse--next state)))

;;;; Dialect

(defun exhub-gherkin-parse--language (lines)
  "Return the language declared by the `# language:' header in LINES."
  (cl-loop for line in (append lines nil)
           for text = (string-trim (cdr line))
           while (or (string-empty-p text) (string-prefix-p "#" text))
           when (string-match "\\`#[ \t]*language[ \t]*:[ \t]*\\([^ \t]+\\)" text)
           return (match-string 1 text)))

(defun exhub-gherkin-parse--dialect (language)
  "Return the dialect entry for LANGUAGE."
  (let ((normalized (downcase (or language "en"))))
    (or (cl-find-if (lambda (entry)
                      (string-equal (downcase (car entry)) normalized))
                    exhub-gherkin-parse--dialects)
        (assoc "en" exhub-gherkin-parse--dialects))))

(defun exhub-gherkin-parse--keywords (language)
  "Return the keyword alist for LANGUAGE, with English as a fallback."
  (let* ((dialect (exhub-gherkin-parse--dialect language))
         (spec (cdr dialect))
         (english (cdr (assoc "en" exhub-gherkin-parse--dialects))))
    (if (equal (car dialect) "en") spec (append spec english))))

(defun exhub-gherkin-parse--header (text keywords)
  "Return (KIND NAME KEYWORD) when TEXT is a Gherkin header line, else nil."
  (when (string-match "\\`\\([^:：]+\\)[:：][ \t]*\\(.*\\)\\'" text)
    (let ((head (string-trim (match-string 1 text)))
          (name (string-trim (match-string 2 text))))
      (cl-loop for entry in keywords
               when (member head (cdr entry))
               return (list (car entry) name head)))))

(defun exhub-gherkin-parse--keyword-regexp (name)
  "Return a step regexp for keyword NAME.
CJK keywords do not require a separating space, English ones do."
  (if (string-match-p "[^[:ascii:]]" name)
      (concat "\\`" (regexp-quote name) "[ \t]*\\(.*\\)\\'")
    (concat "\\`" (regexp-quote name) "[ \t]+\\(.*\\)\\'")))

(defun exhub-gherkin-parse--step (text keywords)
  "Return (KEYWORD . STEP-TEXT) when TEXT introduces a step."
  (cond
   ((string-match "\\`\\*[ \t]+\\(.+\\)\\'" text)
    (cons "*" (string-trim (match-string 1 text))))
   ((catch 'step
      (dolist (entry keywords)
        (when (memq (car entry) exhub-gherkin-parse--step-kinds)
          (dolist (name (cdr entry))
            (when (string-match (exhub-gherkin-parse--keyword-regexp name) text)
              (throw 'step (cons name (string-trim (match-string 1 text))))))))
      nil))))

(defun exhub-gherkin-parse--tags (text)
  "Return the tags on TEXT, or nil when TEXT is not a tag line."
  (when (string-match "\\`[ \t]*@" text)
    (let ((start 0) tags)
      (while (string-match "@\\([^ \t]+\\)" text start)
        (push (match-string 1 text) tags)
        (setq start (match-end 0)))
      (nreverse tags))))

;;;; Doc strings and tables

(defun exhub-gherkin-parse--docstring-open (text)
  "Return (DELIMITER . MEDIA-TYPE) when TEXT opens a doc string."
  (when (string-match "\\`[ \t]*\\(\\\"\\\"\\\"\\|```\\)[ \t]*\\(.*?\\)[ \t]*\\'" text)
    (cons (match-string 1 text) (match-string 2 text))))

(defun exhub-gherkin-parse--dedent-line (line indent)
  "Remove up to INDENT leading whitespace characters from LINE."
  (let ((leading (if (string-match "\\`[ \t]*" line)
                     (length (match-string 0 line))
                   0)))
    (substring line (min indent leading))))

(defun exhub-gherkin-parse--docstring (state open)
  "Consume the doc string opened by OPEN at STATE."
  (let* ((opening (exhub-gherkin-parse--text state))
         (indent (if (string-match "\\`[ \t]*" opening)
                     (length (match-string 0 opening))
                   0))
         (delimiter (car open))
         (media-type (cdr open))
         (lines nil)
         (done nil))
    (exhub-gherkin-parse--next state)
    (while (and (not (exhub-gherkin-parse--eobp state)) (not done))
      (let ((text (exhub-gherkin-parse--text state)))
        (if (string= (string-trim text) delimiter)
            (setq done t)
          (push (exhub-gherkin-parse--dedent-line text indent) lines))
        (exhub-gherkin-parse--next state)))
    (list :content (mapconcat #'identity (nreverse lines) "\n")
          :media-type (unless (string-empty-p media-type) media-type))))

(defun exhub-gherkin-parse--table-start-p (text)
  "Return non-nil when TEXT starts a data table row."
  (string-match-p "\\`[ \t]*|" text))

(defun exhub-gherkin-parse--split-cells (text)
  "Split a table row TEXT on unescaped `|' characters."
  (let ((start 0) (index 0) (length (length text)) cells)
    (while (< index length)
      (let ((char (aref text index)))
        (cond
         ((and (= char ?\\) (< (1+ index) length))
          (setq index (+ index 2)))
         ((= char ?|)
          (push (substring text start index) cells)
          (setq index (1+ index))
          (setq start index))
         (t
          (setq index (1+ index))))))
    (push (substring text start) cells)
    (nreverse cells)))

(defun exhub-gherkin-parse--unescape-cell (cell)
  "Resolve the Gherkin escapes in CELL (`\\|', `\\n' and `\\\\')."
  (let ((index 0) (length (length cell)) (chars nil))
    (while (< index length)
      (let ((char (aref cell index)))
        (if (and (= char ?\\) (< (1+ index) length))
            (let ((next (aref cell (1+ index))))
              (push (pcase next
                      (?n ?\n)
                      (?| ?|)
                      (?\\ ?\\)
                      (_ next))
                    chars)
              (setq index (+ index 2)))
          (push char chars)
          (setq index (1+ index)))))
    (apply #'string (nreverse chars))))

(defun exhub-gherkin-parse--table-row (text)
  "Parse the cells of the table row TEXT."
  (let* ((text (string-trim text))
         (text (if (string-prefix-p "|" text) (substring text 1) text))
         (text (if (and (string-suffix-p "|" text)
                        (not (and (> (length text) 1)
                                  (= (aref text (- (length text) 2)) ?\\))))
                   (substring text 0 (1- (length text)))
                 text)))
    (mapcar (lambda (cell)
              (exhub-gherkin-parse--unescape-cell (string-trim cell)))
            (exhub-gherkin-parse--split-cells text))))

(defun exhub-gherkin-parse--table (state)
  "Consume the data table at STATE."
  (let (rows)
    (while (and (not (exhub-gherkin-parse--eobp state))
                (exhub-gherkin-parse--table-start-p
                 (exhub-gherkin-parse--text state)))
      (push (exhub-gherkin-parse--table-row (exhub-gherkin-parse--text state)) rows)
      (exhub-gherkin-parse--next state))
    (let ((rows (nreverse rows)))
      (when rows
        (list :header (car rows) :rows (cdr rows))))))

;;;; Nodes

(defun exhub-gherkin-parse--clean-description (lines)
  "Trim blank edges from LINES and join them into a string."
  (let ((lines (copy-sequence lines)))
    (while (and lines (string-empty-p (car lines)))
      (pop lines))
    (setq lines (nreverse lines))
    (while (and lines (string-empty-p (car lines)))
      (pop lines))
    (setq lines (nreverse lines))
    (when lines (mapconcat #'identity lines "\n"))))

(defun exhub-gherkin-parse--step-node (state step)
  "Consume STEP at STATE together with its doc string and table."
  (let ((line (exhub-gherkin-parse--line-number state))
        (docstring nil)
        (table nil))
    (exhub-gherkin-parse--next state)
    (exhub-gherkin-parse--skip-blank-lines state)
    (let ((open (and (not (exhub-gherkin-parse--eobp state))
                     (exhub-gherkin-parse--docstring-open
                      (exhub-gherkin-parse--text state)))))
      (when open
        (setq docstring (exhub-gherkin-parse--docstring state open))
        (exhub-gherkin-parse--skip-blank-lines state)))
    (when (and (not (exhub-gherkin-parse--eobp state))
               (exhub-gherkin-parse--table-start-p
                (exhub-gherkin-parse--text state)))
      (setq table (exhub-gherkin-parse--table state)))
    (list :keyword (car step) :text (cdr step) :line line
          :docstring docstring :table table)))

(defun exhub-gherkin-parse--parse-steps (state keywords)
  "Consume description lines and steps until a structural line.
Return (DESCRIPTION . STEPS)."
  (let ((desc-lines nil) (steps nil) (done nil))
    (while (and (not done) (not (exhub-gherkin-parse--eobp state)))
      (let* ((text (string-trim (exhub-gherkin-parse--text state)))
             (step (exhub-gherkin-parse--step text keywords))
             (header (exhub-gherkin-parse--header text keywords)))
        (cond
         ((string-empty-p text)
          (when desc-lines (push "" desc-lines))
          (exhub-gherkin-parse--next state))
         ((string-prefix-p "#" text)
          (exhub-gherkin-parse--next state))
         ((exhub-gherkin-parse--tags text)
          (setq done t))
         (step
          (push (exhub-gherkin-parse--step-node state step) steps))
         (header
          (setq done t))
         (t
          (when (null steps) (push text desc-lines))
          (exhub-gherkin-parse--next state)))))
    (cons (exhub-gherkin-parse--clean-description (nreverse desc-lines))
          (nreverse steps))))

(defun exhub-gherkin-parse--examples (state tags header)
  "Consume the Examples block at STATE."
  (let ((line (exhub-gherkin-parse--line-number state))
        (desc-lines nil)
        (table nil)
        (done nil))
    (exhub-gherkin-parse--next state)
    (while (and (not done) (not (exhub-gherkin-parse--eobp state)))
      (let ((text (string-trim (exhub-gherkin-parse--text state))))
        (cond
         ((string-empty-p text)
          (when desc-lines (push "" desc-lines))
          (exhub-gherkin-parse--next state))
         ((string-prefix-p "#" text)
          (exhub-gherkin-parse--next state))
         ((exhub-gherkin-parse--table-start-p text)
          (setq table (exhub-gherkin-parse--table state))
          (setq done t))
         (t
          (push text desc-lines)
          (exhub-gherkin-parse--next state)))))
    (list :type 'examples
          :keyword (caddr header)
          :name (cadr header)
          :line line
          :tags tags
          :description (exhub-gherkin-parse--clean-description (nreverse desc-lines))
          :table table)))

(defun exhub-gherkin-parse--scenario (state keywords tags header)
  "Consume the scenario at STATE."
  (let ((line (exhub-gherkin-parse--line-number state))
        (body nil)
        (examples nil)
        (pending-tags nil)
        (done nil))
    (exhub-gherkin-parse--next state)
    (setq body (exhub-gherkin-parse--parse-steps state keywords))
    (while (and (not done) (not (exhub-gherkin-parse--eobp state)))
      (let* ((text (string-trim (exhub-gherkin-parse--text state)))
             (line-tags (exhub-gherkin-parse--tags text))
             (child-header (exhub-gherkin-parse--header text keywords)))
        (cond
         ((string-empty-p text)
          (exhub-gherkin-parse--next state))
         ((string-prefix-p "#" text)
          (exhub-gherkin-parse--next state))
         (line-tags
          (setq pending-tags (append pending-tags line-tags))
          (exhub-gherkin-parse--next state))
         ((eq (car child-header) 'examples)
          (push (exhub-gherkin-parse--examples state pending-tags child-header)
                examples)
          (setq pending-tags nil))
         (t
          (setq done t)))))
    (list :type 'scenario
          :mode (if (eq (car header) 'scenario-outline) 'outline 'scenario)
          :keyword (caddr header)
          :name (cadr header)
          :line line
          :tags tags
          :description (car body)
          :steps (cdr body)
          :examples (nreverse examples))))

(defun exhub-gherkin-parse--background (state keywords header)
  "Consume the Background block at STATE."
  (let ((line (exhub-gherkin-parse--line-number state))
        (body nil))
    (exhub-gherkin-parse--next state)
    (setq body (exhub-gherkin-parse--parse-steps state keywords))
    (list :type 'background
          :keyword (caddr header)
          :name (cadr header)
          :line line
          :tags nil
          :description (car body)
          :steps (cdr body))))

(defun exhub-gherkin-parse--rule (state keywords tags header)
  "Consume the Rule block at STATE."
  (let ((line (exhub-gherkin-parse--line-number state))
        (desc-lines nil)
        (children nil)
        (pending-tags nil)
        (done nil))
    (exhub-gherkin-parse--next state)
    (while (and (not done) (not (exhub-gherkin-parse--eobp state)))
      (let* ((text (string-trim (exhub-gherkin-parse--text state)))
             (line-tags (exhub-gherkin-parse--tags text))
             (child-header (exhub-gherkin-parse--header text keywords)))
        (cond
         ((string-empty-p text)
          (when desc-lines (push "" desc-lines))
          (exhub-gherkin-parse--next state))
         ((string-prefix-p "#" text)
          (exhub-gherkin-parse--next state))
         (line-tags
          (setq pending-tags (append pending-tags line-tags))
          (exhub-gherkin-parse--next state))
         ((eq (car child-header) 'background)
          (push (exhub-gherkin-parse--background state keywords child-header)
                children)
          (setq pending-tags nil))
         ((memq (car child-header) '(scenario scenario-outline))
          (push (exhub-gherkin-parse--scenario state keywords pending-tags child-header)
                children)
          (setq pending-tags nil))
         ((null children)
          (push text desc-lines)
          (exhub-gherkin-parse--next state))
         (t
          (setq done t)))))
    (list :type 'rule
          :keyword (caddr header)
          :name (cadr header)
          :line line
          :tags tags
          :description (exhub-gherkin-parse--clean-description (nreverse desc-lines))
          :children (nreverse children))))

(defun exhub-gherkin-parse--feature-body (state keywords)
  "Consume the feature body at STATE.
Return (DESCRIPTION . CHILDREN)."
  (let ((desc-lines nil) (children nil) (pending-tags nil))
    (while (not (exhub-gherkin-parse--eobp state))
      (let* ((text (string-trim (exhub-gherkin-parse--text state)))
             (line-tags (exhub-gherkin-parse--tags text))
             (header (exhub-gherkin-parse--header text keywords)))
        (cond
         ((string-empty-p text)
          (when desc-lines (push "" desc-lines))
          (exhub-gherkin-parse--next state))
         ((string-prefix-p "#" text)
          (exhub-gherkin-parse--next state))
         (line-tags
          (setq pending-tags (append pending-tags line-tags))
          (exhub-gherkin-parse--next state))
         ((eq (car header) 'rule)
          (push (exhub-gherkin-parse--rule state keywords pending-tags header)
                children)
          (setq pending-tags nil))
         ((eq (car header) 'background)
          (push (exhub-gherkin-parse--background state keywords header)
                children)
          (setq pending-tags nil))
         ((memq (car header) '(scenario scenario-outline))
          (push (exhub-gherkin-parse--scenario state keywords pending-tags header)
                children)
          (setq pending-tags nil))
         ((null children)
          (push text desc-lines)
          (exhub-gherkin-parse--next state))
         (t
          (exhub-gherkin-parse--next state)))))
    (cons (exhub-gherkin-parse--clean-description (nreverse desc-lines))
          (nreverse children))))

(defun exhub-gherkin-parse--feature (state keywords language source-name)
  "Parse the feature at STATE."
  (let ((tags nil) (header nil) (header-line nil) (done nil))
    (while (and (not done) (not (exhub-gherkin-parse--eobp state)))
      (let* ((text (string-trim (exhub-gherkin-parse--text state)))
             (line-tags (exhub-gherkin-parse--tags text))
             (found (exhub-gherkin-parse--header text keywords)))
        (cond
         ((string-empty-p text)
          (exhub-gherkin-parse--next state))
         ((string-prefix-p "#" text)
          (exhub-gherkin-parse--next state))
         (line-tags
          (setq tags (append tags line-tags))
          (exhub-gherkin-parse--next state))
         ((eq (car found) 'feature)
          (setq header found)
          (setq header-line (exhub-gherkin-parse--line-number state))
          (exhub-gherkin-parse--next state)
          (setq done t))
         (t
          (setq done t)))))
    (let ((body (exhub-gherkin-parse--feature-body state keywords)))
      (list :language language
            :type 'feature
            :keyword (or (caddr header) "Feature")
            :name (cond (header (cadr header))
                        (source-name (file-name-base source-name))
                        (t "Feature"))
            :line (or header-line 1)
            :tags tags
            :description (car body)
            :children (cdr body)))))

;;; Public API

(defun exhub-gherkin-parse (source &optional source-name)
  "Parse SOURCE, the text of a Gherkin `.feature' file, into a plist tree.
SOURCE-NAME is used for the feature name when SOURCE has no `Feature:'
line.  The returned plist is described in this file's Commentary."
  (let* ((lines (exhub-gherkin-parse--lines source))
         (language (or (exhub-gherkin-parse--language lines) "en"))
         (keywords (exhub-gherkin-parse--keywords language))
         (state (cons lines 0)))
    (exhub-gherkin-parse--feature state keywords language source-name)))

(provide 'exhub-gherkin-parse)

;;; exhub-gherkin-parse.el ends here
