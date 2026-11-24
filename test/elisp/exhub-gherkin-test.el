;;; exhub-gherkin-test.el --- Tests for the Gherkin preview -*- lexical-binding: t; -*-

;; Copyright (C) 2025 EdmondFrank

;;; Commentary:

;; Run with:
;;
;;   emacs -Q --batch -L . -L test/elisp -l ert -l exhub-gherkin-test \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'exhub-gherkin)

(defconst exhub-gherkin-test--basic
  "# language: en
@smoke @fast
Feature: Login
  The login flow
  for registered users.

  Background:
    Given a user exists

  @happy
  Scenario: Successful login
    Given I am on the login page
    When I enter valid credentials
    Then I see the dashboard
")

(defconst exhub-gherkin-test--outline
  "Feature: Search
  Scenario Outline: Search for <term>
    Given I search for <term>
    Then I see results for <term>

    @data
    Examples: terms
      | term   | count |
      | apple  | 3     |
      | banana | 2     |
")

(defconst exhub-gherkin-test--docstring
  "Feature: Payload
  Scenario: Send a payload
    Given a JSON body
      \"\"\"json
      {\"a\": 1}
      \"\"\"
    And these headers
      | name         | value      |
      | Content-Type | text/plain |
    When the request is sent
      ```
      raw
      ```
    Then it succeeds
")

(defconst exhub-gherkin-test--chinese
  "# language: zh-CN
@冒烟
功能: 登录
  背景:
    假如 用户已注册

  场景: 成功登录
    当 我输入正确的账号密码
    那么 我看到首页
")

(defconst exhub-gherkin-test--escapes
  "Feature: Escapes
  Scenario: Cells
    Given a table
      | a\\|b | c\\nd |
")

(defconst exhub-gherkin-test--rule
  "Feature: Rules
  As a user
  I want rules

  Rule: Logged out
    Users must sign in.

    Background:
      Given a guest

    @smoke
    Scenario: Guarded page
      When I open /admin
      Then I see the login page
")

(ert-deftest exhub-gherkin-test-parse-basic ()
  (let ((feature (exhub-gherkin-parse exhub-gherkin-test--basic "login.feature")))
    (should (equal (plist-get feature :language) "en"))
    (should (eq (plist-get feature :type) 'feature))
    (should (equal (plist-get feature :name) "Login"))
    (should (equal (plist-get feature :tags) '("smoke" "fast")))
    (should (equal (plist-get feature :description)
                   "The login flow\nfor registered users."))
    (let* ((children (plist-get feature :children))
           (background (nth 0 children))
           (scenario (nth 1 children)))
      (should (= (length children) 2))
      (should (eq (plist-get background :type) 'background))
      (should (equal (mapcar (lambda (step) (plist-get step :keyword))
                             (plist-get background :steps))
                     '("Given")))
      (should (eq (plist-get scenario :type) 'scenario))
      (should (eq (plist-get scenario :mode) 'scenario))
      (should (equal (plist-get scenario :tags) '("happy")))
      (should (equal (mapcar (lambda (step) (plist-get step :keyword))
                             (plist-get scenario :steps))
                     '("Given" "When" "Then"))))))

(ert-deftest exhub-gherkin-test-parse-outline ()
  (let* ((feature (exhub-gherkin-parse exhub-gherkin-test--outline "search.feature"))
         (scenario (car (plist-get feature :children)))
         (examples (car (plist-get scenario :examples))))
    (should (eq (plist-get scenario :mode) 'outline))
    (should (equal (plist-get scenario :name) "Search for <term>"))
    (should (equal (plist-get examples :tags) '("data")))
    (should (equal (plist-get examples :name) "terms"))
    (should (equal (plist-get (plist-get examples :table) :header)
                   '("term" "count")))
    (should (equal (plist-get (plist-get examples :table) :rows)
                   '(("apple" "3") ("banana" "2"))))))

(ert-deftest exhub-gherkin-test-parse-docstring-and-table ()
  (let* ((feature (exhub-gherkin-parse exhub-gherkin-test--docstring "payload.feature"))
         (steps (plist-get (car (plist-get feature :children)) :steps))
         (json (nth 0 steps))
         (headers (nth 1 steps))
         (raw (nth 2 steps)))
    (should (equal (plist-get (plist-get json :docstring) :media-type) "json"))
    (should (equal (plist-get (plist-get json :docstring) :content) "{\"a\": 1}"))
    (should (equal (plist-get (plist-get headers :table) :header)
                   '("name" "value")))
    (should (equal (plist-get (plist-get headers :table) :rows)
                   '(("Content-Type" "text/plain"))))
    (should (equal (plist-get (plist-get raw :docstring) :content) "raw"))))

(ert-deftest exhub-gherkin-test-parse-chinese ()
  (let* ((feature (exhub-gherkin-parse exhub-gherkin-test--chinese "login.feature"))
         (children (plist-get feature :children))
         (background (nth 0 children))
         (scenario (nth 1 children)))
    (should (equal (plist-get feature :language) "zh-CN"))
    (should (equal (plist-get feature :name) "登录"))
    (should (equal (plist-get feature :tags) '("冒烟")))
    (should (eq (plist-get background :type) 'background))
    (should (equal (plist-get (car (plist-get background :steps)) :text)
                   "用户已注册"))
    (should (equal (plist-get scenario :keyword) "场景"))
    (should (equal (mapcar (lambda (step) (plist-get step :keyword))
                           (plist-get scenario :steps))
                   '("当" "那么")))))

(ert-deftest exhub-gherkin-test-parse-table-escapes ()
  (let* ((feature (exhub-gherkin-parse exhub-gherkin-test--escapes "escapes.feature"))
         (step (car (plist-get (car (plist-get feature :children)) :steps)))
         (table (plist-get step :table)))
    (should (equal (plist-get table :header) '("a|b" "c\nd")))))

(ert-deftest exhub-gherkin-test-parse-rule ()
  (let* ((feature (exhub-gherkin-parse exhub-gherkin-test--rule "rules.feature"))
         (rule (car (plist-get feature :children)))
         (children (plist-get rule :children)))
    (should (eq (plist-get rule :type) 'rule))
    (should (equal (plist-get rule :name) "Logged out"))
    (should (equal (plist-get rule :description) "Users must sign in."))
    (should (= (length children) 2))
    (should (eq (plist-get (nth 0 children) :type) 'background))
    (should (eq (plist-get (nth 1 children) :type) 'scenario))
    (should (equal (plist-get (nth 1 children) :tags) '("smoke")))))

(ert-deftest exhub-gherkin-test-render-rule ()
  (let* ((feature (exhub-gherkin-parse exhub-gherkin-test--rule "rules.feature"))
         (org (exhub-gherkin-render feature "rules.feature")))
    (should (string-match-p "^\\*\\* Rule: Logged out$" org))
    (should (string-match-p "^  Users must sign in\\.$" org))
    (should (string-match-p "^\\*\\*\\* Background$" org))
    (should (string-match-p "^\\*\\*\\* Scenario: Guarded page  :smoke:$" org))))

(ert-deftest exhub-gherkin-test-parse-garbage ()
  (let* ((feature (exhub-gherkin-parse "just some text\n| not | a table? |\n"
                                      "notes.txt"))
         (org (exhub-gherkin-render feature)))
    (should (equal (plist-get feature :name) "notes"))
    (should (string-match-p "just some text" (plist-get feature :description)))
    (should (stringp org))
    (should (string-match-p "\\* Feature: notes" org))))

(ert-deftest exhub-gherkin-test-render-org ()
  (let* ((feature (exhub-gherkin-parse exhub-gherkin-test--outline "search.feature"))
         (org (exhub-gherkin-render feature "search.feature")))
    (should (string-match-p "^#\\+TITLE: Search$" org))
    (should (string-match-p "^#\\+SUBTITLE: search\\.feature$" org))
    (should (string-match-p "^\\* Feature: Search$" org))
    (should (string-match-p "^\\*\\* Scenario Outline: Search for <term>$" org))
    (should (string-match-p "- \\*Given\\* I search for ~<term>~" org))
    (should (string-match-p "^\\*\\*\\* Examples: terms  :data:$" org))
    (should (string-match-p "^| term | count |$" org))
    (should (string-match-p (regexp-quote "|---+---|") org))))

(ert-deftest exhub-gherkin-test-render-is-valid-org ()
  (let* ((feature (exhub-gherkin-parse exhub-gherkin-test--outline "search.feature"))
         (org (exhub-gherkin-render feature "search.feature")))
    (with-temp-buffer
      (insert org)
      (org-mode)
      (let ((headlines (org-element-map (org-element-parse-buffer)
                         'headline #'identity)))
        (should (= (length headlines) 3))))))

(ert-deftest exhub-gherkin-test-source-line-properties ()
  (let* ((feature (exhub-gherkin-parse exhub-gherkin-test--basic "login.feature"))
         (org (exhub-gherkin-render feature "login.feature"))
         (position (string-match "^\\*\\* Scenario: Successful login" org)))
    (should position)
    (should (equal (get-text-property position 'exhub-gherkin-source-line org) 11))))

(ert-deftest exhub-gherkin-test-preview-buffer ()
  (let ((source (generate-new-buffer " *gherkin-test-source*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert exhub-gherkin-test--basic)
            (exhub-gherkin-mode 1))
          (cl-letf (((symbol-function 'exhub-gherkin--display) #'ignore))
            (let ((preview (exhub-gherkin-preview source)))
              (unwind-protect
                  (with-current-buffer preview
                    (should (derived-mode-p 'exhub-gherkin-preview-mode))
                    (should (buffer-local-value 'buffer-read-only preview))
                    (should (eq (buffer-local-value 'exhub-gherkin--source-buffer
                                                    preview)
                                source))
                    (should (string-match-p "^\\* Feature: Login"
                                            (buffer-string)))
                    (with-current-buffer source
                      (goto-char (point-max))
                      (insert "  Scenario: Extra\n    Then it works\n"))
                    (exhub-gherkin-preview-refresh)
                    (should (string-match-p "Scenario: Extra" (buffer-string))))
                (kill-buffer preview)))))
      (kill-buffer source))))

(ert-deftest exhub-gherkin-test-goto-source ()
  (let ((source (generate-new-buffer " *gherkin-goto-source*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert exhub-gherkin-test--basic))
          (cl-letf (((symbol-function 'exhub-gherkin--display) #'ignore)
                    ((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _) (switch-to-buffer buffer))))
            (let ((preview (exhub-gherkin-preview source)))
              (unwind-protect
                  (with-current-buffer preview
                    (goto-char (point-min))
                    (search-forward "Scenario: Successful login")
                    (exhub-gherkin-goto-source)
                    (should (eq (current-buffer) source))
                    (should (= (line-number-at-pos) 11)))
                (kill-buffer preview)))))
      (kill-buffer source))))

(provide 'exhub-gherkin-test)


;;; exhub-gherkin-test.el ends here
