;;; exhub-chat.el --- Exhub Chat in Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2023 EdmondFrank

;; Author: Edmond Frank <edmondfrank@hotmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (posframe "0.9"))
;; Keywords: convenience, translation
;; URL: https://github.com/edmondfrank/exhub

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

 ;;; Commentary:

;; This package provides translation functionality for Emacs using Exhub.

 ;;; Code:

(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'markdown-mode)
(require 'exhub)

(defgroup exhub-chat nil
  "Exhub Chat group."
  :group 'applications)

(defcustom exhub-chat-drafts (list)
  "The drafts that Gemini returned."
  :type 'listp
  :group 'exhub-chat)

(defcustom exhub-chat-draft--begin 0
  "Where the draft begins."
  :type 'numberp
  :group 'exhub-chat)

(defcustom exhub-chat-draft--end 0
  "Where the draft ends."
  :type 'numberp
  :group 'exhub-chat)

(defcustom exhub-chat-buffer-name nil
  "The buffer name of the chat buffer.

If nil, it uses the current buffer."
  :type 'string
  :group 'exhub-chat)

(defvar exhub-chat-lang (or (ignore-errors (car (split-string (getenv "LANG") "\\.")))
                            (car (split-string current-language-environment "-"))))

(defun exhub-chat-output-lang ()
  (pcase exhub-chat-lang
    ("zh_CN" "中文")
    ("Chinese" "中文")
    (_ "English")))

(add-to-list 'auto-mode-alist '("\\.exhub-chat$" . markdown-mode))

(defun exhub-chat--get-chat-buffer ()
  "Get the chat buffer."
  (if exhub-chat-buffer-name
      (get-buffer-create exhub-chat-buffer-name)
    (current-buffer)))

(defun exhub-chat-with-message (prompt)
  (let ((buffer (exhub-chat--get-chat-buffer)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (insert "## User:\n")
        (insert (format "%s\n" prompt)))

      (message "[Exhub-Chat] Please wait for Exhub ...")
      (exhub-call "exhub-chat"
                  prompt
                  (or exhub-chat-buffer-name
                      (buffer-name))))))

(defun exhub-chat-finish-answer (buffer)
  (save-excursion
    (switch-to-buffer buffer)
    (goto-char (point-max))
    (insert "\n\n")
    (message "[Exhub-Chat] Exhub finished replying.")))

(defun exhub-chat-response (serial-number content buffer)
  (if (equal serial-number 1)
      (progn
        (setq exhub-chat-drafts (list))
        (push content exhub-chat-drafts)
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-max))
            (insert "\n### Exhub-Chat:\n")
            (setq exhub-chat-draft--begin (point-max))
            (insert content)
            (setq exhub-chat-draft--end (point-max)))))
    (push content exhub-chat-drafts)))

(define-derived-mode exhub-chat-edit-mode text-mode "exhub-chat/edit"
  "The major mode to edit focus text input.")

(setq exhub-chat-edit-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c C-c") #'exhub-chat-edit-mode-confirm)
        (define-key map (kbd "C-c C-k") #'exhub-chat-edit-mode-cancel)
        map))

(defun exhub-chat ()
  (interactive)
  (let ((prompt (read-string "Chat with Exhub: ")))
    (if (string-empty-p (string-trim prompt))
        (message "Please do not enter an empty prompt.")
      (exhub-chat-with-message prompt))))

(defun exhub-chat-with-temp-buffer ()
  "Start a chat session with Exhub in a new temporary buffer."
  (interactive)
  (let* ((prompt (read-string "Chat with Exhub: "))
         (temp-buffer (generate-new-buffer " *exhub-chat-temp-buffer*")))
    (if (string-empty-p (string-trim prompt))
        (message "Please do not enter an empty prompt.")
      (with-current-buffer temp-buffer
        (insert (format "## User:\n%s\n" prompt))
        (exhub-chat-with-message prompt)))))

(defun exhub-chat-with-multiline ()
  (interactive)
  (let* ((bufname (buffer-name))
         (edit-buffer (generate-new-buffer (format "*exhub-chat-edit-buffer-%s*" bufname))))
    (split-window-below -12)
    (other-window 1)
    (with-current-buffer edit-buffer
      (exhub-chat-edit-mode)
      (set (make-local-variable 'exhub-chat-edit-buffer-name) bufname))
    (switch-to-buffer edit-buffer)
    (exhub-chat--edit-set-header-line)))

(defun exhub-chat-with-multiline-with-temp-buffer ()
  "Start a multiline chat session in a new temporary buffer."
  (interactive)
  (let* ((temp-buffer (generate-new-buffer " *exhub-chat-temp-edit-buffer*")))
    (split-window-below -12)
    (other-window 1)
    (with-current-buffer temp-buffer
      (exhub-chat-edit-mode)
      (set (make-local-variable 'exhub-chat-edit-buffer-name) (buffer-name temp-buffer)))
    (switch-to-buffer temp-buffer)
    (exhub-chat--edit-set-header-line)))

(defun exhub-chat--edit-set-header-line ()
  "Set header line."
  (setq header-line-format
        (substitute-command-keys
         (concat
          " Exhub-Chat Edit Mode: "
          "Confirm with  C-c C-c, "
          "Cancel with  C-c C-k. "
          ))))

(defun exhub-chat-get-buffer-string ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun exhub-chat-edit-mode-cancel ()
  (interactive)
  (kill-buffer)
  (delete-window)
  (message "[Exhub-Chat] Edit cancelled!"))

(defun exhub-chat-edit-mode-confirm ()
  (interactive)
  (let* ((bufname exhub-chat-edit-buffer-name)
         (prompt (exhub-chat-get-buffer-string)))
    (kill-buffer)
    (delete-window)

    (switch-to-buffer bufname)
    (exhub-chat-with-message prompt)))

(defun exhub-chat-return-code (serial-number content buffer begin end)
  (when (and begin end)
    (let* ((block-start (or (string-match "```" content)
                            (string-match "`" content)))
           (code-begin (when block-start
                         (+ (string-match "\n" content (+ block-start (if (string= (substring content block-start (+ block-start 3)) "```") 3 1))) 1)))
           (code-end (when code-begin
                       (string-match (if (string= (substring content block-start (+ block-start 3)) "```") "```" "`") content code-begin)))
           (code (when (and code-begin code-end)
                   (substring content code-begin code-end))))

      (if (and code (equal serial-number 1))
          (progn
            (setq exhub-chat-drafts (list))
            (push code exhub-chat-drafts)
            (with-current-buffer buffer
              (delete-region begin end)
              (goto-char begin)
              (setq exhub-chat-draft--begin begin)
              (insert code)
              (setq exhub-chat-draft--end (+ exhub-chat-draft--begin (length code)))))
        (when code
          (push code exhub-chat-drafts))))))

(defun exhub-chat-return-text (serial-number content buffer begin end)
  (when (and begin end)
    (if (equal serial-number 1)
        (progn
          (setq exhub-chat-drafts (list))
          (push content exhub-chat-drafts)
          (with-current-buffer buffer
            (delete-region begin end)
            (goto-char begin)
            (setq exhub-chat-draft--begin begin)
            (insert content)
            (setq exhub-chat-draft--end (+ exhub-chat-draft--begin (length content)))))
      (push content exhub-chat-drafts))))

(defun exhub-chat-generate-code ()
  (interactive)
  (let* ((selection (if (region-active-p)
                        (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))))
         (mode (replace-regexp-in-string "\\(-ts\\)?-mode$" "" (symbol-name major-mode)))
         (prompt (if (= (length selection) 0)
                     (format "%s, please only output the code, without any explanations or instructions." (read-string "Prompt: "))
                   (format "%s, please only output the code, without any explanations or instructions." (concat mode " " selection)))))
    (exhub-call "exhub-chat"
                prompt
                (buffer-name)
                ""
                "Generating..."
                "Generate code done."
                (point)
                (point)
                "exhub-chat-return-code")))

(defun exhub-chat-adjust-code ()
  (interactive)
  (let* ((selection (if (region-active-p)
                        (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))))
         (mode (replace-regexp-in-string "\\(-ts\\)?-mode$" "" (symbol-name major-mode)))
         (prompt (format "%s the %s code below, please only output the code, without any explanations or instructions."
                         (read-string "Adjust: ") mode)))
    (exhub-call "exhub-chat"
                prompt
                (buffer-name)
                selection
                "Adgjusting..."
                "Adjust code done."
                (region-beginning)
                (region-end)
                "exhub-chat-return-code")))

(defun exhub-chat-polish-document ()
  (interactive)
  (let* ((document (if (region-active-p)
                       (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))
                     (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
         (buffer (generate-new-buffer (format "*exhub-chat-doc-buffer*"))))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer buffer)
    (markdown-mode)
    (delete-region (point-min) (point-max))
    (exhub-call "exhub-chat"
                "Please help me proofread and polish the following text:\n"
                (buffer-name)
                document
                "Polishing..."
                "Polish document done.")))

(defun exhub-chat-improve-document ()
  (interactive)
  (let* ((document (if (region-active-p)
                       (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))
                     (string-trim (buffer-substring-no-properties (point-min) (point-max))))))
    (exhub-call "exhub-chat"
                "You are an expert in English writing grammar checking and strengthening. Your main task is to help users improve their English writing by checking for grammatical errors and correcting them directly. Please improve the following documents and fix any grammar or spelling errors. Only return the improved documents, not other extraneous text!\n--- Here are document: \n"
                (buffer-name)
                document
                "Adjusting..."
                "Adjust document done."
                (region-beginning)
                (region-end)
                "exhub-chat-return-text")))

(defun exhub-chat-explain-code ()
  (interactive)
  (let* ((code (if (region-active-p)
                   (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))
                 (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
         (mode (replace-regexp-in-string "\\(-ts\\)?-mode$" "" (symbol-name major-mode)))
         (buffer (generate-new-buffer (format "*exhub-chat-explain-buffer*"))))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer buffer)
    (markdown-mode)
    (delete-region (point-min) (point-max))
    (exhub-call "exhub-chat"
                (format "Please explain in detail the meaning of the following %s code, in %s, leave a blank line between each sentence:\n" mode (exhub-chat-output-lang))
                (buffer-name)
                code
                "Explaining..."
                "Explain code done.")))

(defun exhub-chat-comment-code ()
  (interactive)
  (let* ((code (if (region-active-p)
                   (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))
                 (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
         (begin (if (region-active-p)
                    (region-beginning)
                  (point-min)))
         (end (if (region-active-p)
                  (region-end)
                (point-max)))
         (mode (replace-regexp-in-string "\\(-ts\\)?-mode$" "" (symbol-name major-mode))))
    (exhub-call "exhub-chat"
                (format "Please add code comments to the following %s code, with the comments written in %s within the code, and output the code including the comments." mode (exhub-chat-output-lang))
                (buffer-name)
                code
                "Commenting..."
                "Comment code done."
                begin
                end
                "exhub-chat-return-code")))

(defun exhub-chat-format-code ()
  (interactive)
  (let* ((code (if (region-active-p)
                   (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))
                 (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
         (begin (if (region-active-p)
                    (region-beginning)
                  (point-min)))
         (end (if (region-active-p)
                  (region-end)
                (point-max)))
         (mode (replace-regexp-in-string "\\(-ts\\)?-mode$" "" (symbol-name major-mode))))
    (exhub-call "exhub-chat"
                (format "Please format the following %s code and output the code with pretty format and indent based on the indentation of the first line." mode)
                (buffer-name)
                code
                "Formatting..."
                "Format code done."
                begin
                end
                "exhub-chat-return-code")))

(defun exhub-chat-refactory-code ()
  (interactive)
  (let* ((code (if (region-active-p)
                   (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))
                 (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
         (mode (replace-regexp-in-string "\\(-ts\\)?-mode$" "" (symbol-name major-mode)))
         (buffer (generate-new-buffer (format "*exhub-chat-refactory-buffer*"))))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer buffer)
    (markdown-mode)
    (delete-region (point-min) (point-max))
    (exhub-call "exhub-chat"
                (format "Please help me refactor the following %s code, in %s. Please reply with the refactoring explanation, refactored code, and diff between two versions. Please ignore the comments and strings in the code during the refactoring. If the code remains unchanged after refactoring, please say 'No need to refactor'." mode (exhub-chat-output-lang))
                (buffer-name)
                code
                "Refactorying..."
                "Refactory code done.")))

(defun exhub-chat-generate-commit-message ()
  (interactive)
  (exhub-call "exhub-chat"
              "generate-git-commit-message"
              (file-truename (file-name-directory buffer-file-name))
              (buffer-name)
              (point)
              (point)))

(defun exhub-chat-generate-pull-desc ()
  (interactive)
  (let* ((buffer (generate-new-buffer (format "*exhub-chat-pull-desc-buffer*")))
         (head-branch (read-string "Enter head branch: "))
         (target-branch (read-string "Enter target branch: "))
         (diff-output (shell-command-to-string (format "git diff %s %s" target-branch head-branch)))
         (content (if (region-active-p)
                      (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))
                    (string-trim (buffer-substring-no-properties (point-min) (point-max))))))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer buffer)
    (markdown-mode)
    (delete-region (point-min) (point-max))
    (exhub-call "exhub-chat"
                (format "Please generate a pull request description for the following diff:\n%s" diff-output)
                (buffer-name)
                ""
                "Generating pull request description..."
                "Generate pull request description done.")))

(defun exhub-chat-generate-pull-review ()
  (interactive)
  (let* ((buffer (generate-new-buffer (format "*exhub-chat-pull-review-buffer*")))
         (target-branch (read-string "Enter target branch: "))
         (magit-buffer (magit-merge-preview target-branch))
         (diff-output (with-current-buffer magit-buffer
                        (buffer-substring-no-properties (point-min) (point-max))))
         (content (if (region-active-p)
                      (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))
                    (string-trim (buffer-substring-no-properties (point-min) (point-max))))))
    (kill-buffer magit-buffer)
    (split-window-right)
    (other-window 1)
    (switch-to-buffer buffer)
    (markdown-mode)
    (delete-region (point-min) (point-max))
    (exhub-call "exhub-chat"
                "You are a software developer responsible for conducting code reviews in the Engineering department of a technology/software company. After reviewing a code submission, generate a comprehensive report summarizing the findings. Include information such as identified issues, recommendations for improvement, areas of strength, and overall code quality assessment. The report should be well-structured, easy to understand, and provide actionable feedback to the developer."
                (format "Please generate a code review for the following diff:\n%s" diff-output)
                (buffer-name)
                ""
                "Generating pull request review..."
                "Generate pull request review done.")))

(defun exhub-chat-optimize-prompts ()
  (interactive)
  (let* ((buffer (generate-new-buffer (format "*exhub-chat-prompts-buffer*")))
         (content (if (region-active-p)
                      (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))
                    (string-trim (buffer-substring-no-properties (point-min) (point-max))))))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer buffer)
    (markdown-mode)
    (delete-region (point-min) (point-max))
    (exhub-call "exhub-chat"
                "I want you to become my Prompt engineer. Your goal is to help me craft the best possible prompt for my needs.\nThe prompt will be used by you. You will follow the following process:\n1. You first gather detailed information about my specific requirements and goals.\n2. Based on my input, you will generate 2 sections:\na) Revised prompt (provide your rewritten prompt, it should be clear, concise, and easily understood by you, And the prompt should contain at least three parts: 1) Role, define the role basic description that match my requirements and goals; 2) Skills, define the skills and skill description that need to meet my specific requirements and goals; 3) Constraints, limit the role to focus on my specific requirements and goals, and don't extend it to irrelevant topics).\nb) Questions (ask any relevant questions pertaining to what additional information is needed from me to improve the prompt).\n3. We will continue this iterative process with me providing additional information to you and you updating the prompt in the Revised prompt section until I say we are done\n"
                (format "Write or optimise role-based template prompts for the following requirements:\n```\n%s\n```" content)
                (buffer-name)
                ""
                "Optimizing prompts..."
                "Optimizte prompts done.")))

(defun exhub-chat-translate-into-chinese ()
  (interactive)
  (let* ((buffer (generate-new-buffer (format "*exhub-chat-translate-buffer*")))
         (content (if (region-active-p)
                      (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))
                    (string-trim (buffer-substring-no-properties (point-min) (point-max))))))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer buffer)
    (markdown-mode)
    (delete-region (point-min) (point-max))
    (exhub-call "exhub-chat"
                (format "请把下面的文段翻译成中文:\n%s" content)
                (buffer-name)
                ""
                "Translating..."
                "Translate text done.")))

(defun exhub-chat-translate-into-english ()
  (interactive)
  (let* ((buffer (generate-new-buffer (format "*exhub-chat-translate-buffer*")))
         (content (if (region-active-p)
                      (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))
                    (string-trim (buffer-substring-no-properties (point-min) (point-max))))))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer buffer)
    (markdown-mode)
    (delete-region (point-min) (point-max))
    (exhub-call "exhub-chat"
                (format "Please translate the following passage into English:\n%s" content)
                (buffer-name)
                ""
                "Translating..."
                "Translate text done.")))

(defun exhub-chat-choose-drafts (draft)
  (interactive (list (completing-read "Choose Draft: " exhub-chat-drafts nil t)))
  (delete-region exhub-chat-draft--begin exhub-chat-draft--end)
  (save-excursion
    (goto-char exhub-chat-draft--begin)
    (insert draft))
  (setq exhub-chat-draft--end (+ exhub-chat-draft--begin (length draft))))

(provide 'exhub-chat)
;;; exhub-chat.el ends here
