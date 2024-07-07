;;; yap-templates.el --- Basic templates for yap -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains templates for yap.  This is just a starting
;; point.  Users are encouraged and expected to create more templates
;; as they see fit.

;;; Code:
(require 'yap-templates-core)

(defun yap--summarize (_ buffer)
  "Summarize the selected text in the specified BUFFER."
  (yap-template-prompt "Summarize the given text. Use bullet points for key ideas." buffer))

(defun yap--explain-code (_ buffer)
  "Explain the code in the specified BUFFER."
  (yap-template-prompt "Explain the code step by step" buffer))

(defun yap-template-prompt-buffer-context (prompt buffer)
  "A template for `yap-prompt' using `PROMPT' and `BUFFER' as context."
  (yap-template-buffer-context yap--default-system-prompt-for-prompt prompt buffer))

(defun yap-template-rewrite-buffer-context (prompt buffer)
  "A template for `yap-rewrite' using `PROMPT' and `BUFFER' as context."
  (yap-template-buffer-context yap--default-system-prompt-for-rewrite prompt buffer))

;; TODO(meain): different set of templates for yap-prompt, yap-rewrite
;; and yap-do so that user won't have the whole set to pick from
;; TODO: Maybe leverage https://github.com/f/awesome-chatgpt-prompts
(defvar yap-templates
  '((default-prompt . yap-template-prompt)
    (default-rewrite . yap-template-rewrite)
    (default-prompt-buffer-context . yap-template-prompt-buffer-context)
    (default-rewrite-buffer-context . yap-template-rewrite-buffer-context)
    (summarize . yap--summarize)
    (explain-code . yap--explain-code)
    (who-what . "What or who is {{prompt}}? Provide a summary and 5 bullet points."))
  "A list of yap templates.")

(provide 'yap-templates)
;;; yap-templates.el ends here