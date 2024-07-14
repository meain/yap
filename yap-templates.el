;;; yap-templates.el --- Basic templates for yap -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains templates for yap.  This is just a starting
;; point.  Users are encouraged and expected to create more templates
;; as they see fit.
;;
;; TODO curated prompts additions:
;; - https://github.com/mustvlad/ChatGPT-System-Prompts

;;; Code:
(require 'yap-templates-core)
(require 'yap-utils)

(defun yap-temlpates--summarize (_ buffer)
  "Summarize the selected text in the specified BUFFER."
  (yap-template-prompt "Summarize the given text. Use bullet points for key ideas." buffer))

(defun yap-templates--explain-code (_ buffer)
  "Explain the code in the specified BUFFER."
  (yap-template-prompt "Explain the code step by step" buffer))

(defun yap-templates--optimize-code (_ buffer)
  "Explain the code in the specified BUFFER."
  (yap-template-buffer-context yap--default-system-prompt-for-rewrite "Optimize the provided code" buffer))

(defun yap--retrieve-awesome-chatgpt-prompts (&optional force-update)
  "Retrieve and cache prompts from awesome-chatgpt-prompts.
The data is cached for a day, unless FORCE-UPDATE is non-nil.
Source: https://raw.githubusercontent.com/f/awesome-chatgpt-prompts/main/prompts.csv"
  (let* ((url "https://raw.githubusercontent.com/f/awesome-chatgpt-prompts/main/prompts.csv")
         (cache-file (expand-file-name "awesome-chatgpt-prompts-cache" user-emacs-directory))
         (cache-age (if (file-exists-p cache-file)
                        (float-time (time-subtract (current-time) (nth 5 (file-attributes cache-file))))
                      (1+ (* 24 60 60))))
         (content (if (and (not force-update) (< cache-age (* 24 60 60)))
                      (with-temp-buffer
                        (insert-file-contents cache-file)
                        (buffer-string))
                    (let ((coding-system-for-read 'utf-8)
                          (coding-system-for-write 'utf-8))
                      (with-current-buffer (url-retrieve-synchronously url)
                        (goto-char (point-min))
                        (re-search-forward "\n\n")
                        (let ((data (buffer-substring-no-properties (point) (point-max))))
                          (kill-buffer)
                          (with-temp-file cache-file
                            (insert data))
                          data)))))
         (lines (split-string content "\n" t)))
    (cdr (mapcar
          (lambda (line)
            (let ((columns (yap--parse-csv-line line)))
              (cons (car columns) (cadr columns))))
          lines))))

(defun yap-templates--awesome-chatgpt-prompts (_ buffer)
  "Generate a template using prompt form f/awesome-chatgpt-prompts.
PROMPT is follow up user prompt and BUFFER is used to provide
addition context in case there is user selection present."
  ;; NOTE: Not all of these oens are directly usable, but this is better than nothing.
  (let* ((awesome-chatgpt-prompt (yap--retrieve-awesome-chatgpt-prompts))
         (selected-prompt (completing-read "Select a system-prompt: " awesome-chatgpt-prompt nil t))
         (prompt (read-string "Prompt: "))) ; prompt is asked here as we need to know the system prompt
    (yap-template-selection-context selected-prompt prompt buffer)))

;; TODO(meain): different set of templates for yap-prompt, yap-rewrite
;; and yap-do so that user won't have the whole set to pick from
(defvar yap-templates
  '((default-prompt . ((prompt . t) (function . yap-template-prompt)))
    (default-rewrite . ((prompt . t) (function . yap-template-rewrite)))

    (default-prompt-buffer-context . ((prompt . t) (function . yap-template-prompt-buffer-context)))
    (default-rewrite-buffer-context . ((prompt . t) (function . yap-template-rewrite-buffer-context)))

    ;; Community
    (community:awesome-chatgpt-prompts .  yap-templates--awesome-chatgpt-prompts)

    ;; Never gets prompt
    (summarize . yap-temlpates--summarize)
    (explain-code . yap-templates--explain-code)
    (optimize-code . yap-templates--optimize-code)

    ;; Gets prompt if {{prompt}} in the template
    (joke . "Tell me a joke")
    (who-what . "What or who is {{prompt}}? Provide a summary and 5 bullet points."))
  "A list of yap templates.")

(provide 'yap-templates)
;;; yap-templates.el ends here