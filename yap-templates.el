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

(defun yap-template-prompt--default ()
  "A default template for `yap-prompt'."
  (yap-with-prompt #'yap-template-prompt))

(defun yap-template-rewrite--default ()
  "A default template for `yap-rewrite'."
  (yap-with-prompt #'yap-template-rewrite))

(defun yap-template-prompt-buffer-context--default ()
  "A default template for `yap-prompt' with buffer context."
  (yap-with-prompt #'yap-template-prompt-buffer-context))

(defun yap-template-rewrite-buffer-context--default ()
  "A default template for `yap-rewrite' with buffer context."
  (yap-with-prompt #'yap-template-rewrite-buffer-context))

(defun yap-temlpates--summarize ()
  "Summarize the selected text."
  (yap-template-prompt "Summarize the given text. Use bullet points for key ideas."))

(defun yap-templates--explain-code ()
  "Explain selected code."
  (yap-template-prompt "Explain the code step by step"))

(defun yap-templates--optimize-code ()
  "Optimize selected code."
  (yap-template-buffer-context yap--default-system-prompt-for-rewrite
                               "Optimize the provided code"
                               (current-buffer)))

(defun yap-templates--emojify ()
  "Emojify the selected text."
  (yap-template-rewrite (concat "Replace words with emojis in the given text. "
                                "Do not give me code to do so. "
                                "Only provide the emojified version with nothing else.")))

(defun yap-templates--who-what ()
  "Generate a template for who/what question."
  (let* ((selection (yap--get-selected-text (current-buffer)))
         (input (if selection
                    selection
                  (read-string "What or who: " (thing-at-point 'word)))))
    (yap-template-prompt (concat "What or who is " input "? Provide a summary and 5 bullet points."))))

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

(defun yap-templates--awesome-chatgpt-prompts ()
  "Generate a template using prompt form f/awesome-chatgpt-prompts.
PROMPT is follow up user prompt."
  ;; NOTE: Not all of these ones are directly usable, but here goes nothing.
  (let* ((awesome-chatgpt-prompt (yap--retrieve-awesome-chatgpt-prompts))
         (selected-prompt (completing-read "Persona: " awesome-chatgpt-prompt nil t))
         (prompt (read-string "Prompt: "))) ; prompt is asked here as we need to know the system prompt
    (yap-template-selection-context selected-prompt prompt (current-buffer))))

;; TODO(meain): different set of templates for yap-prompt, yap-rewrite
;; and yap-do so that user won't have the whole set to pick from
(defvar yap-templates
  '(;; Generic
    (default-prompt . yap-template-prompt--default)
    (default-rewrite . yap-template-rewrite--default)
    (default-prompt-buffer-context . yap-template-prompt-buffer-context--default)
    (default-rewrite-buffer-context . yap-template-rewrite-buffer-context--default)

    ;; Community
    (community:awesome-chatgpt-prompts .  yap-templates--awesome-chatgpt-prompts)

    ;; Biuiltins
    (summarize . yap-temlpates--summarize)
    (explain-code . yap-templates--explain-code)
    (optimize-code . yap-templates--optimize-code)
    (emojify . yap-templates--emojify)
    (who-what . yap-templates--who-what)

    ;; Gets prompt if {{prompt}} in the template
    (joke . "Tell me a joke")
    (difference-between . "What is the difference between {{First}} and {{Second}}?"))
  "A list of yap templates.")

(provide 'yap-templates)
;;; yap-templates.el ends here