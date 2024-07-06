;;; yap-templates.el --- Basic templates for yap -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains basic templates for yap.  A yap template is a
;; function that gets passed in a prompt, section and a buffer returns
;; the text to be sent to an llm for response.

;;; Code:

;; TODO(meain): use defcustom
(defvar yap-default-system-prompt-for-prompt
  (concat "You are a helpful assistant."
          " Give concise answers to questions."
          " Don't be too chatty."
          " Do not ask,suggest follow up questions."
          " For code blocks mark it with the language name.")
  "The system prompt to use for the `yap-prompt' command.")

(defvar yap-default-system-prompt-for-rewrite
  (concat "You are a helpful assistant who helps rewrite/refactor code and prose."
          "For code responses, just provide the raw code snippet without additional text or markers above or below"
          "as in do not add ``` in the response.")
  "The system prompt to use for the `yap-rewrite' command.")

(defun yap--create-messages (system-prompt user-prompt &optional context)
  "Generate Messages to send to the llm.
Use the with the given `SYSTEM-PROMPT', `USER-PROMPT' and `CONTEXT'."
  `(("system" . ,system-prompt)
    ,@(when context
        `(("user" . ,context)
          ("assistant" . "Sure. What would you like me to help with?")))
    ("user" . ,user-prompt)))

(defun yap--template-simple (system-prompt prompt buffer)
  "Create yap template using `SYSTEM-PROMPT', `PROMPT' and `BUFFER'."
  (let* ((selection (yap--get-selected-text buffer))
         (context (if selection
                      (concat  "Use the content below as context for any follow-up tasks:\n\n" selection))))
    (yap--create-messages system-prompt prompt context)))

(defun yap-template-prompt (prompt buffer)
  "A simple prompt template using `PROMPT' and selection in `BUFFER'."
  (yap--template-simple yap-default-system-prompt-for-prompt prompt buffer))

(defun yap-template-rewrite (prompt buffer)
  "A simple rewrite template using `PROMPT' and selection in `BUFFER'."
  (yap--template-simple yap-default-system-prompt-for-rewrite prompt buffer))

(defun yap-template-simple (prompt)
  "Generate a simple template for PROMPT."
  (yap--create-messages yap-default-system-prompt-for-prompt prompt))

;; TODO: Make this work with full buffer if no selection
(defun yap--template-do (prompt buffer)
  "Create a yap template for the `do' command using `PROMPT' and `BUFFER'."
  (if-let* ((selection (yap--get-selected-text buffer))
            (context (if selection
                         (concat  "Use the content below as context for any follow-up tasks:\n\n" selection))))
      (yap--create-messages yap-default-system-prompt-for-prompt prompt context)))

(defun yap--summarize (_ buffer)
  "Summarize the selected text in the specified BUFFER."
  (yap--template-do "Summarize the selected text." buffer))

(defun yap--explain-code (_ buffer)
  "Explain the code in the specified BUFFER."
  (yap--template-do "Explain the code step by step" buffer))

;; TODO(meain): different set of templates for yap-prompt, yap-rewrite
;; and yap-do so that user won't have the whole set to pick from
(defvar yap-templates
  '((default-prompt . yap-template-prompt)
    (default-rewrite . yap-template-rewrite)
    (summarize . yap--summarize)
    (explain-code . yap--explain-code)
    (what . "What or who is {{prompt}}? Provide a summary and 5 bullet points."))
  "A list of yap templates.")

(defun yap--get-selected-text (buffer)
  "Get the selected text in the specified BUFFER, if any."
  (interactive)
  (with-current-buffer buffer
    (if (region-active-p)
        (let ((selection (buffer-substring (region-beginning) (region-end))))
          (message "Selected text: %s" selection)
          selection)
      nil)))

(defun yap--get-filled-template (prompt template buffer)
  "Get the filled `TEMPLATE' for the provided `PROMPT'."
  (let ((yap-template (alist-get template yap-templates)))
    (if (functionp yap-template)
        (funcall yap-template prompt buffer)
      (yap-template-simple (string-replace "{{prompt}}" prompt yap-template)))))

(provide 'yap-templates)
;;; yap-templates.el ends here