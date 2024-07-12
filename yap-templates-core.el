;;; yap-templates-core.el --- Tempting engine for yap -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains core utils to create yap templates. A yap
;; template is a function that gets passed in a prompt, section and a
;; buffer returns the text to be sent to an llm for response.
;;
;; Notes:
;; - Multiple messages work better when compared to as single message in
;;   which we include all the context.
;; - For questions with full buffer context smaller/older models like
;;   gpt-3.5 don't work that well.
;;
;; Todos:
;; - Maybe an option to override model in the template
;; - Separate set of prompts for yap-prompt, yap-rewrite and yap-do
;; - Maybe use strings as keys for yap-templates instead of symbols
;; - Templates should be able to set temperature, freq_penalty,
;;   max_tokens etc. (How will this translate to different services?)

;;; Code:

(defconst yap--default-system-prompt-for-prompt
  (concat "You are a helpful assistant."
          " Give concise answers to questions."
          " Don't be too chatty."
          " Do not ask,suggest follow up questions."
          " For code blocks mark it with the language name.")
  "The system prompt to use for the `yap-prompt' command.")

(defconst yap--default-system-prompt-for-rewrite
  (concat "You are a helpful assistant who helps rewrite/refactor code and prose. "
          "For code responses, just provide the raw code snippet without additional text or markers above or below"
          "as in do not add ``` in the response. "
          " Provide the full code to be rewritten and not just the new changes.")
  "The system prompt to use for the `yap-rewrite' command.")

(defun yap--create-messages (system-prompt user-prompt &optional context)
  "Generate Messages to send to the llm.
Use the with the given `SYSTEM-PROMPT', `USER-PROMPT' and `CONTEXT'."
  `(("system" . ,system-prompt)
    ,@(when context
        `(("user" . ,context)
          ("assistant" . "Sure. What would you like me to help with?")))
    ("user" . ,user-prompt)))

(defun yap-template-simple (prompt)
  "Generate a simple template for PROMPT."
  (yap--create-messages yap--default-system-prompt-for-prompt prompt))

(defun yap-template-selection-context (system-prompt prompt buffer)
  "Create yap template using `SYSTEM-PROMPT', `PROMPT' and `BUFFER'.
If the buffer has a selection, then the selection is used as context."
  (let* ((selection (yap--get-selected-text buffer))
         (context (if selection
                      (concat  "Use the content below as context for any follow-up tasks:\n\n" selection))))
    (yap--create-messages system-prompt prompt context)))

(defun yap-template-buffer-context (system-prompt prompt buffer)
  "Similar to `yap-template-selection-context', but with buffer as context.
`SYSTEM-PROMPT', `PROMPT' and `BUFFER' serve the same purpose as the
name suggest.

Order of messages:
- system-prompt
- user: content before cursor (if selection, then before selection)
- assistant: ok
- user: content after cursor (or selection)
- assistant: ok
- user(if selection): the selected text
- assistant: what can I help with?
- user: {{{prompt}}}"
  (let* ((selection (yap--get-selected-text buffer))
         (before (if selection
                     (buffer-substring-no-properties (point-min) (region-beginning))
                   (buffer-substring-no-properties (point-min) (point))))
         (after (if selection
                    (buffer-substring-no-properties (region-end) (point-max))
                  (buffer-substring-no-properties (point) (point-max)))))
    (if selection
        `(("system" . ,system-prompt)
          ("user" . ,(concat "I'll provide a document in which I have highlighted a section. "
                             "Answer for the highlighted section but use the rest of the text as context."))
          ("assistant" . "OK. What is the highlighted text?")
          ("user" . ,(concat "This is the text in the document that is highlighted:\n\n" selection))
          ("assistant" . "What is there before the highlighted section?")
          ("user" . ,(concat "Here is the text before: \n\n" before))
          ("assistant" . "What is there after the highlighted section?")
          ("user" . ,(concat "Here is the text after: \n\n" after))
          ("assistant" . "What can I help you with?")
          ("user" . ,prompt))
      `(("system" . ,system-prompt)
        ("user" . "I'll provide you context about a document that I am working on. I'm somewhere within the document.")
        ("assistant" . "OK. What comes before your current position?")
        ("user". ,(concat "Here is the text before the cursor:\n\n" before))
        ("assistant" . "What comes after your current position?")
        ("user" . ,(concat "Here is the text after the cursor:\n\n" after))
        ("assistant" . "What can I help you with?")
        ("user" . ,prompt)))))

(defun yap-template-prompt (prompt buffer)
  "A simple prompt template using `PROMPT' and selection in `BUFFER'."
  (yap-template-selection-context yap--default-system-prompt-for-prompt prompt buffer))

(defun yap-template-rewrite (prompt buffer)
  "A simple rewrite template using `PROMPT' and selection in `BUFFER'."
  (yap-template-selection-context yap--default-system-prompt-for-rewrite prompt buffer))

(defun yap-template-prompt-buffer-context (prompt buffer)
  "A template for `yap-prompt' using `PROMPT' and `BUFFER' as context."
  (yap-template-buffer-context yap--default-system-prompt-for-prompt prompt buffer))

(defun yap-template-rewrite-buffer-context (prompt buffer)
  "A template for `yap-rewrite' using `PROMPT' and `BUFFER' as context."
  (yap-template-buffer-context yap--default-system-prompt-for-rewrite prompt buffer))

(defun yap--get-selected-text (buffer)
  "Get the selected text in the specified BUFFER, if any."
  (interactive)
  (with-current-buffer buffer
    (if (region-active-p)
        (let ((selection (buffer-substring (region-beginning) (region-end))))
          selection)
      nil)))

(defun yap--get-filled-template (prompt template buffer)
  "Get the filled `TEMPLATE' for the provided `PROMPT'."
  (let ((yap-template (alist-get template yap-templates)))
    (if (functionp yap-template)
        (funcall yap-template prompt buffer)
      (if (stringp yap-template)
          (yap-template-simple (string-replace "{{prompt}}" prompt yap-template))
        (funcall (alist-get 'function yap-template) prompt buffer)))))

(provide 'yap-templates-core)
;;; yap-templates-core.el ends here