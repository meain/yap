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
  (concat "You are a helpful assistant. "
          "Give concise answers to questions. "
          "Don't be too chatty. "
          "Do not ask or suggest follow up questions. "
          "For code blocks mark it with the language name.")
  "The system prompt to use for the `yap-prompt' command.")

(defconst yap--default-system-prompt-for-rewrite
  (concat "You are a helpful assistant who helps rewrite/refactor code and prose. "
          "Do not ask or suggest follow up questions. "
          "For code responses, ONLY provide the raw code without additional text or markers above or below."
          "NEVER put code block in ``` in the response. "
          "ALWAYS provide the full code to be rewritten and not just the new changes if asked to do code refactor.")
  "The system prompt to use for the `yap-rewrite' command.")

(defun yap--create-messages (system-prompt user-prompt &optional context)
  "Generate Messages to send to the llm.
Use the with the given `SYSTEM-PROMPT', `USER-PROMPT' and `CONTEXT'."
  `((:role "system" :content ,system-prompt)
    ,@(when context
        `((:role "user" :content ,context)
          (:role "assistant" :content "Sure. What would you like me to help with?")))
    (:role "user" :content ,user-prompt)))

(defun yap-template-simple (prompt)
  "Generate a simple template for PROMPT."
  `((:role "system" :content ,yap--default-system-prompt-for-prompt)
    (:role "user" :content ,prompt)))

(defun yap-template-selection-context (system-prompt prompt buffer)
  "Create yap template using `SYSTEM-PROMPT', `PROMPT' and `BUFFER'.
If the buffer has a selection, then the selection is used as context."
  (let* ((selection (yap--get-selected-text buffer))
         (language (yap--get-buffer-language buffer))
         (context (if selection
                      (if language
                          (concat "Use the " language " code below as context for any follow-up tasks:\n\n" selection)
                        (concat  "Use the content below as context for any follow-up tasks:\n\n" selection)))))
    (yap--create-messages system-prompt prompt context)))

(defun yap--get-buffer-language (buffer)
  "Get the programming language associated with the mode of the BUFFER if any."
  (let ((major-mode (with-current-buffer buffer major-mode)))
    (if (provided-mode-derived-p major-mode 'prog-mode)
        (string-trim (string-trim (symbol-name major-mode) nil "-ts-mode") nil "-mode")
      nil)))
(defun yap-template-buffer-context (system-prompt prompt buffer)
  "Similar to `yap-template-selection-context', but with buffer as context.
`SYSTEM-PROMPT', `PROMPT' and `BUFFER' serve the same purpose as the
name suggest.

Order of messages:
- system-prompt
- assistant: give me full content
- user: full content
- assistant: now selection
- user: selection
- assistant: what can I help you with?
- user: {{{prompt}}}"
  (let* ((selection (yap--get-selected-text buffer))
         (language (yap--get-buffer-language buffer))
         (language-text (if language (concat "The code is in " language ". ")))
         (full (yap--get-buffer-text buffer)))
    (if selection
        `((:role "system" :content ,system-prompt)
          (:role "user" :content ,(concat "I'll provide a document in which I have highlighted a section. "
                                          language-text
                                          "Answer should be specific to the highlighted section but use "
                                          "the rest of the text as context to understand the patterns and intent."))
          (:role "assistant" :content "Sure, give me the full document content")
          (:role "user" :content ,full)
          (:role "assistant" :content "OK. What is the highlighted section?")
          (:role "user" :content ,selection)
          (:role "assistant" :content "What can I help you with?")
          (:role "user" :content ,prompt))

      `((:role "system" :content ,system-prompt)
        (:role "user" :content ,(concat "I'll provide you with the document I'm working on and a follow up question. "
                                        "Answer my follow up question using the context provided."))
        (:role "assistant" :content "Sure, give me the full document content")
        (:role "user" :content ,full)
        (:role "assistant" :content "What can I help you with?")
        (:role "user" :content ,prompt)))))

(defun yap-template-split-buffer-context (system-prompt prompt buffer)
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
         (language (yap--get-buffer-language buffer))
         (language-text (if language (concat "The code is in " language ". ")))
         (before (yap--get-text-before buffer))
         (after (yap--get-text-after buffer)))
    (if selection
        `((:role "system" :content ,system-prompt)
          (:role "user" :content ,(concat "I'll provide a document in which I have highlighted a section. "
                                          language-text
                                          "Answer should be specific to the highlighted section but use "
                                          "the rest of the text as context to understand the patterns and intent."))
          (:role "assistant" :content "OK. What is the highlighted text?")
          (:role "user" :content ,selection)
          (:role "assistant" :content "What is there before the highlighted section?")
          (:role "user" :content ,before)
          (:role "assistant" :content "What is there after the highlighted section?")
          (:role "user" :content ,after)
          (:role "assistant" :content "What can I help you with?")
          (:role "user" :content ,prompt))
      `((:role "system" :content ,system-prompt)
        (:role "user" :content ,(concat "I'll provide you context about a document that I am working on. "
                                        "I'm somewhere within the document. Respond with just the request "
                                        "information to be filled in between."))
        (:role "assistant" :content "OK. What comes before your current position?")
        (:role "user" :content ,before)
        (:role "assistant" :content "What comes after your current position?")
        (:role "user" :content ,after)
        (:role "assistant" :content "What can I help you with?")
        (:role "user" :content ,prompt)))))


(defun yap-template-prompt (prompt)
  "A simple prompt template using `PROMPT'."
  (yap-template-selection-context yap--default-system-prompt-for-prompt prompt (current-buffer)))

(defun yap-template-rewrite (prompt)
  "A simple rewrite template using `PROMPT'."
  (yap-template-selection-context yap--default-system-prompt-for-rewrite prompt (current-buffer)))

(defun yap-template-prompt-buffer-context (prompt)
  "A template for `yap-prompt' using `PROMPT' and buffer contents as context."
  (yap-template-buffer-context yap--default-system-prompt-for-prompt prompt (current-buffer)))

(defun yap-template-rewrite-buffer-context (prompt)
  "A template for `yap-rewrite' using `PROMPT' and buffer contents as context."
  (yap-template-buffer-context yap--default-system-prompt-for-rewrite prompt (current-buffer)))

(defun yap-with-prompt (func)
  "Wrap a `FUNC' with a prompt."
  (let ((prompt (read-string "Prompt: " nil 'yap-prompt-history (current-word))))
    (funcall func prompt)))

(defun yap--get-selected-text (buffer)
  "Get the selected text in the specified BUFFER, if any."
  (with-current-buffer buffer
    (when (region-active-p)
      (let ((selection (buffer-substring (region-beginning) (region-end))))
        selection))))

(defun yap--get-buffer-text (buffer)
  "Get the text before the point or selection in the specified BUFFER."
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun yap--get-text-before (buffer)
  "Get the text before the point or selection in the specified BUFFER."
  (with-current-buffer buffer
    (if (region-active-p)
        (buffer-substring-no-properties (point-min) (region-beginning))
      (buffer-substring-no-properties (point-min) (point)))))

(defun yap--get-text-after (buffer)
  "Get the text after the point or selection in the specified BUFFER."
  (with-current-buffer buffer
    (if (region-active-p)
        (buffer-substring-no-properties (region-end) (point-max))
      (buffer-substring-no-properties (point) (point-max)))))

(defun yap-template--string (template)
  "Replace all {{key}} placeholders in TEMPLATE with user input."
  (let ((start 0)
        (result template))
    (while (string-match "{{\\([^}]+\\)}}" result start)
      (let* ((key (match-string 1 result))
             (prompt (format "%s: " key))
             (replacement (read-string prompt)))
        (setq result (string-replace (format "{{%s}}" key) replacement result))
        (setq start (match-end 0))))
    result))

(defun yap--get-filled-template (template)
  "Get the filled `TEMPLATE'."
  (let ((yap-template (alist-get template yap-templates)))
    (if (functionp yap-template)
        (funcall yap-template)
      (if (stringp yap-template)
          (yap-template-simple (yap-template--string yap-template))
        (error "Invalid template type")))))

(provide 'yap-templates-core)
;;; yap-templates-core.el ends here