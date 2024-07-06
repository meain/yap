;;; yap.el --- A package to do quick interactions with llm -*- lexical-binding: t; -*-

;; URL: https://github.com/meain/yap
;; Keywords: llm, convenience
;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "25.1"))
;; Version: 0.1

;;; Commentary:
;; A thing to help you do stuff with llm.  This tool mostly just
;; provides a way for you to easily call predefined templates and edit
;; buffer using the provided responses.

;;; Code:

(require 'json)
(require 'url)

(require 'yap-templates)

;; TODO(meain): add temperature and other controls
(defvar yap-service "openai"
  "The service to use for the yap command.")
(defvar yap-model "gpt-3.5-turbo"
  "The model to use for the yap command.")
(defvar yap-api-key nil
  "The api key to use for the yap command.")
(defvar yap-system-prompt
  (concat "You are a helpful assistant."
          " Give concise answers to questions."
          " Don't be too chatty."
          " Do not ask,suggest follow up questions."
          " For code blocks mark it with the language name.")
  "The system prompt to use for the yap command.")
(defvar yap-respond-in-buffer nil
  "Whether to respond in a new buffer or the echo area.")
(defvar yap-respond-in-buffer-threshold 100
  "If the response is longer than this, always response in new buffer.")
(defvar yap-show-diff-before-rewrite t
  "Whether to show the diff before rewriting the buffer.")

(defvar yap--response-buffer "*yap-response*")

(defun yap--get-error-message (object)
  "Parse out error message from the `OBJECT' if possible."
  (if (alist-get 'error object)
      (alist-get 'message (alist-get 'error object))
    object))

(defun yap--get-models ()
  "Get the models available for the yap command."
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(format "Bearer %s" yap-api-key))))
         (url-request-data-type 'json)
         (resp (with-current-buffer (url-retrieve-synchronously
                                     "https://api.openai.com/v1/models")
                 (goto-char (point-min))
                 (re-search-forward "^$")
                 (json-read))))
    (if (alist-get 'data resp)
        (alist-get 'data resp)
      (progn
        (message "[ERROR] Unable to get models: %s" (yap--get-error-message resp))
        nil))))

(defun yap-set-model ()
  "Fetch models and update the variable."
  (interactive)
  (if-let* ((models (yap--get-models))
            (model-names (mapcar (lambda (x) (alist-get 'id x)) models))
            (model-name (completing-read "Model: " model-names)))
      (setq yap-model model-name)))

(defun yap--get-llm-response (prompt)
  "Get the response from llm for the given PROMPT."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(format "Bearer %s" yap-api-key))))
         (url-request-data
          (json-encode
           `(("model" . ,yap-model)
             ("messages" . ((("role" . "system")
                             ("content" . ,yap-system-prompt))
                            (("role" . "user")
                             ("content" . ,prompt)))))))
         (url-request-data-type 'json)
         (resp (with-current-buffer (url-retrieve-synchronously
                                     "https://api.openai.com/v1/chat/completions")
                 (goto-char (point-min))
                 (re-search-forward "^$")
                 (json-read))))
    (if (alist-get 'choices resp)
        (alist-get 'content
                   (alist-get 'message
                              (aref (alist-get 'choices resp) 0)))
      (progn
        (message "[ERROR] Unable to get response %s" (yap--get-error-message resp))
        nil))))

(defun yap--present-response (response)
  "Present the RESPONSE in a new buffer or the echo area."
  (if (or yap-respond-in-buffer (> (length response) yap-respond-in-buffer-threshold))
      (progn
        (with-current-buffer (get-buffer-create yap--response-buffer)
          (erase-buffer)
          (insert response)
          (display-buffer (current-buffer))
          ;; Enable markdown mode if available
          (if (fboundp 'markdown-mode)
              (markdown-mode))))
    (message response)))

(defun yap-prompt (prompt &optional template)
  "Prompt the user with the given PROMPT using TEMPLATE if provided.
If TEMPLATE is not provided or nil, use the default template.
If invoked with a universal argument (C-u), prompt for TEMPLATE selection.
The response from LLM is displayed in the *yap-response* buffer."
  (interactive "sPrompt: \nP")
  (let* ((template (if (equal template '(4)) ; Check if C-u (universal argument) is provided
                       (intern (completing-read "Template: " (mapcar 'car yap-templates)))
                     (or template 'default-prompt))) ; Otherwise, use default template if not provided
         (filled-prompt (yap--get-filled-template prompt template (current-buffer))))
    (if filled-prompt
        (let ((response (yap--get-llm-response filled-prompt)))
          (if response
              (yap--present-response response)
            (message "[ERROR] Failed to get a response from LLM")))
      (message "[ERROR] Failed to fill template for prompt: %s" prompt))))

(defun yap--show-diff (before after)
  "Show the diff between BEFORE and AFTER."
  ;; TODO: Use diff package
  (let ((diff (substring-no-properties
               (shell-command-to-string
                (format "diff -u <(echo %s) <(echo %s)"
                        (shell-quote-argument before)
                        (shell-quote-argument after))))))
    (format "%s" diff)))

(defun yap--rewrite-buffer-or-selection (response buffer)
  "Replace the buffer or selection with the given RESPONSE in BUFFER."
  (with-current-buffer buffer
    (if response
        (let* ((to-replace (if (region-active-p)
                               (buffer-substring-no-properties (region-beginning) (region-end))
                             (buffer-string)))
               (diff (yap--show-diff to-replace response)))
          (if (or (not yap-show-diff-before-rewrite)
                  (yes-or-no-p (format "%s\nDo you want to apply the following changes? " diff)))
              (if (region-active-p)
                  (progn
                    (delete-region (region-beginning) (region-end))
                    (insert response "\n"))
                (progn
                  (delete-region (point-min) (point-max))
                  (insert response)))
            (message "No changes made.")))
      (message "[ERROR] Failed to get a response from LLM"))))

(defun yap-rewrite (prompt &optional template)
  "Prompt the user with the given PROMPT using TEMPLATE if provided.
Rewrite the buffer or selection if present with the returned response."
  (interactive "sPrompt: \nP")
  (let* ((buffer (current-buffer))
         (template (if (equal template '(4)) ; Check if C-u (universal argument) is provided
                       (intern (completing-read "Template: " (mapcar 'car yap-templates)))
                     (or template 'default-rewrite))) ; Otherwise, use default template if not provided
         (filled-prompt (yap--get-filled-template prompt template buffer)))
    (if filled-prompt
        (yap--rewrite-buffer-or-selection (yap--get-llm-response filled-prompt) buffer)
      (message "[ERROR] Failed to fill template for prompt: %s" prompt))))

(provide 'yap)
;;; yap.el ends here
