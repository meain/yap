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
;;
;; Todo:
;; - Add a way to continue a conversation or refactor (save state and reuse)
;; - Option to retry the last action with a different service/model/template

;;; Code:

(require 'json)
(require 'url)

(require 'yap-templates)
(require 'yap-utils)

(defvar yap-service "openai"
  "The service to use for the yap command.")
(defvar yap-model "gpt-4o-mini"
  "The model to use for the yap command.")
(defvar yap-api-key:openai nil
  "The API key to use with OpenAI models in the yap command.")
(defvar yap-api-key:anthropic nil
  "The API key to use with Anthropic models in the yap command.")
(defvar yap-respond-in-buffer nil
  "Whether to respond in a new buffer or the echo area.")
(defvar yap-respond-in-buffer-threshold 300
  "If the response is longer than this, always respond in a new buffer.")
(defvar yap-show-diff-before-rewrite t
  "Whether to show the diff before rewriting the buffer.")
(defvar yap-log-requests nil
  "Provide a folder to log all the requests and responses.")
(defvar yap-popup-timeout 5
  "The time in seconds to show the popup for.")
(defvar yap-no-popup nil
  "Whether to show the response in a popup or not.
If non-nil, it will use the echo area.")

(defun yap--get-models:openai ()
  "Get a list of OpenAI models available."
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(format "Bearer %s" yap-api-key:openai))))
         (url-request-data-type 'json)
         (resp (with-current-buffer (url-retrieve-synchronously
                                     "https://api.openai.com/v1/models")
                 (goto-char (point-min))
                 (re-search-forward "^$")
                 (json-read))))
    (if (and resp (alist-get 'data resp))
        (mapcar (lambda (x) (alist-get 'id x))
                (alist-get 'data resp))
      (message "[ERROR] Unable to get models: %s"
               (if (not resp)
                   "Response is empty"
                 (yap--get-error-message resp)))
      nil)))

(defun yap--get-models:anthropic ()
  "Return a predefined list of models for Anthropic.
This is a temporary solution until we have a proper API to get models."
  (list "claude-3-5-sonnet-20240620"
        "claude-3-opus-20240229"
        "claude-3-sonnet-20240229"
        "claude-3-haiku-20240307"))

(defun yap-set-service ()
  "Set the service to use for the yap command."
  (interactive)
  (setq yap-service (completing-read "Service: " '("openai" "anthropic")))
  (yap-set-model))

(defun yap-set-model ()
  "Fetch models and update the variable."
  (interactive)
  (if-let* ((models (pcase yap-service
                      ("openai" (yap--get-models:openai))
                      ("anthropic" (yap--get-models:anthropic))))
            (model-name (completing-read "Model: " models)))
      (setq yap-model model-name)))

;; Use `(setq url-debug 1)' to debug things
;; Use `(setq url-debug nil)' to disable debugging
(defun yap--get-llm-response:openai (messages)
  "Get the response from OpenAI LLM for the given set of MESSAGES."
  (let* ((inhibit-message t)
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(format "Bearer %s" yap-api-key:openai))))
         (json-data (json-encode
                     `(("model" . ,yap-model)
                       ("messages" . ,(yap--convert-messages messages)))))
         ;; https://github.com/pythonic-emacs/anaconda-mode/issues/189
         (url-request-data (encode-coding-string json-data 'us-ascii)) ;; TODO: shouldn't this be utf-8
         (url-request-data-type 'json)
         (resp (with-current-buffer (url-retrieve-synchronously
                                     "https://api.openai.com/v1/chat/completions")
                 (set-buffer-multibyte t)   ;; This fixes accented characters and emojis
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

;; TODO: Not tested
(defun yap--get-llm-response:anthropic (messages)
  "Get the response from Anthropic LLM for the given set of MESSAGES."
  (let* ((inhibit-message t)
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(format "x-api-key: %s" yap-api-key:anthropic))))
         (json-data
          (json-encode
           `(("model" . ,yap-model)
             ("system" . ,(yap--system-message messages))
             ("messages" . ,(yap--convert-messages-sans-system messages)))))
         (url-request-data (encode-coding-string json-data 'us-ascii))
         (url-request-data-type 'json)
         (resp (with-current-buffer (url-retrieve-synchronously
                                     "https://api.anthropic.com/v1/messages")
                 (set-buffer-multibyte t)   ;; This fixes accented characters and emojis
                 (goto-char (point-min))
                 (re-search-forward "^$")
                 (json-read))))
    (if (alist-get 'content resp)
        (alist-get 'text (aref (alist-get 'content resp) 0))
      (progn
        (message "[ERROR] Unable to get response %s" (yap--get-error-message resp))
        nil))))

(defun yap--get-llm-response (messages)
  "Get the response from LLM for the given set of MESSAGES."
  (progn
    (message "Processing request via %s and %s model..." yap-service yap-model)
    (let ((response (pcase yap-service
                      ("openai" (yap--get-llm-response:openai messages))
                      ("anthropic" (yap--get-llm-response:anthropic messages))
                      (_ (message "[ERROR] Unsupported service: %s" yap-service) nil))))
      (progn
        (when (and response yap-log-requests)
          (mkdir yap-log-requests t)
          ;; Save logs to disk
          (let ((json-data (json-encode `(("service" . ,yap-service)
                                          ("model" . ,yap-model)
                                          ("messages" . ,messages)
                                          ("response" . ,response))))
                (json-file (format "%s/%s.json" yap-log-requests (format-time-string "%Y%m%d-%H%M%S"))))
            (with-temp-file json-file
              (insert json-data))))
        response))))

(defun yap-prompt (&optional template)
  "Prompt the user with the given PROMPT using TEMPLATE if provided.
If TEMPLATE is not provided or nil, use the default template.
If invoked with a universal argument (C-u), prompt for TEMPLATE selection.
The response from LLM is displayed in the *yap-response* buffer."
  (interactive "P")
  (let* ((template (if (equal template '(4)) ; Check if C-u (universal argument) is provided
                       (intern (completing-read "Template: " (mapcar 'car yap-templates) nil t))
                     (or template 'default-prompt))) ; Otherwise, use default template if not provided
         (llm-messages (yap--get-filled-template template)))
    (if llm-messages
        (let ((response (yap--get-llm-response llm-messages)))
          (if response
              (yap--present-response response)
            (message "[ERROR] Failed to get a response from LLM")))
      (message "[ERROR] Failed to fill template"))))

(defun yap-rewrite (&optional template)
  "Prompt the user with the given PROMPT using TEMPLATE if provided.
Rewrite the buffer or selection if present with the returned response."
  (interactive "P")
  (let* ((template (if (equal template '(4)) ; Check if C-u (universal argument) is provided
                       (intern (completing-read "Template: " (mapcar 'car yap-templates) nil t))
                     (or template 'default-rewrite))) ; Otherwise, use default template if not provided
         (llm-messages (yap--get-filled-template template)))
    (if llm-messages
        (yap--rewrite-buffer-or-selection (yap--get-llm-response llm-messages) (current-buffer))
      (message "[ERROR] Failed to fill template"))))

(defun yap-write (&optional template)
  "Prompt the user with the given PROMPT using TEMPLATE if provided.
Kinda like `yap-rewrite', but just writes instead of replace."
  (interactive "P")
  (let* ((template (if (equal template '(4)) ; Check if C-u (universal argument) is provided
                       (intern (completing-read "Template: " (mapcar 'car yap-templates) nil t))
                     (or template 'default-rewrite))) ; Otherwise, use default template if not provided
         (llm-messages (yap--get-filled-template template)))
    (if llm-messages
        (insert (yap--get-llm-response llm-messages))
      (message "[ERROR] Failed to fill template"))))

(provide 'yap)
;;; yap.el ends here
