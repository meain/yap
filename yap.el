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

(defvar yap--response-buffer "*yap-response*")

(defun yap--get-error-message (object)
  "Parse out error message from the OBJECT if possible."
  (if (alist-get 'error object)
      (alist-get 'message (alist-get 'error object))
    object))

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
    (if (alist-get 'data resp)
        (mapcar (lambda (x) (alist-get 'id x))
                (alist-get 'data resp))
      (progn
        (message "[ERROR] Unable to get models: %s" (yap--get-error-message resp))
        nil))))

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

(defun yap--convert-alist (alist)
  "Convert ALIST from (role . content) to ((\"role\" . role) (\"content\" . content))."
  (mapcar (lambda (pair)
            (let ((role (car pair))
                  (content (cdr pair)))
              `(("role" . ,role) ("content" . ,content))))
          alist))

(defun yap--convert-alist-sans-system (alist)
  "Convert ALIST from (role . content) to ((\"role\" . role) (\"content\" . content)) without system."
  (mapcar (lambda (pair)
            (let ((role (car pair))
                  (content (cdr pair)))
              (if (not (string= role "system"))
                  `(("role" . ,role) ("content" . ,content))
                nil)))
          alist))

(defun yap--system-message (messages)
  "Check if the given MESSAGES contain a system message."
  (let ((system-message (seq-find (lambda (pair)
                                    (string= (car pair) "system"))
                                  messages)))
    (if system-message
        (cdr system-message)
      nil)))

;; Use `(setq url-debug 1)' to debug things
(defun yap--get-llm-response:openai (messages)
  "Get the response from OpenAI LLM for the given set of MESSAGES."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(format "Bearer %s" yap-api-key:openai))))
         (url-request-data
          (json-encode
           `(("model" . ,yap-model)
             ("messages" . ,(yap--convert-alist messages)))))
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
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(format "x-api-key: %s" yap-api-key:anthropic))))
         (url-request-data
          (json-encode
           `(("model" . ,yap-model)
             ("system" . ,(yap--system-message messages))
             ("messages" . ,(yap--convert-alist-sans-system messages)))))
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

(defun yap--present-response (response)
  "Present the RESPONSE in a posframe or a new buffer, defaulting to the echo area.
You can always call `yap-display-output-buffer' to view the output in
a separate buffer."
  (let ((buffer (get-buffer-create yap--response-buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert response)
      ;; Enable markdown mode if available
      (if (fboundp 'markdown-mode) (markdown-mode)))
    (if (or yap-respond-in-buffer (> (length response) yap-respond-in-buffer-threshold))
        (display-buffer buffer)
      (if (and (featurep 'posframe) (fboundp 'posframe-show) (not yap-no-popup))
          (posframe-show " *yap-response*"
                         :string response
                         :timeout yap-popup-timeout
                         :border-width 2
                         :min-width 36
                         :max-width fill-column
                         :min-height 1
                         :left-fringe 8
                         :right-fringe 8
                         :border-color (face-attribute 'vertical-border :foreground)
                         :position (point))
        (message response)))))

(defun yap-display-output-buffer ()
  "Display the output buffer for yap."
  (interactive)
  (display-buffer yap--response-buffer))

(defun yap-prompt (&optional template prompt)
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

(defun yap-rewrite (&optional template prompt)
  "Prompt the user with the given PROMPT using TEMPLATE if provided.
Rewrite the buffer or selection if present with the returned response."
  (interactive "P")
  (let* ((template (if (equal template '(4)) ; Check if C-u (universal argument) is provided
                       (intern (completing-read "Template: " (mapcar 'car yap-templates) nil t))
                     (or template 'default-rewrite))) ; Otherwise, use default template if not provided
         (llm-messages (yap--get-filled-template template)))
    (if llm-messages
        (yap--rewrite-buffer-or-selection (yap--get-llm-response llm-messages) (current-buffer))
      (message "[ERROR] Failed to fill template for prompt: %s" prompt))))

(defun yap-write (&optional template prompt)
  "Prompt the user with the given PROMPT using TEMPLATE if provided.
Kinda like `yap-rewrite', but just writes instead of replace."
  (interactive "P")
  (let* ((template (if (equal template '(4)) ; Check if C-u (universal argument) is provided
                       (intern (completing-read "Template: " (mapcar 'car yap-templates) nil t))
                     (or template 'default-rewrite))) ; Otherwise, use default template if not provided
         (llm-messages (yap--get-filled-template template)))
    (if llm-messages
        (insert (yap--get-llm-response llm-messages))
      (message "[ERROR] Failed to fill template for prompt: %s" prompt))))

(provide 'yap)
;;; yap.el ends here
