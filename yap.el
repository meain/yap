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
(defvar yap-log-requests nil
  "Provide a folder to log all the requests and responses.")

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

(defun yap-set-service ()
  "Set the service to use for the yap command."
  (interactive)
  (setq yap-service (completing-read "Service: " '("openai")))
  (yap-set-model))

(defun yap-set-model ()
  "Fetch models and update the variable."
  (interactive)
  (if-let* ((models (pcase yap-service
                      ("openai" (yap--get-models:openai))))
            (model-name (completing-read "Model: " models)))
      (setq yap-model model-name)))

(defun yap--process-response (messages callback parse-response)
  "Process the response from LLM for the given set of MESSAGES and call CALLBACK.
Use PARSE-RESPONSE to parse the response."
  (lambda (status)
    (if (plist-get status :error)
        (message "[ERROR] %s" (plist-get status :error))
      (progn
        (set-buffer-multibyte t)  ;; This fixes accented characters and emojis
        (goto-char (point-min))
        (re-search-forward "^$")
        (let ((resp (funcall parse-response (current-buffer))))
          (if resp
              (progn
                (when yap-log-requests
                  (mkdir yap-log-requests t)
                  ;; Save logs to disk
                  (let ((json-data (json-encode `(("service" . ,yap-service)
                                                  ("model" . ,yap-model)
                                                  ("messages" . ,messages)
                                                  ("response" . ,resp))))
                        (json-file (format "%s/%s.json" yap-log-requests (format-time-string "%Y%m%d-%H%M%S"))))
                    (with-temp-file json-file
                      (insert json-data))))
                (funcall callback resp))
            (message "[ERROR] Unable to get response %s" (yap--get-error-message resp))))))))

;; Use `(setq url-debug 1)' to debug things
;; Use `(setq url-debug nil)' to disable debugging
(defun yap--get-llm-response:openai (messages callback)
  "Get the response from OpenAI LLM for the given set of MESSAGES.
Once the response is fetched call the CALLBACK."
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
         (url-request-data-type 'json))
    (url-retrieve
     "https://api.openai.com/v1/chat/completions"
     (yap--process-response
      messages callback
      (lambda (buffer)
        (with-current-buffer buffer
          (let* ((resp (json-read))
                 (choices (alist-get 'choices resp)))
            (if choices
                (let* ((message (alist-get 'message (aref choices 0)))
                       (content (alist-get 'content message)))
                  content)
              nil))))))))

(defun yap--get-llm-response (messages callback)
  "Get the response from LLM for the given set of MESSAGES and call the CALLBACK."
  (message "Processing request via %s and %s model..." yap-service yap-model)
  (pcase yap-service
    ("openai" (yap--get-llm-response:openai messages callback))
    (_ (message "[ERROR] Unsupported service: %s" yap-service) nil)))

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
        ;; TODO yap--get-llm-response is not a good name anymore
        (let ((buffer (current-buffer)))
          (yap--get-llm-response llm-messages (lambda (response)
                                                (funcall #'yap--present-response response buffer))))
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
        (let ((buffer (current-buffer))
              (start (if (region-active-p) (region-beginning) (point-min)))
              (end (if (region-active-p) (region-end) (point-max))))
          (yap--get-llm-response llm-messages
                                 (lambda (message)
                                   (yap--rewrite-buffer-or-selection message buffer start end))))
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
        (let ((buffer (current-buffer)))
          (yap--get-llm-response llm-messages
                                 (lambda (message)
                                   (with-current-buffer buffer
                                     (insert message)))))
      (message "[ERROR] Failed to fill template"))))

(provide 'yap)
;;; yap.el ends here
