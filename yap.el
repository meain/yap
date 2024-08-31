;;; yap.el --- A package to do quick interactions with llm -*- lexical-binding: t; -*-

;; URL: https://github.com/meain/yap
;; Keywords: llm, convenience
;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "25.1") (plz "0.9"))
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
(require 'plz)

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

(defun yap-use-fast-model ()
  "Use the fast model for the yap command."
  (interactive)
  (if-let* ((model (pcase yap-service
                     ("openai" "gpt-4o-mini"))))
      (setq yap-model model)))

(defun yap-use-best-model ()
  "Use the best model for the yap command."
  (interactive)
  (if-let* ((model (pcase yap-service
                     ("openai" "gpt-4o"))))
      (setq yap-model model)))

(defun yap--process-response (messages resp callback)
  "Process the final complete response from LLM.
MESSAGES is the original messages sent to LLM.
RESP is the final response from LLM.
CALLBACK is the function to call with the final response."
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
        (if callback (funcall callback resp)))
    (message "[ERROR] Unable to get response %s" (yap--get-error-message resp))))

;; Use `(setq url-debug 1)' to debug things
;; Use `(setq url-debug nil)' to disable debugging
(defun yap--get-llm-response:openai (messages partial-callback &optional final-callback)
  "Get the response from OpenAI LLM for the given set of MESSAGES.
PARTIAL-CALLBACK is called with each chunk of the response.
FINAL-CALLBACK is called with the final response."
  (let* ((inhibit-message t)
         (headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(format "Bearer %s" yap-api-key:openai))))
         (json-data (json-encode
                     `(("model" . ,yap-model)
                       ("stream" . t)
                       ("messages" . ,(yap--convert-messages messages))))))
    (let ((prev-pending "")
          (full-message "")
          (initial-message t)
          (inhibit-message t))
      (plz 'post
        "https://api.openai.com/v1/chat/completions"
        :headers headers
        :body json-data
        :as 'string
        :then (lambda (_)
                (when final-callback
                  (funcall final-callback
                           (with-current-buffer yap--response-buffer (buffer-string)))))
        :filter (lambda (process output)
                  ;; Let plz do its thing
                  ;; This code is from `test-plz-process-filter'
                  (when (buffer-live-p (process-buffer process))
                    (with-current-buffer (process-buffer process)
                      (let ((movingp (= (point) (process-mark process))))
                        (save-excursion
                          (goto-char (process-mark process))
                          (insert output)
                          (set-marker (process-mark process) (point)))
                        (when movingp
                          (goto-char (process-mark process))))))

                  (let* ((filtered-output
                          (if initial-message
                              (progn
                                (setq initial-message nil)
                                (substring output (string-match "\r\n\r\n" output))) ; trim out headers
                            (concat prev-pending output)))
                         (lines (split-string filtered-output "\n" t))
                         ;; trim out empty lines including ones with just \r
                         (non-empty-lines (seq-filter (lambda (x) (length> x 2)) lines))
                         ;; trim out `data: ` prefix from each line
                         (trimmed-lines (mapcar (lambda (x) (substring x 6)) non-empty-lines))
                         ;; filter out lines that are not json as line can but cut off half way through
                         (json-lines (seq-filter (lambda (x) (ignore-errors (json-read-from-string x))) trimmed-lines))
                         ;; convert to json-objects
                         (json-objects (mapcar 'json-read-from-string json-lines)))
                    (with-current-buffer yap--response-buffer
                      ;; Save last line to prev-pending in case it is cut off
                      (if (< (length json-lines) (length trimmed-lines))
                          (setq prev-pending (car (last lines)))
                        (setq prev-pending ""))

                      ;; Data is in choices.0.delta.content
                      (mapc (lambda (x)
                              (let ((message
                                     (alist-get 'content
                                                (alist-get 'delta
                                                           (aref (alist-get 'choices x) 0)))))
                                (when (and partial-callback message)
                                  (funcall partial-callback message))))
                            json-objects))))))))

(defun yap--get-llm-response (messages partial-callback &optional final-callback)
  "Get the response from LLM for the given set of MESSAGES.
PARTIAL-CALLBACK is called with each chunk of the response.
FINAL-CALLBACK is called with the final response."
  (message "Processing request via %s and %s model..." yap-service yap-model)
  (pcase yap-service
    ("openai" (yap--get-llm-response:openai messages partial-callback final-callback))
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
          (yap--clean-response-buffer)
          (yap-show-response-buffer)
          (yap--get-llm-response llm-messages
                                 (lambda (chunk)
                                   (yap--insert-chunk-to-response-buffer chunk))))
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
          (yap--clean-response-buffer)
          (yap-show-response-buffer)
          (yap--get-llm-response llm-messages
                                 (lambda (chunk)
                                   (yap--insert-chunk-to-response-buffer chunk))
                                 (lambda (message)
                                   (yap--hide-response-buffer)
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
