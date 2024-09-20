;;; yap.el --- A package to do quick interactions with llm -*- lexical-binding: t; -*-

;; URL: https://github.com/meain/yap
;; Keywords: llm, convenience
;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "25.1") (plz "0.9") (llm "0.17"))
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
(require 'llm)
(require 'llm-openai)
(require 'llm-ollama)
(require 'llm-claude)

(require 'yap-templates)
(require 'yap-utils)

(defgroup yap nil
  "Customization options for the yap command."
  :group 'tools)

(defcustom yap-service "openai"
  "The service to use for the yap command."
  :type 'string
  :group 'yap)

(defcustom yap-model "gpt-4o"
  "The model to use for the yap command."
  :type 'string
  :group 'yap)

(defcustom yap-api-key-openai nil
  "The API key to use with OpenAI models in the yap command."
  :type 'string
  :group 'yap)

(defcustom yap-log-requests nil
  "Provide a folder to log all the requests and responses."
  :type 'string
  :group 'yap)

(defcustom yap-rewrite-auto-accept nil
  "If non-nil, automatically accept the rewrite."
  :type 'boolean
  :group 'yap)

(defun yap-set-service ()
  "Set the service to use for the yap command."
  (interactive)
  (setq yap-service
        (completing-read
         "Service: "
         '("openai" "ollama" "anthropic")))
  (yap-set-model))

(defun yap-set-model ()
  "Fetch models and update the variable."
  (interactive)
  (if-let* ((models (pcase yap-service
                      ("openai" (yap--get-models:openai))
                      ("ollama" (yap--get-models:ollama))
                      ("anthropic" (yap--get-models:anthropic))))
            (model-name (completing-read "Model: " models)))
      (setq yap-model model-name)))

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

(defcustom yap-llm-provider-override nil
  "Override the LLM provider.
Supports any provider from ahyatt/llm."
  :group 'yap)

(defun yap--get-provider ()
  "Retrieve the LLM provider based on the specified service.

If `yap-llm-provider-override` is set, return that provider.  Otherwise,
determine the provider based on the value of `yap-service`.  Supported
services include 'openai', 'ollama', and 'anthropic'.  If an unsupported
service is specified, log an error message and return nil."
  (if yap-llm-provider-override
      yap-llm-provider-override
    (pcase yap-service
      ("openai" (make-llm-openai :key yap-api-key:openai :chat-model yap-model))
      ("ollama" (make-llm-ollama :chat-model yap-model))
      ("anthropic" (make-llm-claude :key yap-api-key:anthropic :chat-model yap-model))
      (_ (message "[ERROR] Unsupported service: %s" yap-service) nil))))

(defun yap--llm-generate-prompt (llm-messages)
  "Generate prompt based on `LLM-MESSAGES'."
  (make-llm-chat-prompt
   :context (yap--system-message llm-messages)
   :interactions (yap--convert-messages-sans-system llm-messages)))

(defun yap--do (messages partial-callback &optional final-callback)
  "Get LLM response for MESSAGES.
Call PARTIAL-CALLBACK with each chunk, FINAL-CALLBACK with final response."
  (let ((llm-warn-on-nonfree nil) ; this is a personal choice
        ;; (llm-log t) ; use this to log
        (llm-provider (yap--get-provider))
        (prompt (yap--llm-generate-prompt messages)))
    (yap--clean-response-buffer)
    (message "Processing request via %s and %s model..." yap-service yap-model)
    (llm-chat-streaming llm-provider prompt partial-callback final-callback
                        (lambda (_ error) (message "Error processing request: %s" error)))))

(defun yap-buffer-close ()
  "Close the response buffer."
  (interactive)
  (when (get-buffer-window yap--response-buffer)
    (delete-window (get-buffer-window yap--response-buffer))))

(defun yap-buffer-toggle ()
  "Open or close the *yap-response* buffer."
  (interactive)
  (if (get-buffer-window yap--response-buffer)
      (delete-window (get-buffer-window yap--response-buffer))
    (with-current-buffer yap--response-buffer
      (if (length> (buffer-string) 0)
          (yap-show-response-buffer)
        (message "There is nothing in the yap response buffer")))))

(defun yap-prompt (&optional template)
  "Prompt the user with the given PROMPT using TEMPLATE if provided.
If TEMPLATE is not provided or nil, use the default template.
If invoked with a universal argument \\[universal-argument], prompt for TEMPLATE selection.
The response from LLM is displayed in the *yap-response* buffer."
  (interactive "P")
  (let* ((template (if (equal template '(4)) ; Check if C-u (universal argument) is provided
                       (intern (completing-read "Template: " (mapcar 'car yap-templates) nil t))
                     (or template 'default-prompt))) ; Otherwise, use default template if not provided
         (llm-messages (yap--get-filled-template template)))
    (if llm-messages
        (let ((buffer (current-buffer)))
          (let ((first-chunk t))
            (yap--do
             llm-messages
             (lambda (resp)
               (when first-chunk
                 (setq first-chunk nil)
                 (yap-show-response-buffer))
               (setq previous resp)
               (yap--replace-response-buffer resp))
             (lambda (resp)
               (yap--process-response llm-messages resp
                                      'yap--replace-response-buffer)))))
      (message "[ERROR] Failed to fill template"))))

(defun yap-rewrite-cancel ()
  "Cancel the rewrite and kill the buffer."
  (with-current-buffer yap--response-buffer
    (kill-buffer)
    (message "Canceled rewrite")))

(defun yap-rewrite-show-diff (buffer start end message)
  "Show the diff between the BUFFER and the rewritten MESSAGE.
START and END are the region to replace in original buffer."
  (let ((to-replace (with-current-buffer buffer
                      (buffer-substring-no-properties start end)))
        (temp-original (make-temp-file "yap-rewrite-original-"))
        (temp-rewritten (make-temp-file "yap-rewrite-rewritten-")))
    (with-temp-file temp-original
      (insert to-replace))
    (with-temp-file temp-rewritten
      (insert message))
    (with-current-buffer (get-buffer-create "*yap-rewrite-diff*")
      (erase-buffer)
      (insert (with-temp-buffer
                (call-process "diff" nil t nil "-u" temp-original temp-rewritten)
                (buffer-string)))
      (diff-mode)
      (pop-to-buffer (current-buffer))
      (delete-file temp-original)
      (delete-file temp-rewritten))))

(defun yap-rewrite-accept (buffer start end message)
  "Accept the rewrite and replace the region in the BUFFER with MESSAGE.
START and END are the region to replace in original buffer."
  (with-current-buffer buffer
    (delete-region start end)
    (goto-char start)
    ;; newline is because of evil-mode not playing nice
    (if (boundp 'evil-mode)
        (insert message "\n")
      (insert message)))
  (with-current-buffer yap--response-buffer
    (kill-buffer)))

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
              (crrent-major-mode major-mode)
              (start (if (region-active-p) (region-beginning) (point-min)))
              (end (if (region-active-p) (region-end) (point-max))))
          (let ((first-chunk t))
            (yap--do
             llm-messages
             (lambda (resp)
               (when first-chunk
                 (setq first-chunk nil)
                 (yap-show-response-buffer)
                 (with-current-buffer (get-buffer-create yap--response-buffer)
                   (funcall crrent-major-mode)))
               (yap--replace-response-buffer resp))
             (lambda (resp)
               ;; Using buffer text instead of message, this will let the user edit
               ;; the llm response and then use the edited version for rewrites.
               (if yap-rewrite-auto-accept
                   (yap-rewrite-accept buffer start end)
                 (progn
                   (with-current-buffer yap--response-buffer
                     (setq header-line-format
                           (concat
                            "C-c C-c: Accept rewrite | "
                            "C-c C-d: View diff | "
                            "C-c C-k: Cancel"))
                     (local-set-key (kbd "C-c C-k")
                                    (lambda () (interactive) (yap-rewrite-cancel)))
                     (local-set-key (kbd "C-c C-c")
                                    (lambda () (interactive)
                                      (yap-rewrite-accept buffer start end (buffer-string))))
                     (local-set-key (kbd "C-c C-d")
                                    (lambda () (interactive) (yap-rewrite-show-diff buffer start end (buffer-string)))))
                   (yap--replace-response-buffer resp)
                   (pop-to-buffer yap--response-buffer)))))))
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
        (let ((buffer (current-buffer))
              (previous ""))
          (yap--do llm-messages
                   (lambda (resp)
                     (with-current-buffer buffer
                       (insert (string-remove-prefix previous resp))
                       (setq previous resp)))))
      (message "[ERROR] Failed to fill template"))))

(provide 'yap)
;;; yap.el ends here
