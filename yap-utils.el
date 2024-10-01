;;; yap-utils.el --- Bunch of util functions for yap -*- lexical-binding: t; -*-

;;; Commentary:
;; Bunch of util functions for yap

;;; Code:
(require 'plz)

(defvar yap--response-buffer "*yap-response*")

(defun yap--clean-response-buffer ()
  "Clean the response buffer."
  (with-current-buffer (get-buffer-create yap--response-buffer)
    (erase-buffer)))

(defun yap--insert-chunk-to-response-buffer (chunk)
  "Insert CHUNK to the response buffer."
  (with-current-buffer (get-buffer-create yap--response-buffer)
    (save-excursion
      (goto-char (point-max))
      (insert chunk))))

(defcustom yap-follow-output nil
  "Whether to follow the output buffer or not."
  :type 'boolean
  :group 'yap)

(defun yap--replace-response-buffer (content)
  "Replace the response buffer with CONTENT."
  (with-current-buffer (get-buffer-create yap--response-buffer)
    (let* ((current-content (buffer-string))
           (is-substring (string-prefix-p current-content content))
           (chunk (string-remove-prefix current-content content)))
      (when (not (string= current-content content))
        (save-excursion
          (when (not is-substring) (erase-buffer))
          (goto-char (point-max))
          (insert chunk))
        (when yap-follow-output
          (goto-char (point-max)))))))

(defun yap-show-response-buffer ()
  "Show the yap response buffer."
  (interactive)
  (with-current-buffer (get-buffer-create yap--response-buffer)
    (if (fboundp 'markdown-mode) (markdown-mode)))
  (display-buffer (get-buffer-create yap--response-buffer)))

(defun yap--hide-response-buffer ()
  "Hide the yap response buffer."
  (delete-windows-on yap--response-buffer))

(defun yap--parse-csv-line (line)
  "Parse a CSV LINE into a list of fields, handling quotes properly."
  (let ((result '())
        (field "")
        (in-quote nil)
        (i 0))
    (while (< i (length line))
      (let ((char (aref line i)))
        (cond
         ((and (not in-quote) (eq char ?,))
          ;; Comma outside quotes: end of field
          (push (string-trim field) result)
          (setq field ""))
         ((eq char ?\")
          ;; Quote character
          (if in-quote
              (if (and (< (1+ i) (length line)) (eq (aref line (1+ i)) ?\"))
                  ;; Escaped quote
                  (progn
                    (setq field (concat field "\""))
                    (cl-incf i))
                ;; End of quoted field
                (setq in-quote nil))
            ;; Start of quoted field
            (setq in-quote t)))
         (t
          ;; Any other character
          (setq field (concat field (char-to-string char)))))
        (cl-incf i)))
    ;; Add the last field
    (push (string-trim field) result)
    ;; Return the fields in correct order
    (nreverse result)))

(defun yap--get-error-message (object)
  "Parse out error message from the OBJECT if possible."
  (if (alist-get 'error object)
      (alist-get 'message (alist-get 'error object))
    object))

(defun yap--convert-messages-sans-system (messages)
  "Convert MESSAGES from (role . content) to OpenAI format, without system message."
  (seq-filter #'identity
              (seq-map (lambda (message)
                         (let ((role (plist-get message :role))
                               (content (plist-get message :content)))
                           (unless (equal role 'system)
                             (make-llm-chat-prompt-interaction
                              :role (symbol-name role)
                              :content content))))
                       messages)))

(defun yap--system-message (messages)
  "Check if the given MESSAGES contain a system message."
  (when-let ((system-message
              (seq-find (lambda (message)
                          (equal (plist-get message :role) 'system))
                        messages)))
    (plist-get system-message :content)))

(defun yap-display-output-buffer ()
  "Display the output buffer for yap."
  (interactive)
  (display-buffer yap--response-buffer))

(defun yap--utf8-convert (message)
  "Convert MESSAGE to UTF-8."
  (decode-coding-string (string-make-unibyte message) 'utf-8))

(defun yap--handle-error (url headers content err)
  "Write the URL, HEADERS, CONTENT and ERR into the *yap-errors* buffer."
  (let* ((error-buffer (get-buffer-create "*yap-errors*"))
         (response (plz-error-response err))
         (body (plz-response-body response)))
    (with-current-buffer error-buffer
      (erase-buffer)  ;; Clear previous errors
      (insert (format "URL: %s\n\n" url))
      (insert (format "Headers:\n%s\n\n" (json-encode headers)))
      (insert (format "Content:\n%s\n\n" content))
      (insert (format "Error:\n%s" body))))
  (message "An error occurred. Please check the *yap-errors* buffer for details."))


;;; Provider specific stuff
;; Keeping it as a variable so that users can update it
(defcustom yap--anthropic-models
  '("claude-3-5-sonnet-20240620"
    "claude-3-opus-20240229"
    "claude-3-sonnet-20240229"
    "claude-3-haiku-20240307")
  "List of Anthropic models available for use."
  :type '(repeat string)
  :group 'yap)

(defun yap--get-models:anthropic ()
  "Get available models from Anthropic.
Anthropic does not publish an API endpoint and so we have to manually
manage it unfortunately."
  yap--anthropic-models)

(defconst yap-llm-base-url:openai "https://api.openai.com/v1"
  "Base URL for the OpenAI API.")

(defun yap--get-models:openai ()
  "Get a list of OpenAI models available."
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(format "Bearer %s" yap-api-key:openai))))
         (url-request-data-type 'json)
         (resp (with-current-buffer (url-retrieve-synchronously
                                     (concat yap-llm-base-url:openai "/models"))
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

(defcustom yap-llm-base-url:ollama "http://localhost:11434/v1"
  "The base URL for Ollama."
  :type 'string
  :group 'yap)

(defun yap--get-models:ollama ()
  "Get the models for Ollama."
  (let ((yap-llm-base-url:openai yap-llm-base-url:ollama))
    (yap--get-models:openai)))

(provide 'yap-utils)
;;; yap-utils.el ends here