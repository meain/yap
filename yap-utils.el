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
        (when (<  (- (line-number-at-pos (point-max)) (line-number-at-pos)) 3)
          (goto-char (point-max)))))))

(defun yap-show-response-buffer ()
  "Show the yap response buffer."
  (interactive)
  (with-current-buffer (get-buffer-create yap--response-buffer)
    (visual-line-mode t)
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
  (let ((err (alist-get 'error object)))
    (if (not err)
        object
      (if (and (listp err) (alist-get 'message err))
          (alist-get 'message err)
        err))))

(defun yap--convert-messages-sans-system (messages)
  "Convert MESSAGES from (role . content) to OpenAI format, without system message."
  (seq-filter #'identity
              (seq-map (lambda (message)
                         (let ((role (plist-get message :role))
                               (content (plist-get message :content)))
                           (unless (equal role 'system)
                             (make-llm-chat-prompt-interaction
                              :role role
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

(defcustom yap-llm-base-url:anthropic "https://api.anthropic.com/v1/"
  "The base URL for Anthropic."
  :type 'string
  :group 'yap)

(defun yap--get-models:anthropic ()
  "Get the models for Anthropic."
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("anthropic-version" . "2023-06-01")
            ("x-api-key" . ,yap-api-key:anthropic)))
         (url-request-data-type 'json)
         (resp (with-current-buffer (url-retrieve-synchronously
                                     (concat yap-llm-base-url:anthropic "/models"))
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

(defcustom yap-llm-base-url:groq "https://api.groq.com/openai/v1"
  "The base URL for Groq."
  :type 'string
  :group 'yap)

(defun yap--get-models:groq ()
  "Get the models for Groq."
  (let ((yap-llm-base-url:openai yap-llm-base-url:groq)
        (yap-api-key:openai yap-api-key:groq))
    (yap--get-models:openai)))

(defcustom yap-llm-base-url:openrouter "https://openrouter.ai/api/v1"
  "The base URL for Openrouter."
  :type 'string
  :group 'yap)

(defun yap--get-models:openrouter ()
  "Get the models for Openrouter."
  (let ((yap-llm-base-url:openai yap-llm-base-url:openrouter)
        (yap-api-key:openai yap-api-key:openrouter))
    (yap--get-models:openai)))

(defcustom yap-llm-base-url:github "https://models.inference.ai.azure.com"
  "The base URL for Github."
  :type 'string
  :group 'yap)

;; There is no reliable way, this is best-effort
;; https://github.com/Azure/github-models/issues/5
(defun yap--get-models:github ()
  "Get a list of GitHub models available."
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")))
         (url-request-data-type 'json)
         (resp (with-current-buffer (url-retrieve-synchronously
                                     (concat yap-llm-base-url:github "/models"))
                 (goto-char (point-min))
                 (re-search-forward "^$")
                 (json-read))))
    (if (and resp (vectorp resp))
	    (mapcar (lambda (x) (alist-get 'name x)) resp)
      (message "[ERROR] Unable to get models: %s"
               (if (not resp)
		           "Response is empty"
		         (yap--get-error-message resp)))
      nil)))

(defun yap--select-multiple-files-and-buffers (show-files show-buffers)
  "Select multiple files and buffers within the current project.

  Show only files if SHOW-FILES is non-nil, show only buffers if
  SHOW-BUFFERS is non-nil.  Returns a plist with :files and :buffers
  keys."
  (let* ((project-root (when (fboundp 'project-root)
                         (project-root (project-current))))
         (project-files (when project-root
                          (mapcar (lambda (f) (file-relative-name f project-root))
                                  (project-files (project-current)))))
         (buffer-names (mapcar #'buffer-name (buffer-list)))
         (all-candidates
          (cond
           ((and show-files show-buffers)
            (append
             (mapcar (lambda (f) (format "f:%s" f)) project-files)
             (mapcar (lambda (b) (format "b:%s" b)) buffer-names)))
           (show-files project-files)
           (show-buffers buffer-names)
           (t (error "At least one of show-files or show-buffers must be non-nil"))))
         (selected-items
          (completing-read-multiple
           (cond
            ((and show-files show-buffers) "Select files/buffers: ")
            (show-files "Select files: ")
            (show-buffers "Select buffers: "))
           all-candidates
           nil t nil 'my/select-multiple-files-and-buffers-history)))

    (list
     :files (if show-files
                (if (and show-files show-buffers)
                    (mapcar (lambda (f)
                              (expand-file-name (substring f 2) project-root))
                            (seq-filter (lambda (item)
                                          (string-prefix-p "f:" item))
                                        selected-items))
                  (mapcar (lambda (f)
                            (expand-file-name f project-root))
                          selected-items))
              nil)
     :buffers (if show-buffers
                  (if (and show-files show-buffers)
                      (mapcar (lambda (b)
                                (substring b 2))
                              (seq-filter (lambda (item)
                                            (string-prefix-p "b:" item))
                                          selected-items))
                    selected-items)
                nil))))

(provide 'yap-utils)
;;; yap-utils.el ends here
