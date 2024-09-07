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
    (goto-char (point-max))
    (insert chunk)))

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

(defun yap--convert-messages (messages)
  "Convert MESSAGES from (role . content) to OpenAI format."
  (mapcar (lambda (pair)
            (let ((role (car pair))
                  (content (cdr pair)))
              `(("role" . ,role) ("content" . ,content))))
          messages))

(defun yap--convert-messages-sans-system (messages)
  "Convert MESSAGES from (role . content) to OpenAI format, without system message."
  (seq-filter #'identity
              (mapcar (lambda (pair)
                        (let ((role (car pair))
                              (content (cdr pair)))
                          (unless (string= role "system")
                            `(("role" . ,role) ("content" . ,content)))))
                      messages)))

(defun yap--system-message (messages)
  "Check if the given MESSAGES contain a system message."
  (let ((system-message (seq-find (lambda (pair)
                                    (string= (car pair) "system"))
                                  messages)))
    (if system-message
        (cdr system-message)
      nil)))

(defun yap-display-output-buffer ()
  "Display the output buffer for yap."
  (interactive)
  (display-buffer yap--response-buffer))

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

(provide 'yap-utils)
;;; yap-utils.el ends here