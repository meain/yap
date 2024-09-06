;;; yap-utils.el --- Bunch of util functions for yap -*- lexical-binding: t; -*-

;;; Commentary:
;; Bunch of util functions for yap

;;; Code:
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

(defun yap--show-diff-and-confirm (before after)
  "Show the diff between BEFORE and AFTER."
  (let ((diff (substring-no-properties
               (shell-command-to-string
                (format "diff -u <(echo %s) <(echo %s)"
                        (shell-quote-argument before)
                        (shell-quote-argument after))))))
    (with-temp-buffer
      (insert diff)
      (diff-mode)
      (display-buffer (current-buffer))
      (prog1
          (yes-or-no-p "Do you want to apply the changes? ")
        (kill-buffer)))))

(defun yap--rewrite-buffer-or-selection (response buffer start end)
  "Replace the content in BUFFER from START to END with the provided RESPONSE."
  (with-current-buffer buffer
    (if response
        (let* ((to-replace (buffer-substring-no-properties start end)))
          (if (or (not yap-show-diff-before-rewrite)
                  (yap--show-diff-and-confirm to-replace response))
              (progn
                (delete-region start end)
                (insert response "\n"))
            (message "No changes made.")))
      (message "[ERROR] Failed to get a response from LLM"))))

(provide 'yap-utils)
;;; yap-utils.el ends here