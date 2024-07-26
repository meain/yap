;;; yap-utils.el --- Bunch of util functions for yap -*- lexical-binding: t; -*-

;;; Commentary:
;; Bunch of util functions for yap

;;; Code:
(defvar yap-respond-in-buffer nil
  "Whether to respond in a new buffer or the echo area.")
(defvar yap-respond-in-buffer-threshold 300
  "If the response is longer than this, always respond in a new buffer.")
(defvar yap-show-diff-before-rewrite t
  "Whether to show the diff before rewriting the buffer.")
(defvar yap-popup-timeout 5
  "The time in seconds to show the popup for.")
(defvar yap-no-popup nil
  "Whether to show the response in a popup or not.
If non-nil, it will use the echo area.")

(defvar yap--response-buffer "*yap-response*")

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
  (mapcar (lambda (pair)
            (let ((role (car pair))
                  (content (cdr pair)))
              (if (not (string= role "system"))
                  `(("role" . ,role) ("content" . ,content))
                nil)))
          messages))

(defun yap--system-message (messages)
  "Check if the given MESSAGES contain a system message."
  (let ((system-message (seq-find (lambda (pair)
                                    (string= (car pair) "system"))
                                  messages)))
    (if system-message
        (cdr system-message)
      nil)))

(defun yap--present-response (response original-buffer)
  "Present the RESPONSE in a posframe or a new buffer, defaulting to the echo area.
You can always call `yap-display-output-buffer' to view the output in
a separate buffer. ORIGINAL-BUFFER is the buffer we initiated the request."
  (let ((buffer (get-buffer-create yap--response-buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert response)
      ;; Enable markdown mode if available
      (if (fboundp 'markdown-mode) (markdown-mode)))
    (if (or yap-respond-in-buffer (> (length response) yap-respond-in-buffer-threshold))
        (display-buffer buffer)
      (if (and (featurep 'posframe) (fboundp 'posframe-show) (not yap-no-popup))
          (with-current-buffer original-buffer
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
                           :position (point)))
        (message response)))))

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
      (yes-or-no-p "Do you want to apply the changes? "))))

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