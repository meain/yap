;;; yap-templates.el --- Basic templates for yap -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains basic templates for yap.  A yap template is a
;; function that gets passed in a prompt, section and a buffer returns
;; the text to be sent to an llm for response.

;;; Code:

(defun yap-template-simple (prompt buffer)
  "A simple yap template that just return the `PROMPT' and selection `BUFFER'."
  (let ((selection (with-current-buffer buffer
                     (if (region-active-p)
                         (buffer-substring (region-beginning) (region-end))))))
    (concat prompt
            (if (> (length selection) 0)
                (concat
                 "\nUse the following text as the context for providing response to the prompt above:\n"
                 selection)))))

(defvar yap-templates
  '((default . yap-template-simple)
    (what . (lambda (query &rest _) (concat "What or who is " query "? Provide a summary and 5 bullet points."))))
  "A list of yap templates.")

(defun yap--get-selected-text ()
  "Get the selected text in the current buffer, if any."
  (interactive)
  (if (region-active-p)
      (let ((selection (buffer-substring (region-beginning) (region-end))))
        (message "Selected text: %s" selection)
        selection)
    nil))

(defun yap--get-filled-template (prompt template buffer)
  "Get the filled `TEMPLATE' for the given `PROMPT'."
  (if-let ((template-func (alist-get template yap-templates)))
    (funcall template-func prompt buffer)))

(provide 'yap-templates)
;;; yap-templates.el ends here