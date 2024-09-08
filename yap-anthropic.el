;;; yap-anthropic.el --- Anthropic pieces of yap -*- lexical-binding: t; -*-

;;; Commentary:
;; Anthropic specific pieces of yap.

;;; Code:
(require 'url)
(require 'plz)
(require 'yap-utils)

(defcustom yap-llm-base-url:anthropic "https://api.anthropic.com/v1"
  "Base URL for the Anthropic API."
  :type 'string
  :group 'yap)

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

(defun yap--get-llm-response:anthropic (messages partial-callback &optional final-callback)
  "Get the response from Anthropic LLM for the given set of MESSAGES.
PARTIAL-CALLBACK is called with each chunk of the response.
FINAL-CALLBACK is called with the final response."
  (let* ((url (concat yap-llm-base-url:anthropic "/messages"))
         (headers
          `(("Content-Type" . "application/json")
            ("x-api-key" . ,yap-api-key:anthropic)
            ("anthropic-version" . "2023-06-01")))
         (json-data (json-encode
                     `(("model" . ,yap-model)
                       ("stream" . t)
                       ("system" . ,(yap--system-message messages))
                       ("messages" . ,(yap--convert-messages-sans-system messages))
                       ("max_tokens" . 1000)))))
    (let ((prev-pending "")
          (initial-message t)
          (inhibit-message t))
      (plz 'post url
        :headers headers
        :body json-data
        :as 'string
        :then (lambda (_)
                (when final-callback
                  (funcall final-callback
                           (with-current-buffer yap--response-buffer (buffer-string)))))
        :else (lambda (err) (yap--handle-error url headers json-data err))
        :filter (lambda (process output)
                  ;; Let plz do its thing
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
                                (substring output (string-match "\r\n\r\n" output)))
                            (concat prev-pending output)))
                         (lines (split-string filtered-output "\n" t))
                         (non-empty-lines (seq-filter (lambda (x) (length> x 2)) lines))
                         (trimmed-lines (mapcar (lambda (x) (substring x 6)) non-empty-lines))
                         (json-lines (seq-filter (lambda (x) (ignore-errors (json-read-from-string x))) trimmed-lines))
                         (json-objects (mapcar 'json-read-from-string json-lines)))
                    (if (< (length json-lines) (length trimmed-lines))
                        (setq prev-pending (car (last lines)))
                      (setq prev-pending ""))

                    (mapc (lambda (x)
                            (when-let* ((event-type (alist-get 'type x))
                                        (message (alist-get 'delta x))
                                        (content (alist-get 'text message)))
                              (when (and (string= event-type "content_block_delta")
                                         partial-callback)
                                (funcall partial-callback content))))
                          json-objects)))))))

(provide 'yap-anthropic)
;;; yap-anthropic.el ends here
