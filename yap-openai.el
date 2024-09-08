;;; yap-openai.el --- OpenAI pieces of yap -*- lexical-binding: t; -*-

;;; Commentary:
;; OpenAI specific pieces of yap.

;;; Code:
(require 'url)
(require 'plz)

(defcustom yap-llm-base-url:openai "https://api.openai.com/v1"
  "Base URL for the OpenAI API."
  :type 'string
  :group 'yap)

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

;; Use `(setq url-debug 1)' to debug things
;; Use `(setq url-debug nil)' to disable debugging
(defun yap--get-llm-response:openai (messages partial-callback &optional final-callback)
  "Get the response from OpenAI LLM for the given set of MESSAGES.
PARTIAL-CALLBACK is called with each chunk of the response.
FINAL-CALLBACK is called with the final response."
  (let* ((url (concat yap-llm-base-url:openai "/chat/completions"))
         (headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(format "Bearer %s" yap-api-key:openai))))
         (json-data (json-encode
                     `(("model" . ,yap-model)
                       ("stream" . t)
                       ("messages" . ,(yap--convert-messages messages))))))
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
                    ;; Save last line to prev-pending in case it is cut off
                    (if (< (length json-lines) (length trimmed-lines))
                        (setq prev-pending (car (last lines)))
                      (setq prev-pending ""))

                    ;; Data is in choices.0.delta.content
                    (mapc (lambda (x)
                            (when-let* ((message
                                         (alist-get 'content
                                                    (alist-get 'delta
                                                               (aref (alist-get 'choices x) 0)))))
                              (when partial-callback
                                (funcall partial-callback (yap--utf8-convert message)))))
                          json-objects)))))))

(provide 'yap-openai)
;;; yap-openai.el ends here
