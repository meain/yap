;;; yap-ollama.el --- Ollama pieces of yap -*- lexical-binding: t; -*-

;;; Commentary:
;; Ollama specific pieces of yap.  Ollama has OpenAI compatible API
;; and so we can just reuse the work we did there.

;;; Code:
(require 'yap-openai)

(defcustom yap-llm-base-url:ollama "http://localhost:11434/v1"
  "The base URL for Ollama."
  :type 'string
  :group 'yap)

(defun yap--get-models:ollama ()
  "Get the models for Ollama."
  (let ((yap-llm-base-url:openai yap-llm-base-url:ollama))
    (message "Base url: %s" yap-llm-base-url:openai)
    (yap--get-models:openai)))

(defun yap--get-llm-response:ollama (messages partial-callback &optional final-callback)
  "Get the response from Ollama for the given set of MESSAGES.
PARTIAL-CALLBACK is called with each chunk of the response.
FINAL-CALLBACK is called with the final response."
  (let ((yap-llm-base-url:openai yap-llm-base-url:ollama)
        (yap-api-key:openai "fake"))
    (yap--get-llm-response:openai messages partial-callback final-callback)))

(provide 'yap-ollama)
;;; yap-ollama.el ends here