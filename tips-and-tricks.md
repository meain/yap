# Tips and tricks

## Make it so that yap response window always shows up at the top

This configuration ensures that the `*yap-response*` buffer consistently appears at the top of your Emacs frame and occupies 30% of the window height, making it easily visible after each response.

```emacs-lisp
;; Add window rules for *yap-response* buffer so that it shows up at top of the frame
(add-to-list 'display-buffer-alist
             `(,(rx bos "*yap-response*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . top)
               (window-height   . 0.3)))
```

## Setup a function to quickly switch to your favorite model

Define an interactive function that sets your preferred service and model. Call this to instantly switch your default LLM model, saving time when frequently changing between models.

```emacs-lisp
(defun meain/yap-set-default-model ()
  (interactive)
  (setq yap-service "openai")
  (setq yap-model "gpt-4o-mini"))
```

## Using OpenRouter Free Models

Interactively browse and choose from OpenRouter's free models (those with `:free` suffix).

```emacs-lisp
(defun meain/yap-use-openrouter-free ()
  (interactive)
  (let* ((models (seq-filter (lambda (x) (string-suffix-p ":free" x))
                             (yap--get-models:openrouter)))
         (model (completing-read "Model: " models)))
    (setq yap-llm-provider-override nil)
    (setq yap-service "openrouter")
    (setq yap-model model)))
```

## Add a keybinding to a particular template that you use often

Bind a keyboard shortcut to quickly invoke a yap template, like code explanation, improving your workflow without manually triggering commands each time.

```emacs-lisp
(global-set-key (kbd "<your-fav-keybinding>") (lambda () (interactive) (yap-prompt 'explain-code)))
```

## Set custom Ollama endpoint

Override the default Ollama API URL to point yap at your specific self-hosted instance or different endpoint, providing flexibility in deployment.

```emacs-lisp
(setq yap-llm-base-url:ollama "https://my-hosted-ollama/v1")
```

## Loading context from external sources

For example, if you want to load the context from an external source
like [meain/refer](https://github.com/meain/refer), you can do
something like this:

```emacs-lisp
(defun meain/yap-template-with-refer (prompt-type)
(let ((prompt (read-string "Prompt: ")))
    (yap-template-external-context
    (if (eq prompt-type 'rewrite)
        yap--default-system-prompt-for-rewrite
        yap--default-system-prompt-for-prompt)
    prompt
    (current-buffer)
    (shell-command-to-string (concat "refer search --format llm '" prompt "'")))))
(add-to-list 'yap-templates '(yap-rewrite-with-refer . (lambda () (meain/yap-template-with-refer 'rewrite))))
(add-to-list 'yap-templates '(yap-prompt-with-refer . (lambda () (meain/yap-template-with-refer 'prompt))))
```

## Maintain a personal prompts library

Create and fetch reusable prompt files stored in your Emacs directory, letting you standardize instructions for the LLM and maintain an organized library as simple text files.

```emacs-lisp
(defun meain/get-llm-prompt (name)
  "Get the prompt for NAME."
  (with-temp-buffer
    (insert-file-contents
     (string-join (list
                   (expand-file-name user-emacs-directory)
                   "prompts"
                   (format "%s.md" name)) "/"))
    (buffer-string)))

(add-to-list
 'yap-templates
 '(identify-actionable-change . (lambda ()
                                  (yap-template-prompt (meain/get-llm-prompt "identify-actionable-change")))))
```

## Have yap perform a change from the last yap response

Quickly apply changes proposed by the LLM based on the last yap response.

```emacs-lisp
(add-to-list
 'yap-templates
 '(perform-proposed-change . (lambda ()
                               (yap-template-buffer-context
                                (meain/get-llm-prompt "perform-proposed-change")
                                (let ((proposal (read-string "Proposal (default: prev yap response): ")))
                                  (if (string= proposal "")
                                      (with-current-buffer "*yap-response*"
                                        (buffer-substring-no-properties (point-min) (point-max)))
                                    proposal))
                                (current-buffer)))))
```
