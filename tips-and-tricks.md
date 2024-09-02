# Tips and tricks

## Make it so that yap response window always shows up at the top

``` emacs-lisp
;; Add window rules for *yap-response* buffer so that it shows up at top of the frame
(add-to-list 'display-buffer-alist
             `(,(rx bos "*yap-response*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . top)
               (window-height   . 0.2)))
```

## Setup a function to quickly switch to your favorite model

``` emacs-lisp
(defun meain/yap-set-default-model ()
  (interactive)
  (setq yap-service "openai")
  (setq yap-model "gpt-4o-mini"))
```

## Add a keybinding to a particular template that you use often

``` emacs-lisp
(global-set-key (kbd "<your-fav-keybinding>") (lambda () (interactive) (yap-prompt 'explain-code)))
```

## Set custom Ollama endpoint

``` emacs-lisp
(setq yap-llm-base-url:ollama "https://my-hosted-ollama/v1")
```

