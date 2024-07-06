# yap

A thing to help you do stuff with llm. This tool mostly just provides
a way for you to easily call predefined templates and edit buffer
using the provided responses.

*Btw, the word yap loosely translates to "do it" in Turkish.*

---

## Installation

**Once we have it in some package repository**, you can do something like the following:

```emacs-lisp
(use-package yap
  :ensure t
  :config
  (setq yap-api-key openai-api-key)
  (setq yap-respond-in-buffer nil)
  (setq yap-show-diff-before-rewrite t)

  (global-set-key (kbd "M-f y y") 'yap-prompt)
  (global-set-key (kbd "M-f y r") 'yap-rewrite)
  (global-set-key (kbd "M-f y d") 'yap-do))
```
