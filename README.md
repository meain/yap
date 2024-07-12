# `yap` - Your Assistant for Prompting LLMs

`yap` is an Emacs package designed to facilitate interactions with
Large Language Models (LLMs) by providing predefined templates to
prompt the LLMs and modify buffers based on the responses. This
package is particularly useful for quick questions, code explanations,
or even buffer rewrites.

*Btw, the word yap loosely translates to "do it" in Turkish.*

> [!WARNING]
> This package is still in its early stages and may have bugs or
> issues. Please report any problems you encounter. Also the Anthropic
> support is untested (they somehow don't like any of my cards).

https://github.com/user-attachments/assets/b947e1e5-8d34-4dbc-aacd-20a06beb74c7

---

## Features

- **Prompt Templates**: Easily call predefined templates for common tasks.
- **Buffer Modifications**: Modify your buffer with responses from LLMs.
- **Service Support**: Currently supports OpenAI and Anthropic models.
- **Custom Templates**: Create and use your own templates.

## Installation

To install `yap`, add the following to your Emacs configuration:

```emacs-lisp
(use-package yap
  :load-path "path-to-yap-directory"
  :config
  ;; Setup API keys
  (setq yap-api-key:openai "your-openai-api-key")
  (setq yap-api-key:anthropic "your-anthropic-api-key")

  ;; Few customizations (optional)
  (setq yap-respond-in-buffer nil) ; respond in minibuffer vs separate buffer
  (setq yap-respond-in-buffer-threshold 300) ; override yap-respond-in-buffer if response is longer than this
  (setq yap-show-diff-before-rewrite t)

  ;; Optionally you can also specify the service and model here
  (yap-set-service "openai") ; default: openai
  (yap-set-model "gpt-4o") ; default: gpt-3.5-turbo

  ;; Setup keybindings (optional)
  ;; These are what I have, not recommended to use as is
  (global-set-key (kbd "M-f y y") 'yap-prompt)
  (global-set-key (kbd "M-f y r") 'yap-rewrite)
  (global-set-key (kbd "M-f y d") 'yap-do))
```

## Basic Usage

- `yap-prompt`: Prompt the LLM with a selected template.
- `yap-rewrite`: Rewrite the buffer or selected text using the LLM.
- `yap-write`: Insert the LLM response without replacing existing
  content.

Additionally, you can pass a universal argument (`C-u`) to these
commands to bring up a template picker, allowing you to choose from
the available templates interactively:

Besides this, you have two function `yap-set-service` and
`yap-set-model` to set the service and model you want to use.

## Real-World Examples

Here are a few scenarios where `yap` can be especially useful. Most of
the yap templates are optimized to work on a selection and so if you
want to have it work on the entire buffer, you can just select the
entire buffer. There are template types which work on whole buffer,
but the default ones are optimized for selection.

1. **Summarizing Meeting Notes**:
   If you've just finished typing up notes from a meeting and need a concise summary, you can use the `summarize` template.
   ```emacs-lisp
   (yap-prompt 'summarize)
   ```

2. **Explaining Complex Code**:
   When dealing with a particularly complex piece of code that a colleague wrote, and you need a detailed explanation to understand it.
   ```emacs-lisp
   (yap-prompt 'explain-code)
   ```

3. **Optimizing Code**:
   If you're looking to optimize a block of code for better performance, you can use the `optimize-code` template.
   ```emacs-lisp
   (yap-rewrite 'optimize-code)
   ```

4. **Generating Documentation**:
   When you need to generate documentation for a newly written function or module, you can use a custom template to describe it.
   ```emacs-lisp
   (yap-prompt 'default-prompt "Generate documentation for the following function.")
   ```

## Creating New Templates

Creating new templates in `yap` is straightforward. Templates under
the hood are message chains to be sent to `messages` field in llm
APIs. That said, you don't have to build it out from scratch.

There are three levels of templates in yap.

### Simple string template

This is just a string template. Examples would be:

``` emacs-lisp
(joke . "Tell me a joke")
(who-what . "What or who is {{prompt}}? Provide a summary and 5 bullet points.")
```

These get automatically converted to the right format, and if
`{{prompt}}` is present in the string, the user is asked for a prompt
to be included.

### Using builtin helpers

These a little more involved and can be used when you want to provide
additional context along with your final prompt.

There are 4 utility functions that you can leverage as of now:

- `yap-template-prompt`: User prompt + user selection if present for a response
- `yap-template-rewrite`: User prompt + user selection if present for rewrite
- `yap-template-prompt-buffer-context`: User prompt + buffer context + user selection if present for a response
- `yap-template-rewrite-buffer-context`: User prompt + buffer context + user selection if present for rewrite

Here is an example of using them:

```emacs-lisp
(defun document-function (prompt buffer)
  "My custom template using PROMPT and BUFFER."
  (yap-template-prompt "Document the function" buffer))

(add-to-list 'yap-templates '(document-function . document-function))
```

For these cases, you will have to define if the user has to provide
additional prompt. You can do that my specifying this when you add the
the function to the `yap-template` alist.

```emacs-lisp
(add-to-list 'yap-templates
        '(document-function . ((prompt . t)
                               (function . yap-template-prompt))))
```

### Fully custom

If you think these don't work out for you, you can construct the full
set of messages yourself. It will look something like this:

```emacs-lisp
(defun my-custom-buffer-context-template (prompt buffer)
  "Custom template using PROMPT and BUFFER as context."
  (let* ((selection (yap--get-selected-text buffer))
         (before (if selection
                     (buffer-substring-no-properties (point-min) (region-beginning))
                   (buffer-substring-no-properties (point-min) (point))))
         (after (if selection
                    (buffer-substring-no-properties (region-end) (point-max))
                  (buffer-substring-no-properties (point) (point-max)))))
    (if selection
        `(("system" . "You are a helpful assistant. Provide concise and clear responses.")
          ("user" . "I'll provide a document in which I have highlighted a section. Answer for the highlighted section but use the rest of the text as context.")
          ("assistant" . "OK. What is the highlighted text?")
          ("user" . ,(concat "This is the text in the document that is highlighted:\n\n" selection))
          ("assistant" . "What is there before the highlighted section?")
          ("user" . ,(concat "Here is the text before: \n\n" before))
          ("assistant" . "What is there after the highlighted section?")
          ("user" . ,(concat "Here is the text after: \n\n" after))
          ("assistant" . "What can I help you with?")
          ("user" . ,prompt))
      `(("system" . "You are a helpful assistant. Provide concise and clear responses.")
        ("user" . "I'll provide you context about a document that I am working on. I'm somewhere within the document.")
        ("assistant" . "OK. What comes before your current position?")
        ("user" . ,(concat "Here is the text before the cursor:\n\n" before))
        ("assistant" . "What comes after your current position?")
        ("user" . ,(concat "Here is the text after the cursor:\n\n" after))
        ("assistant" . "What can I help you with?")
        ("user" . ,prompt)))))

(add-to-list 'yap-templates
             '(custom-buffer-context . ((prompt . t)
                                        (function . my-custom-buffer-context-template))))
```

---

✨ Happy Prompting! ✨
