# `yap` - Your Assistant for Prompting LLMs

`yap` is an Emacs package designed to facilitate interactions with
Large Language Models (LLMs) by providing predefined templates to
prompt the LLMs and modify buffers based on the responses. This
package is particularly useful for quick questions, code explanations,
or even buffer rewrites.

*Btw, the word yap loosely translates to "do it" in Turkish.*

> [!WARNING]
> This package is in very early stages of development. I'm still
> squashing bugs and figuring out the best interface. If you have any
> suggestions or feedback, please let me know.

https://github.com/user-attachments/assets/b947e1e5-8d34-4dbc-aacd-20a06beb74c7

*You can see more screen casts
[here](https://github.com/meain/yap/issues/2). Also, if you like this,
you might also like [meain/yaq](https://github.com/meain/yaq).*

---

## Features

- **Prompt Templates**: Easily call predefined templates for common tasks.
- **Buffer Modifications**: Modify your buffer with responses from LLMs.
- **Service Support**: Currently supports OpenAI, Anthropic and Ollama
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

  ;; Optionally you can specify a service and model
  (yap-set-service "openai") ; default: openai
  (yap-set-model "gpt-4o") ; default: gpt-4o

  ;; You could also set any provider from ahyatt/llm like below
  (setq yap-llm-provider-override (make-llm-openai :key openai-api-key :chat-model model))

  ;; Add window rules for *yap-response* buffer so that it shows up at
  ;; top of the frame
  (add-to-list 'display-buffer-alist
               `(,(rx bos yap--response-buffer eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . top)
                 (window-height   . 0.3)))

  ;; Setup keybindings (optional)
  (global-set-key (kbd "<your-fav-keybinding>") 'yap-prompt)
  (global-set-key (kbd "<your-fav-keybinding>") 'yap-rewrite)
  (global-set-key (kbd "<your-fav-keybinding>") 'yap-write)
  (global-set-key (kbd "<your-fav-keybinding>") (lambda () (interactive) (yap-prompt 'explain-code))))
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

*See [tips-and-tricks](./tips-and-tricks.md) for some useful tips.*

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

4. **Generating Shell Command**:
    When you need to generate a shell command for a given task, you can use the `generate-shell-command` template with `yap-write` in the shell buffer.
    ```emacs-lisp
    (yap-write 'generate-shell-command)
    ```

*Take a look at `yap-templates` in code to see all the available templates.*

## Creating New Templates

Creating new templates in `yap` is straightforward. Templates under
the hood are message chains to be sent to `messages` field in llm
APIs. That said, you don't have to build it out from scratch.

There are three levels of templates in yap.

### Simple string template

This is just a string template. Examples would be:

``` emacs-lisp
(joke . "Tell me a joke")
(who-what . "What or who is {{Who/What}}? Provide a summary and 5 bullet points.")
```

These get automatically converted to the right format, and if
`{{<name>}}` blocks are present in the string, the user is asked for
an input with `<name>` as the input prompt.

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
(defun document-function ()
  "My custom template using PROMPT and BUFFER."
  (yap-template-prompt "Document the function"))

(add-to-list 'yap-templates '(document-function . document-function))
```

### Fully custom

If you think these don't work out for you, you can construct the full
set of messages yourself. It will look something like this:

```emacs-lisp
(defun my-custom-buffer-context-template ()
  "Custom template using PROMPT and BUFFER as context."
  (let* ((buffer (current-buffer))
         (prompt (read-string "Prompt: "))
         (selection (yap--get-selected-text buffer))
         (before (yap--get-text-before))
         (after (yap--get-text-after)))
    (if selection
        `((:role "system" :content "You are a helpful assistant. Provide concise and clear responses.")
          (:role "user" :content "I'll provide a document in which I have highlighted a section. Answer for the highlighted section but use the rest of the text as context.")
          (:role "assistant" :content "OK. What is the highlighted text?")
          (:role "user" :content ,selection)
          (:role "assistant" :content "What is there before the highlighted section?")
          (:role "user" :content ,before)
          (:role "assistant" :content "What is there after the highlighted section?")
          (:role "user" :content ,after)
          (:role "assistant" :content "What can I help you with?")
          (:role "user" :content ,prompt))
      `((:role "system" :content "You are a helpful assistant. Provide concise and clear responses.")
        (:role "user" :content "I'll provide you context about a document that I am working on. I'm somewhere within the document.")
        (:role "assistant" :content "OK. What comes before your current position?")
        (:role "user" :content ,before)
        (:role "assistant" :content "What comes after your current position?")
        (:role "user" :content ,after)
        (:role "assistant" :content "What can I help you with?")
        (:role "user" :content ,prompt)))))

(add-to-list 'yap-templates
             '(custom-buffer-context . my-custom-buffer-context-template))
```

---

✨ Happy Prompting! ✨
