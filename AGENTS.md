## Project Overview

`yap` is an Emacs package for interacting with LLMs via predefined templates. It supports OpenAI, Anthropic, Ollama, Groq, OpenRouter, and GitHub Models. Uses the `ahyatt/llm` library for provider abstraction and `plz` for HTTP requests.

## Testing

Load the package in Emacs for manual testing:
```sh
./_scripts/test-yap.sh
```
This starts Emacs with yap loaded from the current directory. There are no automated tests.

## Architecture

Four Emacs Lisp files, loaded in this dependency order:

1. **yap-utils.el** - Low-level utilities: response buffer management (`*yap-response*`), LLM provider construction per service, model listing API calls, CSV parsing, message format conversion for `llm` library
2. **yap-templates-core.el** - Template engine: loads prompt files from `prompts/` directory, builds message chains (plist-based `:role`/`:content` format), handles user input collection (`{{placeholder}}` interpolation), provides core template constructors (`yap-template-prompt`, `yap-template-rewrite`, `yap-template-buffer-context`, `yap-template-external-context`)
3. **yap-templates.el** - Built-in template definitions: wires up specific templates (summarize, explain-code, etc.) and defines `yap-templates` alist that maps template symbols to functions or strings
4. **yap.el** - Entry point: three main interactive commands (`yap-prompt`, `yap-rewrite`, `yap-write`), provider selection, streaming via `llm-chat-streaming`, rewrite accept/reject/diff UI

## Key Concepts

- **Templates** are entries in the `yap-templates` alist. A template value is either a function (returns a message chain) or a string (auto-converted via `yap-template-simple`). Strings with `{{Name}}` placeholders prompt the user for input.
- **Message chains** are lists of plists: `(:role system/user/assistant :content "...")`
- **Three interaction modes**: `yap-prompt` (display in response buffer), `yap-rewrite` (replace selection/buffer with response, with accept/reject flow), `yap-write` (insert at point)
- **Prompt files** in `prompts/` are markdown files loaded by `yap--get-prompt`
- Provider is determined by `yap-service` string, with `yap-llm-provider-override` for custom `llm` providers

## Conventions

- Private functions use `yap--` prefix, public API uses `yap-` prefix
- Template functions in yap-templates.el use `yap-template--` or `yap-templates--` prefix
- Service-specific functions use colon notation: `yap-api-key:openai`, `yap--get-models:openai`
