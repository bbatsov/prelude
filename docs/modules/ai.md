# Prelude AI

## Overview

[gptel](https://github.com/karthink/gptel) is a simple, multi-backend
LLM client for Emacs. This module installs it and provides a single
global keybinding so you can drive it from anywhere.

gptel supports many providers, including:

- **Anthropic** (Claude)
- **OpenAI** (GPT-4, GPT-5, etc.)
- **Google** (Gemini)
- **Ollama** (local models)
- **Azure OpenAI**, **OpenRouter**, **Perplexity**, and more

## Key Bindings

| Key | Command | Description |
| --- | ------- | ----------- |
| <kbd>C-c q</kbd> | `gptel-menu` | Transient menu (entry to gptel) |

Inside a gptel chat buffer, <kbd>C-c RET</kbd> sends the current
prompt, or the region when it's active (that's gptel's own default).

## Configuring a backend

You need to configure a backend and an API key in your personal
config (e.g. `~/.emacs.d/personal/ai.el`).  Some examples:

### Anthropic (Claude)

```emacs-lisp
(setq gptel-backend (gptel-make-anthropic "Claude"
                      :stream t
                      :key (getenv "ANTHROPIC_API_KEY"))
      gptel-model 'claude-sonnet-4-6)
```

### OpenAI

OpenAI is gptel's default backend, so you only need to set the key
and the model:

```emacs-lisp
(setq gptel-api-key (getenv "OPENAI_API_KEY")
      gptel-model 'gpt-4o)
```

### Local Ollama

```emacs-lisp
(setq gptel-backend (gptel-make-ollama "Ollama"
                      :host "localhost:11434"
                      :stream t
                      :models '(llama3.1:8b qwen2.5-coder:7b))
      gptel-model 'qwen2.5-coder:7b)
```

See the [gptel README](https://github.com/karthink/gptel) for
backend-specific options and the full list of supported providers.

## Tips

- `auth-source` is supported for API keys — pass
  `:key 'gptel-api-key-from-auth-source` or use a function — so you
  don't have to keep the key in environment variables.
- `gptel-add` (available from the transient menu) lets you attach
  buffers, regions, or files to the chat context.
- For agentic editing workflows, take a look at
  [aidermacs](https://github.com/MatthewZMD/aidermacs) — it's not
  bundled here but pairs naturally with this module.
