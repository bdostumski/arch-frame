;;; module/completions-module.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This module centralizes all completion-related configurations for Doom Emacs.
;; Completions help improve productivity by providing context-aware suggestions
;; for code, text, and minibuffer inputs.
;;
;; Safe loading order:
;;   1. Company-mode (buffer-level completions, integrates with LSP/snippets)
;;   2. Ivy (minibuffer completion, narrowing, and search)
;;
;; Each submodule can be individually customized or replaced without affecting
;; the rest of the completion system.

;;; Code:

;; ---------------------------------------------------------------------------
;; Company-mode configuration
;; Provides intelligent, context-aware completions directly in buffers.
;; Integrates with:
;;   - LSP servers for language-aware suggestions
;;   - Snippets (YASnippet or similar) for code templates
;;   - Manual or automatic completion triggers
;; ---------------------------------------------------------------------------
(load! "config/completion-config/completion-company-config.el")

;; ---------------------------------------------------------------------------
;; Ivy configuration
;; Enhances Emacs minibuffer completion and narrowing.
;; Provides features like:
;;   - Incremental search and filtering
;;   - Fuzzy matching for commands, buffers, files
;;   - Integration with Counsel and Swiper for extended functionality
;; ---------------------------------------------------------------------------
(load! "config/completion-config/completion-ivy-config.el")

(provide 'completions-module)

;;; completions-module.el ends here
