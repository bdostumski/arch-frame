;;; module/completions-module.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This module handles all completion-related configuration for Doom Emacs.
;; It loads separate configs for:
;;   - Company: in-buffer autocompletion
;;   - Ivy: minibuffer completion and narrowing
;;
;; Each config file is modularized under completion-config/.

;;; Code:

;; Company-mode configuration
;; Provides context-aware completion in buffers, integrated with LSP and snippets.
(load! "config/completion-config/completion-company-config.el")

;; Ivy configuration
;; Enhances minibuffer completion, narrowing, and search workflows.
(load! "config/completion-config/completion-ivy-config.el")

(provide 'completions-module)

;;; completions-module.el ends here
