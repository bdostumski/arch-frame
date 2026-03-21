;;; module/editor-module.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This module consolidates all editor-related configurations in one place.
;;
;; Recommended safe load order within editor-module:
;;   1. editor-evil-config          ; foundational modal editing
;;   2. editor-fold-config          ; folding relies on basic editing features
;;   3. editor-format-config        ; formatting may depend on editing modes
;;   4. editor-multiple-cursors-config ; depends on Evil and editing state
;;   5. editor-rainbow-delimiters-config ; cosmetic, safe to load last
;;
;; Each feature is split into its own file under `config/editor-config/`
;; for clarity, modularity, and maintainability.

;;; Code:

;; ---------------------------------------------------------------------------
;; 1. Evil-mode (Vim emulation)
;; Provides modal editing: Normal, Insert, Visual, etc.
;; Core foundation for other editor enhancements.
;; ---------------------------------------------------------------------------
(load! "config/editor-config/editor-evil-config.el")

;; ---------------------------------------------------------------------------
;; 2. Code folding
;; Collapse/expand code blocks, functions, and headings.
;; Relies on basic editing features provided by Evil-mode or Emacs.
;; ---------------------------------------------------------------------------
(load! "config/editor-config/editor-fold-config.el")

;; ---------------------------------------------------------------------------
;; 3. Automatic code formatting
;; Applies consistent code style via LSP, formatters, or linters.
;; Depends on editing modes for proper integration.
;; ---------------------------------------------------------------------------
(load! "config/editor-config/editor-format-config.el")

;; ---------------------------------------------------------------------------
;; 4. Multiple cursors
;; Enables simultaneous edits in multiple buffer locations.
;; Typically depends on Evil-mode keybindings and basic editing.
;; ---------------------------------------------------------------------------
(load! "config/editor-config/editor-multiple-cursors-config.el")

;; ---------------------------------------------------------------------------
;; 5. Rainbow delimiters
;; Adds visual aid for nested delimiters (parentheses, brackets, braces).
;; Cosmetic, safe to load last.
;; ---------------------------------------------------------------------------
(load! "config/editor-config/editor-rainbow-delimiters-config.el")

(provide 'editor-module)

;;; editor-module.el ends here
