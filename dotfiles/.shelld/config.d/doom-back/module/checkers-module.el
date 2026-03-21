;;; module/checkers-module.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This module centralizes all "checker"-related configurations in Emacs.
;; Checkers help ensure code and text quality by detecting errors, warnings,
;; and style issues.
;;
;; Safe loading order:
;;   1. General checker configuration (base setup, keybindings, common vars)
;;   2. Spell checking (relatively independent)
;;   3. Grammar checking (requires base checker setup)
;;   4. Syntax checking (dependent on Flycheck/Flymake availability)
;;
;; Each checker type is isolated in its own configuration file for clarity
;; and maintainability. You can easily enable or disable specific checkers
;; by commenting/uncommenting the corresponding `load!` line.

;;; Code:

;; ---------------------------------------------------------------------------
;; General checker configuration
;; Sets up base behaviors, keybindings, and common settings
;; for all checkers in Emacs.
;; ---------------------------------------------------------------------------
(load! "config/checkers-config/checkers-config.el")

;; ---------------------------------------------------------------------------
;; Spell checking
;; Configures spell checkers (aspell, hunspell, ispell)
;; and integrates with text and code buffers.
;; Loading this before grammar checking ensures dependencies are satisfied.
;; ---------------------------------------------------------------------------
(load! "config/checkers-config/checkers-spell-config.el")

;; ---------------------------------------------------------------------------
;; Grammar checking
;; Provides grammar correction and suggestions for text
;; via tools such as LanguageTool or other grammar backends.
;; ---------------------------------------------------------------------------
(load! "config/checkers-config/checkers-grammar-config.el")

;; ---------------------------------------------------------------------------
;; Syntax checking
;; Configures syntax and error checking for programming languages.
;; Includes Flycheck and Flymake integration for inline diagnostics.
;; ---------------------------------------------------------------------------
(load! "config/checkers-config/checkers-syntax-config.el")

(provide 'checkers-module)

;;; checkers-module.el ends here
