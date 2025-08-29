;;; config.el --- Doom Emacs Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Loads modular configuration files from the `configurations/` directory.

;;; Code:

;; ----------------------------------------
;; Core Editor Behavior
;; ----------------------------------------

(load! "configurations/emacs-config.el")
(load! "configurations/editor-config.el")

;; ----------------------------------------
;; UI and Completion
;; ----------------------------------------

(load! "configurations/ui-config.el")
(load! "configurations/completion-config.el")

;; ----------------------------------------
;; Language & LSP Support
;; ----------------------------------------

(load! "configurations/tools-config.el")
(load! "configurations/lang-config.el")

;; ----------------------------------------
;; Checkers (Spell, Syntax, etc.)
;; ----------------------------------------

(load! "configurations/checkers-config.el")

;; ----------------------------------------
;; Email Client
;; ----------------------------------------

(load! "configurations/email-config.el")

;;; config.el ends here
