;;; module/os-module.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This module contains OS-specific and system integration configurations.
;; It ensures that Emacs operates smoothly across terminals, Unix-like systems,
;; and provides helpful keybinding displays.
;;
;; Load order rationale for safety:
;; 1. TTY / terminal support - ensures colors, keybindings, and terminal-specific
;;    behaviors are initialized first.
;; 2. Unix integration - loads shell utilities, clipboard, direnv, and other
;;    Unix-specific integrations that depend on TTY support.
;; 3. Which-key - displays available keybindings, depends on base keymaps being
;;    properly initialized.

;;; Code:

;; ---------------------------------------------------------------------------
;; 1. TTY / terminal support
;; ---------------------------------------------------------------------------
;; Configures color schemes, keybindings, and behaviors for Emacs running
;; inside a terminal (TTY). Ensures terminal compatibility for subsequent modules.
(load! "config/os-config/os-tty-config.el")

;; ---------------------------------------------------------------------------
;; 2. Unix system integration
;; ---------------------------------------------------------------------------
;; Provides clipboard integration, shell utilities, environment variable
;; management (e.g., direnv), and other Unix-specific enhancements.
(load! "config/os-config/os-unix-config.el")

;; ---------------------------------------------------------------------------
;; 3. Which-key configuration
;; ---------------------------------------------------------------------------
;; Real-time display of available keybindings in Emacs.
;; Relies on keymaps and terminal/system support being initialized first.
(load! "config/os-config/os-which-key-config.el")

(provide 'os-module)

;;; os-module.el ends here
