;;; module/os-module.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This module contains OS-specific and system-integration configurations.
;; It ensures Emacs works smoothly across terminals, Unix-like systems,
;; and provides useful keybinding helpers.

;;; Code:

;; TTY support (colors, keybindings, etc. when Emacs runs in a terminal)
(load! "config/os-config/os-tty-config.el")

;; Unix integration (clipboard, shell utils, direnv, etc.)
(load! "config/os-config/os-unix-config.el")

;; Which-key configuration (displays available keybindings in real-time)
(load! "config/os-config/os-which-key-config.el")

(provide 'os-module)

;;; os-module.el ends here
