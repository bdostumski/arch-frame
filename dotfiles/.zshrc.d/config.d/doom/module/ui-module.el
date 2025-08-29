;;; module/ui-module.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This module aggregates all UI-related configurations for Doom Emacs.
;; Each sub-config provides enhancements for the visual experience,
;; dashboards, modelines, icons, popups, treemacs integration, and more.

;;; Code:

;; Deft - quick note management
(load! "config/ui-config/ui-deft-config.el")

;; Doom themes and UI tweaks
(load! "config/ui-config/ui-doom-config.el")

;; Doom dashboard - startup screen with projects/recent files
(load! "config/ui-config/ui-doom-dashboard-config.el")

;; Doom modeline - enhanced status bar
(load! "config/ui-config/ui-doom-modeline-config.el")

;; Quit confirmation & exit UI tweaks
(load! "config/ui-config/ui-doom-quit-config.el")

;; Emoji support
(load! "config/ui-config/ui-emoji-config.el")

;; Highlight TODO/FIXME/NOTE keywords in code
(load! "config/ui-config/ui-hl-todo-config.el")

;; Minimap sidebar
(load! "config/ui-config/ui-minimap-config.el")

;; Popup management (temporary windows, messages, etc.)
(load! "config/ui-config/ui-popup-config.el")

;; Treemacs file explorer integration
(load! "config/ui-config/ui-treemacs-config.el")

;; Unicode & symbols rendering improvements
(load! "config/ui-config/ui-unicode-config.el")

;; Version control gutter indicators
(load! "config/ui-config/ui-vc-gutter-config.el")

;; Vim-style ~ at line ends (tilde fringe)
(load! "config/ui-config/ui-vi-tilde-fringe-config.el")

;; Window selection helpers (switching buffers visually)
(load! "config/ui-config/ui-window-select-config.el")

;; Word wrapping enhancements
(load! "config/ui-config/ui-word-wrap-config.el")

;; Workspaces (multiple editing sessions, like tabs)
(load! "config/ui-config/ui-workspaces-config.el")

;; Zen mode (distraction-free editing)
(load! "config/ui-config/ui-zen-config.el")

(provide 'ui-module)

;;; ui-module.el ends here
