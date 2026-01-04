;;; module/ui-module.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This module aggregates all UI-related configurations for Doom Emacs.
;; It includes themes, dashboards, modelines, popups, Treemacs, and
;; other visual enhancements.
;;
;; Safety/load order rationale:
;; 1. Lightweight, low-risk UI enhancements (emojis, word-wrap, unicode) first.
;; 2. Foundational appearance settings (themes, modeline, dashboard) second.
;; 3. Integrations that rely on other UI components (Treemacs, popup) loaded after.
;; 4. Optional or heavier visual enhancements (minimap, zen mode) loaded last.

;;; Code:

;; ---------------------------------------------------------------------------
;; 1. Basic UI enhancements
;; ---------------------------------------------------------------------------
(load! "config/ui-config/ui-emoji-config.el")              ;; Emoji rendering
(load! "config/ui-config/ui-unicode-config.el")            ;; Unicode & symbol improvements
(load! "config/ui-config/ui-word-wrap-config.el")          ;; Word wrap enhancements
(load! "config/ui-config/ui-vi-tilde-fringe-config.el")    ;; Vim-style tilde at line ends

;; ---------------------------------------------------------------------------
;; 2. Core appearance & dashboards
;; ---------------------------------------------------------------------------
(load! "config/ui-config/ui-doom-config.el")               ;; Doom theme & general tweaks
(load! "config/ui-config/ui-doom-modeline-config.el")      ;; Enhanced status bar
(load! "config/ui-config/ui-doom-dashboard-config.el")    ;; Startup dashboard
(load! "config/ui-config/ui-doom-quit-config.el")         ;; Quit confirmation / exit tweaks

;; ---------------------------------------------------------------------------
;; 3. File navigation & workspace helpers
;; ---------------------------------------------------------------------------
(load! "config/ui-config/ui-treemacs-config.el")          ;; File explorer
(load! "config/ui-config/ui-window-select-config.el")     ;; Visual buffer selection
(load! "config/ui-config/ui-workspaces-config.el")        ;; Multiple editing sessions

;; ---------------------------------------------------------------------------
;; 4. Popups and notifications
;; ---------------------------------------------------------------------------
(load! "config/ui-config/ui-popup-config.el")             ;; Temporary windows/messages

;; ---------------------------------------------------------------------------
;; 5. Highlighting & productivity helpers
;; ---------------------------------------------------------------------------
(load! "config/ui-config/ui-hl-todo-config.el")           ;; TODO/FIXME highlighting
(load! "config/ui-config/ui-deft-config.el")              ;; Quick note management

;; ---------------------------------------------------------------------------
;; 6. Optional or heavier visual enhancements
;; ---------------------------------------------------------------------------
(load! "config/ui-config/ui-minimap-config.el")           ;; Minimap sidebar
(load! "config/ui-config/ui-zen-config.el")               ;; Distraction-free Zen mode
(load! "config/ui-config/ui-vc-gutter-config.el")         ;; Git/VC gutter indicators

(provide 'ui-module)

;;; ui-module.el ends here
