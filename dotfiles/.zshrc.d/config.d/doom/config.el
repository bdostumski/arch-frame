;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; Doom Emacs main configuration
;;; Commentary:
;; This file loads modular configuration files from the `module/` directory.
;; Each module handles a specific area of functionality:
;; UI, editing, LSP, tools, checkers, languages, apps, OS integration, etc.
;;
;; Load order is chosen for safety and dependency reasons:

;;; Code:

(require 'cl-lib)

;; Enable debugger mode
;;(setq debug-on-error t)
;; ---------------------------------------------------------------------------
;; 1. Editing Enhancements
;; ---------------------------------------------------------------------------
;; Provides modal editing (Evil), smartparens, folding, formatting, multiple cursors, snippets.
;; Fundamental for daily editing, loaded first.
(load! "module/editor-module.el")

;; ---------------------------------------------------------------------------
;; 2. Completion & Snippets
;; ---------------------------------------------------------------------------
;; Handles Company, Ivy, Vertico, and YASnippet for in-buffer and minibuffer completion.
;; Loaded after editor core to ensure proper keybindings and hooks.
(load! "module/completion-module.el")

;; ---------------------------------------------------------------------------
;; 3. UI & Visuals
;; ---------------------------------------------------------------------------
;; Themes, modeline, dashboard, icons, Treemacs, minimap, Zen mode, and visual enhancements.
;; Loaded early so visual defaults are applied to all subsequent modules.
(load! "module/ui-module.el")

;; ---------------------------------------------------------------------------
;; 4. Emacs Core & Utilities
;; ---------------------------------------------------------------------------
;; Dired/Dirvish, ibuffer, undo system, electric pairs, file templates, yasnippet support.
;; Ensures core utilities are available for other modules.
(load! "module/emacs-module.el")

;; ---------------------------------------------------------------------------
;; 5. Terminal Integration
;; ---------------------------------------------------------------------------
;; Eshell, vterm, shell configurations, and Tmux integration.
;; Loaded after core utilities and UI for proper terminal rendering and keybindings.
(load! "module/term-module.el")

;; ---------------------------------------------------------------------------
;; 6. Checkers & Linters
;; ---------------------------------------------------------------------------
;; Spell checking, grammar tools, Flycheck, Flymake.
;; Loaded after editing and completion modules to ensure hooks and keybindings are set.
(load! "module/checkers-module.el")

;; ---------------------------------------------------------------------------
;; 7. Tools & Productivity
;; ---------------------------------------------------------------------------
;; Magit, Docker, Gist, debuggers, LSP, PDF tools, Make, task runners, AI/LLM integrations.
;; Loaded after editors, UI, and checkers so all hooks and dependencies are available.
(load! "module/tools-module.el")

;; ---------------------------------------------------------------------------
;; 8. OS Integration
;; ---------------------------------------------------------------------------
;; OS-specific tweaks, Unix tools, TTY configuration, direnv, which-key.
;; Loaded after foundational modules to ensure compatibility.
(load! "module/os-module.el")

;; ---------------------------------------------------------------------------
;; 9. Language Support
;; ---------------------------------------------------------------------------
;; Programming and markup language modules.
;; Includes LSP, Tree-sitter, SQL, web stack, Ruby, Java, C/C++, etc.
;; Loaded after core tools and OS modules to ensure LSP servers, syntax parsers, and formatters work.
(load! "module/lang-module.el")

;; ---------------------------------------------------------------------------
;; 10. Email Clients
;; ---------------------------------------------------------------------------
;; mu4e configuration and email integrations.
;; Loaded after languages and tools so notifications and org integration are functional.
(load! "module/email-module.el")

;; ---------------------------------------------------------------------------
;; 11. Applications
;; ---------------------------------------------------------------------------
;; Calendar, RSS, IRC, EMMS, Org-roam, chat, and other user-facing apps.
;; Loaded near the end to rely on all editor, UI, and tools infrastructure.
(load! "module/app-module.el")

;; ---------------------------------------------------------------------------
;; 12. User Config & Secrets
;; ---------------------------------------------------------------------------
;; Personal keybi
(load! "module/keybindings.el")
