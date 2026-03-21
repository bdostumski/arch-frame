;;; module/config-module.el -*- lexical-binding: t; -*-
;;; Commentary:
;; This module loads the default configuration settings that apply
;; globally across your Emacs setup. It serves as the foundation layer
;; on top of which all other modules (editor, apps, checkers, completions, etc.)
;; are built.
;;
;; Safe load order:
;;   1. config-module (this module) – ensures global defaults are applied first
;;   2. editor-module – depends on global editing behaviors
;;   3. completions-module – may rely on editor settings
;;   4. checkers-module – may depend on completions and editor
;;   5. app-module – optional tools, runs last to avoid dependency issues
;;
;; Centralized global settings include:
;;   - Keybindings and shortcuts
;;   - Editor behaviors (line numbers, tabs)
;;   - Interface tweaks (theme, modeline, font)
;;   - Miscellaneous performance and usability adjustments

;;; Code:

;; ---------------------------------------------------------------------------
;; Default global configuration
;; Loads base settings that apply to Emacs globally.
;; Ensures a consistent environment for all other modules.
;; ---------------------------------------------------------------------------
(load! "config/default-config/config-default-config.el")

(provide 'config-module)

;;; config-module.el ends here
